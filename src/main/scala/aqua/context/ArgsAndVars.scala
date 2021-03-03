package aqua.context

import aqua.context.marker.{DataMarker, ExtractedVarMarker, FuncArgMarker}
import aqua.context.scope.Scope
import aqua.context.walker.{Acc, ExpectAndDefine, Walker}
import aqua.parser.{AbilityFuncCall, AbilityId, Block, DefFunc, Extract, FuncCall, FuncExpr, On, Par}
import aqua.parser.lexer.{DataType, Value, Var}
import cats.Comonad
import shapeless._
import shapeless.ops.hlist.Selector
import cats.syntax.comonad._

case class ArgsAndVars[F[_]](expDef: ExpectAndDefine[F, Value[F], DataMarker[F, HNil]]) {
  def clearDefinitions: ArgsAndVars[F] = copy(expDef.clearDefinitions)
}

object ArgsAndVars {
  type Acc[F[_]] = ExpectAndDefine[F, Value[F], DataMarker[F, HNil]]
  def emptyAcc[F[_]]: Acc[F] = ExpectAndDefine.empty[F, Value[F], DataMarker[F, HNil]]
  def empty[F[_]]: ArgsAndVars[F] = ArgsAndVars[F](emptyAcc[F])

  class ExpDef[F[_]: Comonad, I <: HList, O <: HList](extend: Walker[F, I, O])(implicit getScope: Selector[O, Scope[F]])
      extends Walker[F, I, ArgsAndVars[F] :: O] {
    type Ctx = ArgsAndVars[F] :: O

    override def exitFuncOpGroup(group: FuncExpr[F, I], last: Ctx): Ctx =
      last.head :: extend.exitFuncOpGroup(group, last.tail)

    override def funcOpCtx(op: FuncExpr[F, I], prev: Ctx): Ctx = {
      lazy val in = extend.funcOpCtx(op, prev.tail)
      val data = prev.head
      lazy val mode = getScope(in).mode.map(_.extract)
      def combinedWith(other: Acc[F] => ExpectAndDefine[F, Value[F], DataMarker[F, HNil]]): ArgsAndVars[F] =
        ArgsAndVars[F](data.expDef.combine(other(emptyAcc[F]), mode))

      op match {
        case FuncCall(_, args, _) =>
          combinedWith(_ expect Acc.fromValues(args)) :: in
        case AbilityFuncCall(_, _, args, _) =>
          combinedWith(_ expect Acc.fromValues(args)) :: in
        case ex @ Extract(n, fc, _) =>
          val f = funcOpCtx(fc, prev)
          f.head
            .copy(
              f.head.expDef.combine(empty[F].expDef defined Acc.one(n.name.extract, ExtractedVarMarker(n, ex)), mode)
            ) :: f.tail
        case AbilityId(_, id, _) =>
          combinedWith(_ expect Acc.fromValues(id :: Nil)) :: in
        case On(p, _, _) =>
          combinedWith(_ expect Acc.fromValues(p :: Nil)) :: in
        case Par(_, _, _) =>
          data :: in
      }
    }

    override def blockCtx(block: Block[F, I]): Ctx =
      (block match {
        case DefFunc(head, _, _) =>
          head.args
            .foldLeft(empty[F]) {
              case (acc, (k, v, dt: DataType[F])) =>
                acc.copy(acc.expDef.defined(Acc.one(k, FuncArgMarker(Var(v), dt))))
              case (acc, _) => acc
            }

        case _ =>
          empty[F]

      }) :: extend.blockCtx(block)

    override def duplicates(prev: Out, next: Out): List[F[String]] =
      next.head.expDef.defineAcc
        .takeKeys(prev.head.expDef.defineAcc.keys)
        .data
        .flatMap {
          case (k, vs) => vs.toList.map(_.toError(s"Duplicated variable definition `$k`"))
        }
        .toList ::: extend.duplicates(prev.tail, next.tail)

    override def emptyCtx: Out = empty[F] :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      ArgsAndVars(prev.head.expDef.clearDefinitions combineSeq block.head.expDef) :: extend
        .combineBlockCtx(prev.tail, block.tail)

    override def unresolved(ctx: Out): List[F[String]] =
      ctx.head.expDef.expectAcc.data.flatMap {
        case (k, vs) => vs.toList.map(v => v.as(s"Unresolved variable `$k`"))
      }.toList ::: extend.unresolved(ctx.tail)
  }
}
