package aqua.ast.expr

import aqua.ast.algebra.ValuesAlgebra
import aqua.ast.gen.Gen
import aqua.ast.{Expr, Prog}
import aqua.parser.lexer.Token._
import aqua.parser.lexer.Value
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.syntax.functor._

case class ReturnExpr[F[_]](value: Value[F]) extends Expr[F] {

  def program[Alg[_]](implicit V: ValuesAlgebra[F, Alg]): Prog[Alg, Gen] =
    V.resolveType(value).as(Gen.noop)

}

object ReturnExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    (`<-` *> ` ` *> Value.`value`[F]).map(ReturnExpr(_))
}