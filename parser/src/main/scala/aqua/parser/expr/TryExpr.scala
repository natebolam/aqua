package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.{Parser => P}

case class TryExpr[F[_]](point: Token[F]) extends Expr[F](TryExpr, point)

object TryExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] =
    IfExpr.validChildren

  override def p[F[_]: LiftParser: Comonad]: P[TryExpr[F]] =
    `try`.lift.map(Token.lift[F, Unit](_)).map(TryExpr(_))
}
