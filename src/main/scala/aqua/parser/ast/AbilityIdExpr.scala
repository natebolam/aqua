package aqua.parser.ast

import aqua.interim.abilities.AbilitiesAlgebra
import aqua.parser.lexer.{Ability, Value}
import aqua.parser.lift.LiftParser
import cats.parse.{Parser => P}
import aqua.parser.lexer.Token._
import cats.Comonad

case class AbilityIdExpr[F[_]](ability: Ability[F], id: Value[F]) extends Expr[F] {

  def program[Alg[_]](implicit A: AbilitiesAlgebra[Alg]): Prog[Alg, Unit] =
    A.setServiceId(ability, id)

}

object AbilityIdExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[AbilityIdExpr[F]] =
    ((Ability.ab[F] <* ` `) ~ Value.`value`).map {
      case (ability, id) => AbilityIdExpr(ability, id)
    }

}
