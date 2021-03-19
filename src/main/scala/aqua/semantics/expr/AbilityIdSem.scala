package aqua.semantics.expr

import aqua.generator.Gen
import aqua.parser.expr.AbilityIdExpr
import aqua.semantics.Prog
import aqua.semantics.algebra.ValuesAlgebra
import aqua.semantics.algebra.abilities.AbilitiesAlgebra

import cats.syntax.flatMap._
import cats.syntax.functor._

class AbilityIdSem[F[_]](val expr: AbilityIdExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit A: AbilitiesAlgebra[F, Alg], V: ValuesAlgebra[F, Alg]): Prog[Alg, Gen] =
    V.ensureIsString(expr.id) >> A.setServiceId(expr.ability, expr.id) as Gen.noop
}
