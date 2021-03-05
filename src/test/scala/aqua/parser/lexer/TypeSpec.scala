package aqua.parser.lexer

import aqua.interim.ScalarType
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id

import scala.language.implicitConversions

class TypeSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.interim.ScalarType.u32

  implicit def strToBt(st: ScalarType): BasicTypeToken[Id] = BasicTypeToken[Id](st)

  "Basic type" should "parse" in {
    BasicTypeToken.`basictypedef`.parseAll("u32").right.value should be(u32: BasicTypeToken[Id])
    BasicTypeToken.`basictypedef`.parseAll("()") should be('left)
  }

  "Arrow type" should "parse" in {

    TypeToken.`arrowdef`.parseAll("-> B").right.value should be(
      ArrowTypeToken[Id]((), Nil, Some(CustomTypeToken[Id]("B")))
    )
    TypeToken.`arrowdef`.parseAll("A -> B").right.value should be(
      ArrowTypeToken[Id]((), CustomTypeToken[Id]("A") :: Nil, Some(CustomTypeToken[Id]("B")))
    )
    TypeToken.`arrowdef`.parseAll("u32 -> Boo").right.value should be(
      ArrowTypeToken[Id]((), (u32: BasicTypeToken[Id]) :: Nil, Some(CustomTypeToken[Id]("Boo")))
    )
    TypeToken.`typedef`.parseAll("u32 -> ()").right.value should be(
      ArrowTypeToken[Id]((), (u32: BasicTypeToken[Id]) :: Nil, None)
    )
    TypeToken.`arrowdef`.parseAll("A, u32 -> B").right.value should be(
      ArrowTypeToken[Id](
        (),
        CustomTypeToken[Id]("A") :: (u32: BasicTypeToken[Id]) :: Nil,
        Some(CustomTypeToken[Id]("B"))
      )
    )
    TypeToken.`arrowdef`.parseAll("[]Absolutely, u32 -> B").right.value should be(
      ArrowTypeToken[Id](
        (),
        ArrayTypeToken(CustomTypeToken[Id]("Absolutely")) :: (u32: BasicTypeToken[Id]) :: Nil,
        Some(CustomTypeToken[Id]("B"))
      )
    )

  }

  "Array type" should "parse" in {
    TypeToken.`typedef`.parseAll("[]Something") should be(Right(ArrayTypeToken(CustomTypeToken[Id]("Something"))))
    TypeToken.`typedef`.parseAll("[]u32") should be(Right(ArrayTypeToken(u32: BasicTypeToken[Id])))
    TypeToken.`typedef`.parseAll("[][]u32") should be(
      Right(ArrayTypeToken[Id](ArrayTypeToken[Id](u32: BasicTypeToken[Id])))
    )
  }

}
