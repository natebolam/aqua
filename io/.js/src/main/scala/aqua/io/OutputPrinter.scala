package aqua.io

import cats.effect.std.Console

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object AirBeautifier {

  @js.native
  @JSImport("@fluencelabs/air-beautify-wasm/air_beautify_wasm.js", "beautify")
  def beautify(air: String): String = js.native
}

// Uses to print outputs in CLI
// TODO: add F[_], cause it is effect
object OutputPrinter {

  def print(str: String): Unit = {
    println(AirBeautifier.beautify(str))
  }

  def errorF[F[_]: Console](str: String): F[Unit] = {
    Console[F].errorln(scala.Console.RED + str + scala.Console.RESET)
  }
}
