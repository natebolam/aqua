package aqua.model

import aqua.generator.{ArrowCallable, TypescriptFile, TypescriptFunc}

import scala.collection.immutable.Queue
import cats.syntax.show._

case class ScriptModel(funcs: Queue[FuncModel]) extends Model {

  def enqueue(m: Model): ScriptModel = m match {
    case f: FuncModel => copy(funcs.enqueue(f))
    case _ => this
  }

  def generateAir: Queue[String] =
    funcs
      .foldLeft((Map.empty[String, ArrowCallable], Queue.empty[String])) { case ((funcsAcc, outputAcc), func) =>
        funcsAcc.updated(func.name, func.callable) -> outputAcc.enqueue(func.generateAir(funcsAcc).show)
      }
      ._2

  def generateTypescript: String =
    TypescriptFile(
      funcs
        .foldLeft((Map.empty[String, ArrowCallable], Queue.empty[TypescriptFunc])) {
          case ((funcsAcc, outputAcc), func) =>
            funcsAcc.updated(func.name, func.callable) -> outputAcc.enqueue(func.generateTs(funcsAcc))
        }
        ._2
    ).show

}
