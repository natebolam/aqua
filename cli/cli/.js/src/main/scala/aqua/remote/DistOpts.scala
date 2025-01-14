package aqua.remote

import aqua.ArgOpts.jsonFromFileOpt
import aqua.builder.ArgumentGetter
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.run.{GeneralOptions, GeneralOpts, CliFunc}
import aqua.types.{ArrayType, ScalarType, StructType}
import aqua.*
import aqua.io.PackagePath
import aqua.js.{JsonEncoder, VarJson}
import cats.data.{NonEmptyList, NonEmptyMap, ValidatedNec}
import cats.data.Validated.{invalidNec, validNec}
import cats.effect.{Async, Concurrent, ExitCode, Resource, Sync}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.show.*
import cats.{Applicative, Monad}
import com.monovore.decline.Opts
import fs2.io.file.Files
import scribe.Logging

import scala.collection.immutable.SortedMap
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js

// Options and commands to work blueprints, modules and services
object DistOpts extends Logging {

  val DistAqua = "aqua/dist.aqua"

  val DeployFuncName = "deploy"
  val RemoveFuncName = "remove"
  val CreateServiceFuncName = "createService"
  val AddBlueprintFuncName = "addBlueprint"

  def srvNameOpt: Opts[String] =
    Opts
      .option[String]("service", "Service to deploy from the config file")

  def srvIdOpt: Opts[String] =
    Opts
      .option[String]("id", "Service id to remove", "i")

  def blueprintIdOpt: Opts[String] =
    Opts
      .option[String]("id", "Blueprint id", "i")

  def blueprintNameOpt: Opts[String] =
    Opts
      .option[String]("name", "Blueprint name", "n")

  def dependencyOpt: Opts[NonEmptyList[String]] =
    Opts
      .options[String]("dependency", "Blueprint dependency. May be used several times", "d")

  // Removes service from a node
  def remove[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "remove_service",
      "Remove service",
      (GeneralOpts.opt, srvIdOpt).mapN { (common, srvId) =>
        RunInfo(
          common,
          CliFunc(RemoveFuncName, LiteralRaw.quote(srvId) :: Nil),
          Option(PackagePath(DistAqua))
        )
      }
    )

  def createService[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "create_service",
      "Deploy service from existing blueprint",
      (GeneralOpts.opt, blueprintIdOpt).mapN { (common, blueprintId) =>
        RunInfo(
          common,
          CliFunc(CreateServiceFuncName, LiteralRaw.quote(blueprintId) :: Nil),
          Option(PackagePath(DistAqua))
        )
      }
    )

  def addBlueprint[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "add_blueprint",
      "Add blueprint to a peer",
      (GeneralOpts.opt, blueprintNameOpt, dependencyOpt).mapN {
        (common, blueprintName, dependencies) =>
          val depsWithHash = dependencies.map { d =>
            if (d.startsWith("hash:"))
              d
            else
              "hash:" + d
          }
          val addBlueprintType = StructType(
            "AddBlueprint",
            NonEmptyMap.of(
              ("name", ScalarType.string),
              ("dependencies", ArrayType(ScalarType.string))
            )
          )
          val addBlueprintRequestVar =
            VarRaw("addBlueprint", addBlueprintType)
          RunInfo(
            common,
            CliFunc(AddBlueprintFuncName, addBlueprintRequestVar :: Nil),
            Option(PackagePath(DistAqua)),
            Nil,
            Map(
              addBlueprintRequestVar.name -> VarJson(
                addBlueprintRequestVar,
                js.Dynamic
                  .literal("name" -> blueprintName, "dependencies" -> depsWithHash.toList.toJSArray)
              )
            )
          )
      }
    )

  def configFromFileOpt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, js.Dynamic]]] = {
    jsonFromFileOpt("config-path", "Path to a deploy config", "p")
  }

  // Uploads a file to IPFS, creates blueprints and deploys a service
  def deploy[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.applyF(
      "deploy_service",
      "Deploy service from WASM modules",
      (
        GeneralOpts.optWithSecretKeyCustomTimeout(60000),
        configFromFileOpt[F],
        srvNameOpt
      ).mapN { (common, configFromFileF, srvName) =>
        configFromFileF.map { dff =>
          dff
            .andThen(config =>
              val srvConfig = {
                val c = config.selectDynamic(srvName)
                if (js.isUndefined(c)) None
                else Some(c)
              }
              srvConfig match {
                case Some(c) =>
                  JsonEncoder.aquaTypeFromJson(srvName, c).andThen { configType =>
                    val srvArg = VarRaw(srvName, configType)
                    val args = LiteralRaw.quote(srvName) :: srvArg :: Nil
                    // if we have default timeout, increase it
                    validNec(
                      RunInfo(
                        common,
                        CliFunc(DeployFuncName, args),
                        Option(PackagePath(DistAqua)),
                        Nil,
                        // hack: air cannot use undefined fields, fill undefined arrays with nils
                        Map(srvName -> VarJson(srvArg, c))
                      )
                    )
                  }

                case None =>
                  invalidNec(s"No service '$srvName' in the config.")

              }
            )
        }
      }
    )
}
