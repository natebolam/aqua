package aqua

import cats.effect.kernel.Async
import fs2.io.file.{Files, Path}
import aqua.js.{Meta, Module}
import scribe.Logging
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*

import scala.util.Try

object PlatformPackagePath extends Logging {

  // it could be global installed aqua and local installed, different paths for this
  def getPackagePath[F[_]: Async](path: String): F[Path] = {
    val meta = Meta.metaUrl
    val req = Module.createRequire(meta)
    Try {
      // this can throw an error, global or local project path
      val builtinPath = Path(req.resolve("@fluencelabs/aqua-lib/builtin.aqua").toString)
      val rootProjectPath = builtinPath.resolve("../../../..")
      // hack, check if it is a local dependency or global
      val filePath = rootProjectPath.resolve(path)
      Files[F].exists(filePath).map {
        // if not exists, it should be local dependency, check in node_modules
        case false => rootProjectPath.resolve("node_modules/@fluencelabs/aqua").resolve(path)
        case true => filePath

      }
    }.getOrElse {
      // we don't care about path if there is no builtins, but must write an error
      logger.error("Unexpected. Cannot find project path")
      Path(path).pure[F]
    }
  }

  // get path to node modules if there is `aqua-lib` module with `builtin.aqua` in it
  def getGlobalNodeModulePath: List[Path] = {
    val meta = Meta.metaUrl
    val req = Module.createRequire(meta)
    Try {
      // this can throw an error
      val pathStr = req.resolve("@fluencelabs/aqua-lib/builtin.aqua").toString
      // hack
      val globalAquaPath = Path(pathStr).parent.flatMap(_.parent.flatMap(_.parent))

      // Also hack. If we found installed `aqua-lib`, it should be in `node_modules` global path.
      // In global `node_modules` could be installed aqua libs and we must use them,
      // if they were imported in aqua files
      val globalNodeModulesPath =
        globalAquaPath.flatMap(_.parent.flatMap(_.parent.flatMap(_.parent)))

      globalAquaPath.toList ++ globalNodeModulesPath.toList
    }.getOrElse {
      // we don't care about path if there is no builtins, but must write an error
      logger.error("Unexpected. Cannot find 'aqua-lib' dependency with `builtin.aqua` in it")
      Nil
    }

  }
}
