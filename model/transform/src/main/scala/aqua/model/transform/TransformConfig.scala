package aqua.model.transform

import aqua.model.{AquaContext, LiteralModel, ValueModel, VarModel}
import aqua.types.ScalarType
import cats.kernel.Monoid

case class TransformConfig(
  getDataService: String = "getDataSrv",
  callbackService: String = "callbackSrv",
  errorHandlingService: String = "errorHandlingSrv",
  errorFuncName: String = "error",
  respFuncName: String = "response",
  relayVarName: Option[String] = Some("-relay-"),
  wrapWithXor: Boolean = true,
  constants: List[TransformConfig.Const] = Nil
) {

  val errorId: ValueModel = LiteralModel.quote(errorFuncName)
  val errorHandlingCallback: ValueModel = LiteralModel.quote(errorHandlingService)
  val callbackSrvId: ValueModel = LiteralModel.quote(callbackService)
  val dataSrvId: ValueModel = LiteralModel.quote(getDataService)

  // Host peer id holds %init_peer_id% in case Aqua is not compiled to be executed behind a relay,
  // or relay's variable otherwise
  val hostPeerId: TransformConfig.Const =
    TransformConfig.Const(
      "host_peer_id",
      relayVarName.fold[ValueModel](LiteralModel.initPeerId)(r => VarModel(r, ScalarType.string))
    )

  implicit val aquaContextMonoid: Monoid[AquaContext] = {
    val constantsMap = (hostPeerId :: constants).map(c => c.name -> c.value).toMap
    AquaContext
      .implicits(
        AquaContext.blank
          .copy(values =
            Map(
              VarModel.lastError.name -> VarModel.lastError,
              "nil" -> LiteralModel.nil
            ) ++ constantsMap
          )
      )
      .aquaContextMonoid
  }

}

object TransformConfig {
  case class Const(name: String, value: ValueModel)

  def forHost: TransformConfig =
    TransformConfig(wrapWithXor = false, relayVarName = None)
}
