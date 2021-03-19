package aqua.generator

import aqua.semantics.{ArrowType, DataType}

sealed trait ArrowCallable {
  def toCallGen(args: List[DataView], result: Option[String]): AirGen
}

class FuncCallable(argNames: List[(String, Either[DataType, ArrowType])], retValue: Option[DataView], bodyGen: AirGen)
    extends ArrowCallable {

  override def toCallGen(args: List[DataView], result: Option[String]): AirGen =
    bodyGen.wrap { c =>
      val argsFull = argNames.zip(args)
      val argsToData = argsFull.collect { case ((n, Left(_)), v) =>
        n -> v
      }
      // TODO: here we need to collect ArrowCallable's
      val argsToArrows = argsFull.collect { case ((n, Right(_)), v) =>
        n -> v
      }

      (
        c.copy(data = c.data ++ argsToData),
        _.copy(data = c.data ++ result.zip(retValue))
      )
    }
}

class SrvCallable(srvId: DataView, fnName: String) extends ArrowCallable {

  override def toCallGen(args: List[DataView], result: Option[String]): AirGen =
    ServiceCallGen(srvId, fnName, args, result)
}

class SrvCallableOnPeer(peerId: DataView, srvId: DataView, fnName: String) extends ArrowCallable {

  override def toCallGen(args: List[DataView], result: Option[String]): AirGen =
    // TODO: hop via relay, if needed!
    ServiceCallGen(srvId, fnName, args, result).wrap(ctx => (ctx.copy(peerId = peerId), _.copy(peerId = ctx.peerId)))
}
