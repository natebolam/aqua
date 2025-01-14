package aqua.model.inline

import aqua.model.*
import aqua.model.inline.state.InliningState
import aqua.raw.ops.*
import aqua.raw.value.{ApplyPropertyRaw, FunctorRaw, IntoFieldRaw, IntoIndexRaw, LiteralRaw, VarRaw}
import aqua.types.*
import cats.syntax.show.*
import cats.data.{Chain, NonEmptyList, NonEmptyMap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrowInlinerSpec extends AnyFlatSpec with Matchers {

  "arrow inliner" should "convert simple arrow" in {

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          CallArrowRawTag.service(LiteralRaw.quote("dumb_srv_id"), "dumb", Call(Nil, Nil)).leaf,
          ArrowType(ProductType(Nil), ProductType(Nil)),
          Nil,
          Map.empty,
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      CallServiceModel(
        LiteralModel("\"dumb_srv_id\"", LiteralType.string),
        "dumb",
        CallModel(Nil, Nil)
      ).leaf
    ) should be(true)

  }

  /*
    func stream-callback(cb: []string -> ()):
      records: *string
      cb(records)
   */
  "arrow inliner" should "pass stream to callback properly" in {
    val streamType = StreamType(ScalarType.string)
    val streamVar = VarRaw("records", streamType)
    val streamModel = VarModel("records", StreamType(ScalarType.string))
    val canonName = streamVar.name + "_canon"
    val canonModel = VarModel(canonName, CanonStreamType(ScalarType.string))
    val cbType = ArrowType(ProductType(ArrayType(ScalarType.string) :: Nil), ProductType(Nil))
    val cbVal = VarModel("cb-pass", cbType)

    val cbArg = VarRaw("cbVar", ArrayType(ScalarType.string))

    val cbArrow = FuncArrow(
      "cb",
      CallArrowRawTag
        .service(
          LiteralRaw.quote("test-service"),
          "some-call",
          Call(cbArg :: Nil, Nil)
        )
        .leaf,
      ArrowType(
        ProductType.labelled(
          (
            cbArg.name,
            cbArg.`type`
          ) :: Nil
        ),
        ProductType(Nil)
      ),
      Nil,
      Map.empty,
      Map.empty,
      None
    )

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "stream-callback",
          RestrictionTag(streamVar.name, true).wrap(
            SeqTag.wrap(
              DeclareStreamTag(streamVar).leaf,
              CallArrowRawTag.func("cb", Call(streamVar :: Nil, Nil)).leaf
            )
          ),
          ArrowType(
            ProductType.labelled(
              (
                "cb",
                cbType
              ) :: Nil
            ),
            ProductType(Nil)
          ),
          Nil,
          Map("cb" -> cbArrow),
          Map.empty,
          None
        ),
        CallModel(cbVal :: Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      RestrictionModel(streamVar.name, true).wrap(
        SeqModel.wrap(
          CanonicalizeModel(streamModel, CallModel.Export(canonModel.name, canonModel.`type`)).leaf,
          CallServiceModel(
            LiteralModel("\"test-service\"", LiteralType.string),
            "some-call",
            CallModel(canonModel :: Nil, Nil)
          ).leaf
        )
      )
    ) should be(true)

  }

  /*
    func stream-callback(cb: string -> ()):
      records: *string
      cb(records!)
  */
  ignore /*"arrow inliner"*/ should "pass stream with gate to callback properly" in {
    val streamType = StreamType(ScalarType.string)
    val streamVar = VarRaw("records", streamType)
    val streamVarLambda =
      ApplyPropertyRaw(
        VarRaw("records", streamType),
        IntoIndexRaw(LiteralRaw.number(0), ScalarType.string)
      )
    val streamModel = VarModel(
      "records",
      StreamType(ScalarType.string),
      Chain.one(IntoIndexModel("0", ScalarType.string))
    )
    val cbType = ArrowType(ProductType(ScalarType.string :: Nil), ProductType(Nil))
    val cbVal = VarModel("cb-pass", cbType)

    val cbArg =
      VarRaw(
        "cbVar",
        ScalarType.string
      )

    val cbArrow = FuncArrow(
      "cb",
      CallArrowRawTag
        .service(
          LiteralRaw.quote("test-service"),
          "some-call",
          Call(cbArg :: Nil, Nil)
        )
        .leaf,
      ArrowType(
        ProductType.labelled(
          (
            cbArg.name,
            cbArg.`type`
          ) :: Nil
        ),
        ProductType(Nil)
      ),
      Nil,
      Map.empty,
      Map.empty,
      None
    )

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "stream-callback",
          RestrictionTag(streamVar.name, true).wrap(
            SeqTag.wrap(
              DeclareStreamTag(streamVar).leaf,
              CallArrowRawTag.func("cb", Call(streamVarLambda :: Nil, Nil)).leaf
            )
          ),
          ArrowType(
            ProductType.labelled(
              (
                "cb",
                cbType
              ) :: Nil
            ),
            ProductType(Nil)
          ),
          Nil,
          Map("cb" -> cbArrow),
          Map.empty,
          None
        ),
        CallModel(cbVal :: Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      RestrictionModel(streamVar.name, true).wrap(
        CallServiceModel(
          LiteralModel("\"test-service\"", LiteralType.string),
          "some-call",
          CallModel(streamModel :: Nil, Nil)
        ).leaf
      )
    ) should be(true)

  }

  /*
    service TestService("test-service"):
      get_records() -> []string

    func inner(inner-records: *[]string):
      inner-records <- TestService.get_records()

    func retrieve_records() -> [][]string:
        records: *[]string
        -- 'inner-records' argument in `inner` should be renamed as `records` in resulted AIR
        append_records(records)
        <- records
   */
  "arrow inliner" should "work with streams as arguments" in {

    val returnType = ArrayType(ArrayType(ScalarType.string))
    val streamType = StreamType(ArrayType(ScalarType.string))
    val canonType = CanonStreamType(ArrayType(ScalarType.string))
    val recordsVar = VarRaw("records", streamType)
    val recordsModel = VarModel(recordsVar.name, recordsVar.baseType)
    val canonModel = VarModel(recordsVar.name + "_canon", canonType)
    val innerRecordsVar = VarRaw("inner-records", StreamType(ArrayType(ScalarType.string)))
    val innerName = "inner"

    val inner = FuncArrow(
      innerName,
      CallArrowRawTag
        .service(
          LiteralRaw.quote("test-service"),
          "get_records",
          Call(Nil, Call.Export(innerRecordsVar.name, streamType) :: Nil)
        )
        .leaf,
      ArrowType(
        ProductType.labelled((innerRecordsVar.name -> streamType) :: Nil),
        ProductType(Nil)
      ),
      Nil,
      Map.empty,
      Map.empty,
      None
    )

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "outer",
          SeqTag.wrap(
            DeclareStreamTag(recordsVar).leaf,
            CallArrowRawTag.func(innerName, Call(recordsVar :: Nil, Nil)).leaf,
            CallArrowRawTag
              .service(
                LiteralRaw.quote("callbackSrv"),
                "response",
                Call(recordsVar :: Nil, Nil)
              )
              .leaf
          ),
          ArrowType(ProductType(Nil), ProductType(returnType :: Nil)),
          Nil,
          Map(innerName -> inner),
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      SeqModel.wrap(
        CallServiceModel(
          LiteralModel("\"test-service\"", LiteralType.string),
          "get_records",
          CallModel(Nil, CallModel.Export(recordsModel.name, recordsModel.`type`) :: Nil)
        ).leaf,
        SeqModel.wrap(
          CanonicalizeModel(recordsModel, CallModel.Export(canonModel.name, canonType)).leaf,
          CallServiceModel(
            LiteralModel("\"callbackSrv\"", LiteralType.string),
            "response",
            CallModel(canonModel :: Nil, Nil)
          ).leaf
        )
      )
    ) should be(true)

  }

  /*
   data Prod:
     value: string

   service OpHa("op"):
     array(a: string, b: string) -> []string
     identity(a: string) -> string

   func doSmth(arg: Prod):
     v = arg.value
     OpHa.identity(v)
   */
  "arrow inliner" should "hold lambda" in {

    // lambda that will be assigned to another variable
    val objectVarLambda =
      VarRaw("object", StructType("objectType", NonEmptyMap.one("field", ScalarType.string)))
        .withProperty(
          IntoFieldRaw("field", ScalarType.string)
        )

    val flattenObject = VarRaw("object_flat", ScalarType.string)

    // raw object
    val objectVar = VarRaw(
      "object",
      StructType("objectType", NonEmptyMap.one("field", ScalarType.string))
    )

    // export object
    val getSrvTag = CallArrowRawTag.service(
      LiteralRaw.quote("getSrv"),
      "getObj",
      Call(Nil, Call.Export(objectVar.name, objectVar.`type`) :: Nil)
    )

    // function where we assign object lambda to value and call service
    val inner =
      FuncArrow(
        "inner",
        SeqTag.wrap(
          AssignmentTag(
            objectVarLambda,
            "fieldValue"
          ).leaf,
          CallArrowRawTag
            .service(
              LiteralRaw.quote("callbackSrv"),
              "response",
              Call(VarRaw("fieldValue", ScalarType.string) :: Nil, Nil)
            )
            .leaf
        ),
        ArrowType(
          ProductType.labelled((objectVar.name, objectVar.`type`) :: Nil),
          ProductType(Nil)
        ),
        Nil,
        Map.empty,
        Map.empty,
        None
      )

    // wrapper that export object and call inner function
    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          SeqTag.wrap(
            getSrvTag.leaf,
            CallArrowRawTag.func(inner.funcName, Call(objectVar :: Nil, Nil)).leaf
          ),
          ArrowType(
            ProductType(Nil),
            ProductType(Nil)
          ),
          Nil,
          Map(inner.funcName -> inner),
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      SeqModel.wrap(
        CallServiceModel(
          LiteralModel("\"getSrv\"", LiteralType.string),
          "getObj",
          CallModel(Nil, CallModel.Export(objectVar.name, objectVar.`type`) :: Nil)
        ).leaf,
        SeqModel.wrap(
          FlattenModel(ValueModel.fromRaw(objectVarLambda), flattenObject.name).leaf,
          CallServiceModel(
            LiteralModel("\"callbackSrv\"", LiteralType.string),
            "response",
            CallModel(ValueModel.fromRaw(flattenObject) :: Nil, Nil)
          ).leaf
        )
      )
    ) should be(true)

  }

  /*
  func joinIdxLocal(idx: i16, nodes: []string):
    join nodes[idx]
   */
  "arrow inliner" should "not rename value in index array lambda" in {

    // lambda that will be assigned to another variable
    val argArray = VarRaw(
      "nodes",
      ArrayType(ScalarType.string)
    )

    val idxVar = VarRaw("idx", ScalarType.u32)

    val arrIdx = VarRaw("nodes", ArrayType(ScalarType.string)).withProperty(
      IntoIndexRaw(idxVar, ScalarType.string)
    )

    val getArrTag = CallArrowRawTag
      .service(
        LiteralRaw.quote("getSrv"),
        "getArr",
        Call(Nil, Call.Export(argArray.name, argArray.`type`) :: Nil)
      )
      .leaf

    val getIdxTag = CallArrowRawTag
      .service(
        LiteralRaw.quote("getSrv"),
        "getIdx",
        Call(Nil, Call.Export(idxVar.name, idxVar.`type`) :: Nil)
      )
      .leaf

    // function where we assign object lambda to value and call service
    val inner =
      FuncArrow(
        "inner",
        JoinTag(NonEmptyList.one(arrIdx)).leaf,
        ArrowType(
          ProductType.labelled(
            (idxVar.name, idxVar.`type`) :: (argArray.name, argArray.`type`) :: Nil
          ),
          ProductType(Nil)
        ),
        Nil,
        Map.empty,
        Map.empty,
        None
      )

    // wrapper that export object and call inner function
    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          SeqTag.wrap(
            getArrTag,
            getIdxTag,
            CallArrowRawTag.func(inner.funcName, Call(idxVar :: argArray :: Nil, Nil)).leaf
          ),
          ArrowType(
            ProductType(Nil),
            ProductType(Nil)
          ),
          Nil,
          Map(inner.funcName -> inner),
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      SeqModel.wrap(
        CallServiceModel(
          LiteralModel("\"getSrv\"", LiteralType.string),
          "getArr",
          CallModel(Nil, CallModel.Export(argArray.name, argArray.`type`) :: Nil)
        ).leaf,
        CallServiceModel(
          LiteralModel("\"getSrv\"", LiteralType.string),
          "getIdx",
          CallModel(Nil, CallModel.Export(idxVar.name, idxVar.`type`) :: Nil)
        ).leaf
      )
    ) should be(true)

  }

  "arrow inliner" should "rename value in arrow with same name as in for" in {
    val argVar = VarRaw("arg", ScalarType.u32)
    val iVar = VarRaw("i", ScalarType.string)
    val iVar0 = VarRaw("i-0", ScalarType.string)
    val innerVar = VarRaw("i", ScalarType.u32)
    val returnVar = VarRaw("ret", ScalarType.u32)

    val array = VarRaw(
      "nodes",
      ArrayType(ScalarType.string)
    )

    val inner =
      FuncArrow(
        "inner",
        ReturnTag(NonEmptyList.one(innerVar)).leaf,
        ArrowType(
          ProductType.labelled(
            (innerVar.name, innerVar.`type`) :: Nil
          ),
          ProductType(innerVar.`type` :: Nil)
        ),
        innerVar :: Nil,
        Map.empty,
        Map.empty,
        None
      )

    val serviceId = LiteralRaw.quote("test-service")
    val fnName = "some-call"

    val inFold = SeqTag.wrap(
      CallArrowRawTag
        .func(
          inner.funcName,
          Call(argVar :: Nil, Call.Export(returnVar.name, returnVar.`type`) :: Nil)
        )
        .leaf,
      CallArrowRawTag
        .service(
          serviceId,
          fnName,
          Call(returnVar :: Nil, Nil)
        )
        .leaf
    )

    val foldOp =
      ForTag(iVar.name, array, Some(ForTag.WaitMode)).wrap(inFold, NextTag(iVar.name).leaf)

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          SeqTag.wrap(
            AssignmentTag(LiteralRaw("1", LiteralType.number), argVar.name).leaf,
            foldOp
          ),
          ArrowType(
            ProductType(Nil),
            ProductType(Nil)
          ),
          Nil,
          Map(inner.funcName -> inner),
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      ForModel(iVar0.name, ValueModel.fromRaw(array), Some(ForModel.NeverMode)).wrap(
        CallServiceModel(
          LiteralModel.fromRaw(serviceId),
          fnName,
          CallModel(LiteralModel("1", LiteralType.number) :: Nil, Nil)
        ).leaf,
        NextModel(iVar0.name).leaf
      )
    ) should be(true)
  }

}
