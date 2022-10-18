package aqua.model.inline.raw

import aqua.model.{
  CallModel,
  CallServiceModel,
  CanonicalizeModel,
  FlattenModel,
  ForModel,
  FunctorModel,
  IntoFieldModel,
  IntoIndexModel,
  LiteralModel,
  MatchMismatchModel,
  NextModel,
  PropertyModel,
  PushToStreamModel,
  RestrictionModel,
  SeqModel,
  ValueModel,
  VarModel,
  XorModel
}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{
  ApplyFunctorRaw,
  ApplyPropertyRaw,
  CallArrowRaw,
  FunctorRaw,
  IntoFieldRaw,
  IntoIndexRaw,
  LiteralRaw,
  PropertyRaw,
  ValueRaw,
  VarRaw
}
import aqua.types.{ArrayType, CanonStreamType, ScalarType, StreamType}
import cats.data.{Chain, State}
import cats.syntax.monoid.*
import cats.instances.list.*

object ApplyPropertiesRawInliner extends RawInliner[ApplyPropertyRaw] {

  private[inline] def removeProperty[S: Mangler: Exports: Arrows](
    vm: ValueModel
  ): State[S, (ValueModel, Inline)] =
    vm match {
      case VarModel(nameM, btm, propertyM) if propertyM.nonEmpty =>
        for {
          nameMM <- Mangler[S].findAndForbidName(nameM)
        } yield VarModel(nameMM, vm.`type`, Chain.empty) -> Inline.preload(
          // TODO use smth more resilient to make VarRaw from a flattened VarModel
          nameMM -> ApplyPropertyRaw.fromChain(VarRaw(nameM, btm), propertyM.map(_.toRaw))
        )
      case _ =>
        State.pure(vm -> Inline.empty)
    }

  private[inline] def unfoldProperty[S: Mangler: Exports: Arrows](
    p: PropertyRaw
  ): State[S, (PropertyModel, Inline)] = // TODO property for collection
    p match {
      case IntoFieldRaw(field, t) =>
        State.pure(IntoFieldModel(field, t) -> Inline.empty)
      case f@IntoIndexRaw(vm: ApplyPropertyRaw, t) =>
        println("unfold: " + f)
        for {
          nn <- Mangler[S].findAndForbidName("ap-prop")
        } yield IntoIndexModel(nn, t) -> Inline.preload(nn -> vm)

      case IntoIndexRaw(vr: (VarRaw | CallArrowRaw), t) =>
        unfold(vr, propertiesAllowed = false).map {
          case (VarModel(name, _, _), inline) => IntoIndexModel(name, t) -> inline
          case (LiteralModel(v, _), inline) => IntoIndexModel(v, t) -> inline
        }

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Inline.empty)
    }

  private def increment(v: ValueModel, result: VarModel) =
    CallServiceModel(
      LiteralModel("\"math\"", ScalarType.string),
      "add",
      CallModel(
        v :: LiteralModel.fromRaw(LiteralRaw.number(1)) :: Nil,
        CallModel.Export(result.name, result.`type`) :: Nil
      )
    ).leaf

  def unfoldRawWithPropertyModels[S: Mangler: Exports: Arrows](
    raw: ValueRaw,
    propertyModels: Chain[PropertyModel],
    propertyPrefix: Inline,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    println("property models: " + propertyModels)
    println("raw: " + raw)
    Exports[S].exports.flatMap { exports =>
      unfold(raw, propertiesAllowed).flatMap {
        case (v: VarModel, prefix) =>
          ((v.`type`, propertyModels.headOption) match {
            // canonicalize stream
            case (st: StreamType, Some(idx @ IntoIndexModel(_, _))) =>
              for {
                uniqueResultName <- Mangler[S].findAndForbidName(v.name + "_result_canon")
                uniqueTestName <- Mangler[S].findAndForbidName(v.name + "_test")
                _ = println("idx: " + idx)
              } yield {
                val varSTest = VarModel(uniqueTestName, st)
                val iter = VarModel("s", st.element)

                val iterCanon = VarModel(v.name + "_iter_canon", CanonStreamType(st.element))

                val resultCanon =
                  VarModel(uniqueResultName, CanonStreamType(st.element), propertyModels)

                val incrVar = VarModel("incr_idx", ScalarType.u32)

                val tree = RestrictionModel(varSTest.name, true).wrap(
                  ForModel(iter.name, v, Some(ForModel.NeverMode)).wrap(
                    increment(idx.idxToValueModel, incrVar),
                    PushToStreamModel(
                      iter,
                      CallModel.Export(varSTest.name, varSTest.`type`)
                    ).leaf,
                    CanonicalizeModel(
                      varSTest,
                      CallModel.Export(iterCanon.name, iterCanon.`type`)
                    ).leaf,
                    XorModel.wrap(
                      MatchMismatchModel(
                        iterCanon
                          .copy(properties = Chain.one(FunctorModel("length", ScalarType.`u32`))),
                        incrVar,
                        true
                      ).leaf,
                      NextModel(iter.name).leaf
                    )
                  ),
                  CanonicalizeModel(
                    varSTest,
                    CallModel.Export(resultCanon.name, CanonStreamType(st.element))
                  ).leaf
                )

                (resultCanon, Inline.tree(tree), true)
              }

            case _ =>
              val vm = v.copy(properties = v.properties ++ propertyModels).resolveWith(exports)
              State.pure((vm, Inline.empty, false))
          }).flatMap { case (genV, genInline, isSeq) =>
            println("genV 1: " + genV)
            println("gen inline 1: " + genInline)
            val prefInline =
              if (isSeq)
                Inline(
                  propertyPrefix.flattenValues ++ genInline.flattenValues,
                  Chain.one(SeqModel.wrap((propertyPrefix.predo ++ genInline.predo).toList: _*))
                )
              else propertyPrefix |+| genInline
            println("pref inline: " + prefInline)
            println("prefix: " + prefix)
            println("isSeq: " + isSeq)
            println("prop pred: " + propertyPrefix)
            if (propertiesAllowed) State.pure(genV -> (prefix |+| prefInline))
            else
              removeProperty(genV).map { case (vmm, mpp) =>
                println("mpp: " + mpp)
                vmm -> (prefix |+| mpp |+| prefInline)
              }
          }

        case (v, prefix) =>
          println("strange v: " + v)
          println("strange prefix: " + prefix)
          println("strange prop prefix: " + propertyPrefix)
          // What does it mean actually? I've no ides
          State.pure((v, prefix |+| propertyPrefix))
      }
    }
  }

  private def unfoldProperties[S: Mangler: Exports: Arrows](
    raw: ValueRaw,
    properties: Chain[PropertyRaw],
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    println("raw: " + raw)
    println("properties: " + properties)
    properties
      .foldLeft[State[S, (Chain[PropertyModel], Inline, ValueRaw)]](
        State.pure((Chain.empty[PropertyModel], Inline.empty, raw))
      ) { case (pcm, p) =>
        pcm.flatMap { case (pc, m, r) =>
          unfoldProperty(p).map { case (pm, mm) =>
            println("pm: " + pm)
            println("m: " + m)
            println("mm: " + mm)
            (pc :+ pm, m |+| mm, r)
          }
        }
      }
      .flatMap { case (propertyModels, map, r) =>
        unfoldRawWithPropertyModels(r, propertyModels, map, propertiesAllowed)
      }
  }

  override def apply[S: Mangler: Exports: Arrows](
    apr: ApplyPropertyRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    val (raw, properties) = apr.unwind

    val leftToFunctor = properties.takeWhile {
      case FunctorRaw(_, _) => false
      case _ => true
    }

    if (leftToFunctor.length == properties.length) {
      unfoldProperties(raw, properties, propertiesAllowed)
    } else {
      // split properties like this:
      // properties -- functor -- properties with functors
      // process properties, process functor in ApplyFunctorRawInliner
      // then process tail recursively
      (for {
        ur <- properties.dropWhile {
          case FunctorRaw(_, _) => false
          case _ => true
        }.uncons
        (functor: FunctorRaw, right) = ur
      } yield {
        (leftToFunctor, functor, right)
      }).map { case (left, functor, right) =>
        for {
          vmLeftInline <- unfold(ApplyPropertyRaw.fromChain(raw, left), propertiesAllowed)
          (leftVM, leftInline) = vmLeftInline
          fRaw = ApplyFunctorRaw(leftVM.toRaw, functor)
          vmFunctorInline <- ApplyFunctorRawInliner(fRaw, false)
          (fVM, fInline) = vmFunctorInline
          vmRightInline <- unfold(ApplyPropertyRaw.fromChain(fVM.toRaw, right), propertiesAllowed)
          (vm, rightInline) = vmRightInline
        } yield {
          println("left inline: " + leftInline)
          println("f inline: " + fInline)
          println("right inline: " + rightInline)
          vm -> (leftInline |+| fInline |+| rightInline)
          /*
          vm -> Inline(
            leftInline.flattenValues ++ fInline.flattenValues ++ rightInline.flattenValues,
            Chain.one(SeqModel.wrap((leftInline.predo ++ fInline.predo ++ rightInline.predo).toList: _*))
          )
          */
        }
      }.getOrElse(unfoldProperties(raw, properties, propertiesAllowed))
    }

}
