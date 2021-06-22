package aqua.model.func.raw

import aqua.model.ValueModel
import cats.data.Chain
import cats.data.Chain.{:==, ==:, nil}
import wvlet.log.LogSupport

import scala.annotation.tailrec

object PathFinder extends LogSupport {

  def find(from: RawCursor, to: RawCursor, isExit: Boolean = false): Chain[ValueModel] = {

    val fromOn = Chain.fromSeq(from.pathOn).reverse
    val toOn = Chain.fromSeq(to.pathOn).reverse

    val wasHandled =
      !isExit &&
        to.leftSiblings.isEmpty &&
        to.moveUp.exists(_.pathOn == to.pathOn) &&
        !to.parentTag.contains(ParTag)

    if (wasHandled) {
      debug("Was handled")
      debug(" :: " + from)
      debug(" -> " + to)
      Chain.empty
    } else {
      debug("Find path")
      debug(" :: " + from)
      debug(" -> " + to)
      findPath(
        fromOn,
        toOn,
        Chain.fromOption(from.currentPeerId),
        Chain.fromOption(to.currentPeerId)
      )
    }
  }

  def optimizePath(
    peerIds: Chain[ValueModel],
    prefix: Chain[ValueModel],
    suffix: Chain[ValueModel]
  ): Chain[ValueModel] = {
    val optimized = peerIds
      .foldLeft(Chain.empty[ValueModel]) {
        case (acc, p) if acc.lastOption.contains(p) => acc
        case (acc, p) if acc.contains(p) => acc.takeWhile(_ != p) :+ p
        case (acc, p) => acc :+ p
      }
    val noPrefix = skipPrefix(optimized, prefix, optimized)
    skipSuffix(noPrefix, suffix, noPrefix)
  }

  def findPath(
    fromOn: Chain[OnTag],
    toOn: Chain[OnTag],
    fromPeer: Chain[ValueModel],
    toPeer: Chain[ValueModel]
  ): Chain[ValueModel] = {
    val (from, to) = skipCommonPrefix(fromOn, toOn)
    val fromFix =
      if (from.isEmpty && fromPeer != toPeer) Chain.fromOption(fromOn.lastOption) else from
    val toFix = if (to.isEmpty && fromPeer != toPeer) Chain.fromOption(toOn.lastOption) else to
    val fromTo = fromFix.reverse.flatMap(_.via.reverse) ++ toFix.flatMap(_.via)
    val optimized = optimizePath(fromPeer ++ fromTo ++ toPeer, fromPeer, toPeer)

    trace("FIND PATH " + fromFix)
    trace("       -> " + toFix)
    trace(s"$fromPeer $toPeer")
    trace("                     Optimized: " + optimized)
    optimized
  }

  @tailrec
  def skipPrefix[T](chain: Chain[T], prefix: Chain[T], init: Chain[T]): Chain[T] =
    (chain, prefix) match {
      case (c ==: ctail, p ==: ptail) if c == p => skipPrefix(ctail, ptail, init)
      case (_, `nil`) => chain
      case (_, _) => init
    }

  @tailrec
  def skipCommonPrefix[T](chain1: Chain[T], chain2: Chain[T]): (Chain[T], Chain[T]) =
    (chain1, chain2) match {
      case (c ==: ctail, p ==: ptail) if c == p => skipCommonPrefix(ctail, ptail)
      case _ => chain1 -> chain2
    }

  @tailrec
  def skipSuffix[T](chain: Chain[T], suffix: Chain[T], init: Chain[T]): Chain[T] =
    (chain, suffix) match {
      case (cinit :== c, pinit :== p) if c == p => skipSuffix(cinit, pinit, init)
      case (_, `nil`) => chain
      case (_, _) => init
    }
}