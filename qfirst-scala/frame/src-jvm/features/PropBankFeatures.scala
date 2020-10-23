package qfirst.frame.features

import qfirst.frame._

import qfirst.frame.util.Cell
import qfirst.frame.util.FileCached
import qfirst.frame.util.NonMergingMap
import qfirst.frame.util.VectorFileUtil

import java.nio.file._

import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.FileUtil
import jjm.implicits._

import cats.Order
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream

import io.circe.generic.JsonCodec
import io.circe.{Encoder, Decoder}

import freelog.EphemeralTreeLogger
import freelog.implicits._

abstract class PropBankFeatures[Arg](
  mode: RunMode,
  val assumeGoldVerbSense: Boolean)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends Features[String, Arg](mode)(implicitly[Encoder[String]], implicitly[Decoder[String]], cs, Log) {

  override def getIfPropBank: Option[PropBankFeatures[Arg]] = Some(this)

  override def getVerbLemma(verbType: String): String = {
    if(assumeGoldVerbSense) verbType.takeWhile(_ != '.')
    else verbType
  }

  def renderVerbType(verbType: String): String = verbType

  // don't store the models in the same dir, because they cluster different kinds of things
  override def modelDir = super.modelDir.map(
    _.resolve(if(assumeGoldVerbSense) "by-sense" else "by-lemma")
  ).flatTap(createDir)

  def verbSenseLabels: CachedVerbFeats[String]

  def argRoleLabels: CachedArgFeats[PropBankRoleLabel]
}
