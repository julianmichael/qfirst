package qfirst.frame.browse

import qfirst.frame.DataSetting
import qfirst.frame.RunMode

import japgolly.scalajs.react._

import cats.~>
import cats.Id
import cats.implicits._

import org.scalajs.dom
import org.scalajs.dom.experimental

import scalacss.DevDefaults._

import qasrl.bank._
import qasrl.bank.service._

import io.circe.{Encoder, Decoder}

import scala.concurrent.Future

import jjm.DotKleisli
import jjm.DotMap
import jjm.OrWrapped
import jjm.io.HttpUtil
import jjm.implicits._

import radhoc._

object Main {
  def runApp[VerbType: Encoder : Decoder, Arg: Encoder : Decoder] = {
    VerbAnnStyles.addToDocument()

    import scala.concurrent.ExecutionContext.Implicits.global

    val docApiEndpoint: String = dom.document
      .getElementById(SharedConstants.docApiUrlElementId)
      .getAttribute("value")

    type DelayedFuture[A] = () => Future[A]
    val wrapCallback = λ[DelayedFuture ~> AsyncCallback](f =>
      AsyncCallback.fromFuture(f())
    )

    val initialFeatureCache = DotMap.empty[Id, FeatureReq[VerbType, Arg]]

    val featureApiEndpoint: String = dom.document
      .getElementById(SharedConstants.featureApiUrlElementId)
      .getAttribute("value")
    val featureService = jjm.Memo.memoizeDotFuture(
      HttpUtil.makeHttpPostClient[FeatureReq[VerbType, Arg]](featureApiEndpoint),
      initialFeatureCache
        // TODO remove cast
    ).andThenK(OrWrapped.mapK(wrapCallback))
      .asInstanceOf[FeatureService[OrWrapped[AsyncCallback, ?], jjm.ling.en.InflectedForms, qfirst.frame.ClausalQuestion]]

    val verbApiEndpoint: String = dom.document
      .getElementById(SharedConstants.verbApiUrlElementId)
      .getAttribute("value")

    val initialVerbCache = DotMap.empty[Id, VerbFrameService.Request]

    val verbFrameService = VerbFrameService(
      jjm.Memo.memoizeDotFuture(
        HttpUtil.makeHttpPostClient[VerbFrameService.Request](verbApiEndpoint),
        initialVerbCache
      ).andThenK(OrWrapped.mapK(wrapCallback))
    )

    val query = NavQuery.fromString(dom.window.location.pathname.tail)

    val runMode = io.circe.parser.decode[RunMode](
      dom.document.getElementById(SharedConstants.devFlagElementId).getAttribute("value")
    ).right.get

    NewVerbUI.Component(
      NewVerbUI.Props(
        verbFrameService, featureService,
        query, runMode
      )
    ).renderIntoDOM(
      dom.document.getElementById(SharedConstants.mainDivElementId)
    )
  }

  def main(args: Array[String]): Unit = {
    val dataSetting: DataSetting = DataSetting.fromString(
      dom.document
      .getElementById(SharedConstants.dataSettingElementId)
      .getAttribute("value")
    ).get

    dataSetting match {
      case d @ DataSetting.Qasrl => runApp[d.VerbType, d.Arg]
      case d @ DataSetting.Ontonotes5(_) => () // runApp[d.VerbType, d.Arg]
      case d @ DataSetting.CoNLL08(_) => () // runApp[d.VerbType, d.Arg]
    }
  }
}
