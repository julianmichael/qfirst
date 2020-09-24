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
  def runApp[VerbType: Encoder, Arg: Encoder : Decoder] = {
    VerbAnnStyles.addToDocument()

    import scala.concurrent.ExecutionContext.Implicits.global

    val docApiEndpoint: String = dom.document
      .getElementById(SharedConstants.docApiUrlElementId)
      .getAttribute("value")

    type DelayedFuture[A] = () => Future[A]
    val wrapCallback = λ[DelayedFuture ~> AsyncCallback](f =>
      AsyncCallback.fromFuture(f())
    )

    val initialCache = DotMap.empty[Id, DocumentService.Request]

    val documentService = DocumentService(
      jjm.Memo.memoizeDotFuture(
        HttpUtil.makeHttpPostClient[DocumentService.Request](docApiEndpoint),
        initialCache
      ).andThenK(OrWrapped.mapK(wrapCallback))
    )

    // TODO cache
    val featureApiEndpoint: String = dom.document
      .getElementById(SharedConstants.featureApiUrlElementId)
      .getAttribute("value")
    val featureService = HttpUtil
      .makeHttpPostClient[FeatureReq[VerbType, Arg]](featureApiEndpoint)
      .andThenK(wrapCallback)
      .asInstanceOf[FeatureService[AsyncCallback, jjm.ling.en.InflectedForms, qfirst.frame.ClausalQuestion]]
    // TODO remove cast

    val verbApiEndpoint: String = dom.document
      .getElementById(SharedConstants.verbApiUrlElementId)
      .getAttribute("value")

    // TODO cache
    val verbFrameService = VerbFrameService(
      HttpUtil
        .makeHttpPostClient[VerbFrameService.Request](verbApiEndpoint)
        .andThenK(wrapCallback)
    )

    val query = NavQuery.fromString(dom.window.location.pathname.tail)

    val runMode = io.circe.parser.decode[RunMode](
      dom.document.getElementById(SharedConstants.devFlagElementId).getAttribute("value")
    ).right.get

    VerbAnnUI.Component(
      VerbAnnUI.Props(
        documentService, verbFrameService, featureService,
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
