package qfirst.frame.ann

import cats.~>
import cats.Id
import cats.implicits._

import org.scalajs.dom
import org.scalajs.dom.experimental

import scalacss.DevDefaults._

import qasrl.bank._
import qasrl.bank.service._

import scala.concurrent.Future

import jjm.DotMap
import jjm.OrWrapped
import jjm.LowerCaseString
import jjm.io.HttpUtil
import jjm.implicits._

import japgolly.scalajs.react.AsyncCallback

object Main {
  def main(args: Array[String]): Unit = {
    VerbAnnStyles.addToDocument()

    val docApiEndpoint: String = dom.document
      .getElementById(SharedConstants.docApiUrlElementId)
      .getAttribute("value")

    val initialCache = DotMap.empty[Id, DocumentService.Request]
    //  .put(DocumentService.GetDataIndex)(dataIndex)
    //  .put(DocumentService.SearchDocuments(Search.Query(None, Set())))(dataIndex.allDocumentIds)

    import scala.concurrent.ExecutionContext.Implicits.global

    type DelayedFuture[A] = () => Future[A]
    val wrapCallback = λ[DelayedFuture ~> AsyncCallback](f =>
      AsyncCallback.fromFuture(f())
    )

    val documentService = DocumentService(
      jjm.Memo.memoizeDotFuture(
        HttpUtil.makeHttpPostClient[DocumentService.Request](docApiEndpoint),
        initialCache
      ).andThenK(OrWrapped.mapK(wrapCallback))
    )

    val verbApiEndpoint: String = dom.document
      .getElementById(SharedConstants.verbApiUrlElementId)
      .getAttribute("value")

    val verbClient = VerbAnnotationService(
      HttpUtil
        .makeHttpPostClient[VerbAnnotationService.Request](verbApiEndpoint)
        .andThenK(wrapCallback)
    )

    val query = NavQuery.fromString(dom.window.location.pathname.tail)

    VerbAnnUI.Component(
      VerbAnnUI.Props(documentService, verbClient, query)
    ).renderIntoDOM(
      dom.document.getElementById(SharedConstants.mainDivElementId)
    )
  }
}
