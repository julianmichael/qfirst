package qfirst.frame.browse
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

import scala.concurrent.Future

import jjm.DotKleisli
import jjm.DotMap
import jjm.OrWrapped
import jjm.io.HttpUtil
import jjm.implicits._

import radhoc._

object Main {
  def main(args: Array[String]): Unit = {

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

    val verbApiEndpoint: String = dom.document
      .getElementById(SharedConstants.verbApiUrlElementId)
      .getAttribute("value")

    val verbFrameService = VerbFrameService(
      HttpUtil
        .makeHttpPostClient[VerbFrameService.Request](verbApiEndpoint)
        .andThenK(wrapCallback)
    )

    // // TODO replace with simple caching transformation thing thing
    // import qasrl.bank.service.WebClientDocumentService
    // val dataService = new WebClientDocumentService(docApiEndpoint)
    // object CachedDataService extends DocumentService[OrWrapped[Future, ?]] {
    //   import scala.concurrent.ExecutionContext.Implicits.global
    //   import scala.collection.mutable
    //   import DocumentService._
    //   var indexCache: Option[DataIndex] = None
    //   val documentCache = mutable.Map.empty[DocumentId, Document]
    //   val documentRequestCache = mutable.Map.empty[DocumentId, Future[Document]]

    //   def getDataIndex = indexCache.map(OrWrapped.pure[Future](_)).getOrElse {
    //     val fut = dataService.getDataIndex
    //     fut.foreach { doc =>
    //       indexCache = Some(doc)
    //     }
    //     OrWrapped.wrapped(fut)
    //   }

    //   def getDocument(id: DocumentId) = {
    //     documentCache.get(id).map(OrWrapped.pure[Future](_)).getOrElse {
    //       documentRequestCache.get(id).map(OrWrapped.wrapped(_)).getOrElse {
    //         val fut = dataService.getDocument(id)
    //         documentRequestCache.put(id, fut)
    //         fut.foreach { doc =>
    //           documentRequestCache.remove(id)
    //           documentCache.put(id, doc)
    //         }
    //         OrWrapped.wrapped(fut)
    //       }
    //     }
    //   }

    //   override def searchDocuments(query: Search.Query) = {
    //     if(query.isEmpty) {
    //       getDataIndex.map(_.allDocumentIds)
    //     } else {
    //       OrWrapped.wrapped(dataService.searchDocuments(query))
    //     }
    //   }
    // }

    val query = NavQuery.fromString(dom.window.location.pathname.tail)

    VerbAnnUI.Component(
      VerbAnnUI.Props(
        documentService, verbFrameService, query,
        io.circe.parser.decode[RunMode](
          dom.document.getElementById(SharedConstants.devFlagElementId).getAttribute("value")
        ).right.get
      )
    ).renderIntoDOM(
      dom.document.getElementById(SharedConstants.mainDivElementId)
    )
  }
}
