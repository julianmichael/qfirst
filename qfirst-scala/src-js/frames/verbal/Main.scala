package qfirst.frames.verbal

import cats.implicits._

import org.scalajs.dom
import org.scalajs.dom.experimental

import scalacss.DevDefaults._

import qasrl.bank._
import qasrl.bank.service._

import scala.concurrent.Future

import radhoc._

import nlpdata.util.LowerCaseStrings._

object Main {
  def main(args: Array[String]): Unit = {
    VerbAnnStyles.addToDocument()
    val docApiEndpoint: String = dom.document
      .getElementById(SharedConstants.docApiUrlElementId)
      .getAttribute("value")
    val verbApiEndpoint: String = dom.document
      .getElementById(SharedConstants.verbApiUrlElementId)
      .getAttribute("value")

    import qasrl.bank.service.WebClientDocumentService
    val dataService = new WebClientDocumentService(docApiEndpoint)
    object CachedDataService extends DocumentService[CacheCall] {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.collection.mutable
      import DocumentService._
      var indexCache: Option[DataIndex] = None
      val documentCache = mutable.Map.empty[DocumentId, Document]
      val documentRequestCache = mutable.Map.empty[DocumentId, Future[Document]]

      def getDataIndex = indexCache.map(Cached(_)).getOrElse {
        val fut = dataService.getDataIndex
        fut.foreach { doc =>
          indexCache = Some(doc)
        }
        Remote(fut)
      }

      def getDocument(id: DocumentId) = {
        documentCache.get(id).map(Cached(_)).getOrElse {
          documentRequestCache.get(id).map(Remote(_)).getOrElse {
            val fut = dataService.getDocument(id)
            documentRequestCache.put(id, fut)
            fut.foreach { doc =>
              documentRequestCache.remove(id)
              documentCache.put(id, doc)
            }
            Remote(fut)
          }
        }
      }

      override def searchDocuments(query: Search.Query) = {
        if(query.isEmpty) {
          getDataIndex.map(_.allDocumentIds)
        } else {
          Remote(dataService.searchDocuments(query))
        }
      }
    }

    val path = dom.window.location.pathname.tail
    val verbMatch = Option(path).filter(_.nonEmpty).foldMap(_.takeWhile(_ != '/').split(",").map(_.lowerCase).toSet)
    val verbRemainderOpt = Option(path.dropWhile(_ != '/')).filter(_.nonEmpty).map(_.tail).filter(_.nonEmpty)
    val docMatch = verbRemainderOpt.foldMap(_.takeWhile(_ != '/').split(",").map(_.lowerCase).toSet)
    val docRemainderOpt = verbRemainderOpt.map(_.dropWhile(_ != '/')).filter(_.nonEmpty).map(_.tail).filter(_.nonEmpty)
    val sentMatch = docRemainderOpt.foldMap(_.takeWhile(_ != '/').split(",").map(_.lowerCase).toSet)
    val query = NavQuery(verbMatch, docMatch, sentMatch)

    VerbAnnUI.Component(
      VerbAnnUI.Props(
        CachedDataService, VerbAnnotationClient(verbApiEndpoint), query
      )
    ).renderIntoDOM(
      dom.document.getElementById(SharedConstants.mainDivElementId)
    )
  }
}
