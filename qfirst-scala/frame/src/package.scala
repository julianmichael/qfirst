package qfirst

import cats.Order
import cats.implicits._

import io.circe.generic.JsonCodec

import monocle.macros._

import jjm.ling.ESpan
import jjm.ling.en.InflectedForms

import qasrl.ArgumentSlot
import qasrl.Frame

import qfirst.clause.ArgStructure

import freelog.LogLevel
import freelog.ProgressSpec

package object frame extends qfirst.frame.PackagePlatformExtensions {
  implicit val logLevel = LogLevel.Trace
  implicit val progressSpec = ProgressSpec.simple(barLength = 50)

  // XXX obsolete
  type ClausalQ = (Frame, ArgumentSlot)

  type TemplateQ = (ArgStructure, ArgumentSlot)
  type QAPairs = Map[ClausalQuestion, List[List[ESpan]]]

  type Instances[VerbType, A] = Map[
    VerbType, Map[String, NonMergingMap[Int, A]]
  ]
  object Instances {
    type Qasrl = Instances[InflectedForms, QAPairs]
    type PropBank = Instances[String, QAPairs]
  }
  type ParaphraseAnnotations = Map[
    // sentence
    String, Map[
      // verb index
      Int, VerbParaphraseLabels
    ]
  ]


  @Lenses @JsonCodec case class ClausalQuestion(
    frame: Frame,
    slot: ArgumentSlot
  ) {
    def clauseTemplate = ArgStructure(frame.args, frame.isPassive).forgetAnimacy
    def template: TemplateQ = clauseTemplate -> slot
    // TODO remove need for this
    def tuple: (Frame, ArgumentSlot) = (frame, slot)
  }
  object ClausalQuestion {
    implicit val clausalQuestionOrder: Order[ClausalQuestion] = Order.whenEqual(
      Order.by[ClausalQuestion, String](_.frame.toString),
      Order.by[ClausalQuestion, String](_.slot.toString)
    )
  }

  @Lenses @JsonCodec case class VerbId(
    sentenceId: String, verbIndex: Int
  )
  object VerbId {
    import io.circe._
    implicit val verbIdKeyEncoder = KeyEncoder.instance[VerbId](vid =>
      s"${vid.sentenceId}:${vid.verbIndex}"
    )
    implicit val verbIdKeyDecoder = KeyDecoder.instance[VerbId](s =>
      scala.util.Try(VerbId(s.reverse.dropWhile(_ != ':').tail.reverse, s.reverse.takeWhile(_ != ':').reverse.toInt)).toOption
    )
    implicit val verbIdOrder = Order.whenEqual(
      Order.by[VerbId, String](_.sentenceId),
      Order.by[VerbId, Int](_.verbIndex)
    )
  }

  @Lenses @JsonCodec case class QuestionId(
    verbId: VerbId, question: ClausalQuestion
  ) {
    def frame = question.frame
    def slot = question.slot
  }
  object QuestionId {
    implicit val questionIdOrder =
      Order.whenEqual(
        Order.by[QuestionId, VerbId](_.verbId),
        Order.by[QuestionId, ClausalQuestion](_.question)
      )
  }

  // type PropBank = Instances[String, QAPairs]
  // type PropBankElmo = Instances[String, DenseVector[Float]]
  // type PropBankLabels = Instances[String, String]
  // type Qasrl = Instances[InflectedForms, QAPairs]
  // type QasrlElmo = Instances[InflectedForms, DenseVector[Float]]

  def getArgumentSlotsForClauseTemplate(clauseTemplate: ArgStructure): Set[ArgumentSlot] = {
    (clauseTemplate.args.keys.toList: List[ArgumentSlot]).filter {
      case qasrl.Obj2 => clauseTemplate.args.get(qasrl.Obj2) match {
        case Some(qasrl.Prep(_, None)) => false
        case _ => true
      }
      case _ => true
    }.toSet
  }

  import scala.collection.immutable.SortedSet
  import cats.Order
  import cats.data.NonEmptySet
  import cats.implicits._
  import monocle.Iso

  def nonEmptySetOptionIso[A: Order] = Iso[Option[NonEmptySet[A]], Set[A]](
    _.foldMap(_.toSortedSet: Set[A]))(
    s => NonEmptySet.fromSet(SortedSet(s.toSeq:_*)))
}
