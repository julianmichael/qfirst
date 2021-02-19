package qfirst.cafe

import cats._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import jjm.ling.en.InflectedForms
import jjm.implicits._

import munit.CatsEffectSuite
import cats.data.Validated
import qasrl.Tense

class CafeTests extends CatsEffectSuite {

  val eatForms = InflectedForms(
    stem = "eat".lowerCase,
    past = "ate".lowerCase,
    presentSingular3rd = "eats".lowerCase,
    pastParticiple = "eaten".lowerCase,
    presentParticiple = "eating".lowerCase
  )

  def doPro(clauseType: ClauseType, includeSubject: Boolean) =
    ArgumentProForm.DoSomething(
      clauseType, includeSubject,
      subject = ArgumentPosition(Some(ArgumentProForm.what), None),
      modifiers = Vector(),
      tan = TAN(Some(Tense.Finite.Present), false, false, false)
  )

  val rendered = for {
    clauseType <- ClauseType.all
    includeSubject <- List(false, true)
  } yield (
    doPro(clauseType, includeSubject).render(RenderContext.Arg),
    (clauseType, includeSubject)
  )

  test("view rendered") {
    rendered.foreach {
      case (res, (clauseType, inclSubj)) =>
        val inclSubjStr = if(inclSubj) " (with subject)" else ""
        println(s"\n$clauseType$inclSubjStr")
        res match {
          case Validated.Valid(tree) => println(LabeledTree.showGloss(tree))
          case Validated.Invalid(errs) =>
            errs.toList.foreach(err => println("\t" + err.msg + "\n\t" + err.component.toString))
        }
    }
  }
}
