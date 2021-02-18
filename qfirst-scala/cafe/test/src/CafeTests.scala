package qfirst.cafe

import cats._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import jjm.ling.en.InflectedForms
import jjm.implicits._

import munit.CatsEffectSuite

class CafeTests extends CatsEffectSuite {

  val eatForms = InflectedForms(
    stem = "eat".lowerCase,
    past = "ate".lowerCase,
    presentSingular3rd = "eats".lowerCase,
    pastParticiple = "eaten".lowerCase,
    presentParticiple = "eating".lowerCase
  )

  val doPro = Predication.Verbal.doSomething(
    subject = ArgumentPosition(Some(ArgumentProForm.what), None),
    Vector(), TAN(None, false, false, false)
  )

  val rendered = for {
    clauseType <- ClauseType.all
    includeSubject <- List(false, true)
  } yield (clauseType, includeSubject, doPro.render(clauseType, includeSubject))

  test("view rendered") {
    rendered.foreach(println)
  }
}
