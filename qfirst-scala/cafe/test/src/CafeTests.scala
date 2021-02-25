package qfirst.cafe

import cats._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import jjm.LowerCaseString
import jjm.ling.Text
import jjm.ling.en.InflectedForms
import jjm.implicits._

import munit.CatsEffectSuite
import cats.data.Validated
import qasrl.Tense
import qasrl.labeling.ClauseResolution
import qasrl.ArgStructure
import qasrl.labeling.SlotBasedLabel
import cats.data.NonEmptyList
import qasrl.data.QuestionLabel
import qfirst.cafe.Predication.ClauseFeats

class CafeTests extends CatsEffectSuite {

  val eat = InflectedForms(
    stem = "eat".lowerCase,
    past = "ate".lowerCase,
    presentSingular3rd = "eats".lowerCase,
    pastParticiple = "eaten".lowerCase,
    presentParticiple = "eating".lowerCase
  )

  val want = InflectedForms(
    stem = "want".lowerCase,
    past = "wanted".lowerCase,
    presentSingular3rd = "wants".lowerCase,
    pastParticiple = "wanted".lowerCase,
    presentParticiple = "wanting".lowerCase
  )

  val present = TAN(Some(Tense.Finite.Present), false, false, false)

  val doPro = Predication.Verbal.doSomething(
    // clauseType, includeSubject,
    subject = Argument.ProForm.who,
    // modifiers = Vector(),
    tan = present
  )

  val eatPred = Predication.Verbal.apply(
    None,
    subject = Argument.ProForm.who,
    verb = Lexicon.Verb(eat),
    isPassive = false,
    arguments = Vector(Argument.ProForm.what),
    tan = TAN(Some(Tense.Finite.Modal("might".lowerCase)), false, false, false)
  )

  val wantEat = Predication.Verbal(
    None,
    subject = Argument.ProForm.who,
    verb = Lexicon.Verb(want),
    isPassive = false,
    arguments = Vector(Argument.ToInfinitive(Some(eatPred), false, Set())),
    tan = TAN(Some(Tense.Finite.Present), true, false, false)
  )

  val wantDo = Predication.Verbal(
    None,
    subject = Argument.ProForm.who,
    verb = Lexicon.Verb(want),
    isPassive = false,
    arguments = Vector(Argument.ToInfinitive(Some(doPro), false, Set())),
    tan = TAN(Some(Tense.Finite.Present), true, false, false)
  )

  val rendered = for {
    clauseType <- ClauseType.all
    includeSubject <- List(false, true)
  } yield (
    wantDo.render(Predication.ClauseFeats(clauseType, includeSubject)),
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

  test("produce answer candidates/templates") {
    wantEat.extractionPaths
      .map { extractionPath =>
        renderQA(wantEat, extractionPath, Vector())
      }.foreach {
        case Validated.Valid(out) =>
          println(out)
        case Validated.Invalid(err) =>
          println(err)
      }
  }

  def renderQA(
    pred: Predication.Clausal,
    path: ArgumentPath.Descent[Extraction],
    answers: Vector[String]
  ) = {
    val answerNPs = answers.map(ans =>
      Argument.NounPhrase(
        Some(
          Predication.NounPhrase(
            None,
            ans,
            None,
            Some(Person.Third),
            Some(Number.Singular)
          )
        ),
        None
      )
    ) ++ Vector(
      Argument.BareInfinitive(Some(eatPred)),
      Argument.ProForm.who
    )
    val inverted = Predication.ClauseFeats(ClauseType.Inverted, includeSubject = true)
    val finite = Predication.ClauseFeats(ClauseType.Finite, includeSubject = true)
    // println(pred)
    // println(path.show)
    pred.render(
      inverted,
      path = path
    ).andThen { case (questionClauseTree, Extraction(arg, focusPath, swap)) =>
        // println("----------")
        // println("made it!")
        // println(LabeledTree.showGloss(questionClauseTree))
        // println(focusPath.show)
        // println(arg)
        // println(swap.replace(Right(Argument.ProForm.who)))
        // println("----------")
        arg.render(ArgPosition.Subj, swap, focusPath).map {
          case (focusedArgTree, ProFormExtraction(pro, swap)) =>
            val answers = answerNPs
              .map(Right(_))
              .map(swap.replace)
              // .map(x => {println("\n~~~~~\n" + x + "\n~~~~~~\n"); x})
              .map(_.map(_.render(finite)))
              // .map(x => {println("\nvvvvv\n" + x + "\n^^^^^^\n"); x})
            // .map(_.andThen(_.render(finite)))
            Validated.valid(
              (focusedArgTree |+| questionClauseTree)
            ) -> answers
        }
    }.map { case (questionV, answerVs) =>
        val question = questionV match {
          case Validated.Valid(q) => LabeledTree.showGloss(q)
          case Validated.Invalid(err) => err.toString
        }
        val answers = answerVs.map {
          case Right(Validated.Valid(a)) => LabeledTree.showGloss(a)
          case Right(Validated.Invalid(err)) => err.toString
          case Left(err) => err.toString
        }
        question + "\n" + answers.mkString("\n")
    }
  }

  import qasrl.bank.Data
  import java.nio.file.Paths

  test("align to QA-SRL") {
    val qasrlPath = Paths.get("../qasrl/data/qasrl-v2_1/orig/dev.jsonl.gz")
    IO.fromTry(Data.readQasrlDataset(qasrlPath)).flatMap { data =>
      data.sentences.take(30).toList.traverse { case (sid, sentence) =>
        IO {
          println("\n\n" + sid)
          println(Text.render(sentence.sentenceTokens))
          sentence.verbEntries.foreach { case (verbIndex, verb) =>
            println(s"($verbIndex) ${verb.verbInflectedForms.stem}")
            verb.questionLabels
              .filter(p => (p._1.endsWith("do?") || p._1.endsWith("doing?")))
              // .filter(p => (p._1.endsWith("into?")))
              .foreach { case (qString, qLabel) =>
              val answerSpans = qLabel.answerJudgments
                .flatMap(_.judgment.getAnswer)
                .flatMap(_.spans.toSortedSet)
              val answerStrings = answerSpans.toVector.sorted
                .map(s => Text.renderSpan(sentence.sentenceTokens, s))
              val answersString = answerStrings.mkString(" / ")

              val div = " ========== "
              println(div + qString + div)
              println("\t" + answersString)

              Conversion.getAlignedPredications(verb.verbInflectedForms, qLabel).foreach {
                case (pred, path) =>
                  pred.render(ClauseFeats(ClauseType.Finite, true)) match {
                    case Validated.Valid(tree) => println(LabeledTree.showGloss(tree))
                    case Validated.Invalid(err) => println("\t" + err)
                  }
                  println
                  renderQA(pred, path, answerStrings) match {
                    case Validated.Valid(res) => println(res)
                    case Validated.Invalid(err) => println(err)
                  }
              }
            }
          }
        }
      }
    }
  }

  // predication: Predication
  // innerPath: ArgumentPath[ProForm]
  // set of allowable answers
  // procedure to incorporate answer back into the original.
  // origPath: ArgumentPath[?] -- points to extracted element
  //
  // FIRST: normal pied piping.
  // I want: 
  //
  // I want: tree, arg, inner path, list of answer types, for each one ???


  // TODO
  // test("argument extraction paths are always valid") {
  // }
  // test("predication extraction paths are always valid") {
  // }
}
