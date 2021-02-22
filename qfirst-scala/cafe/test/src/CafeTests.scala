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
    modifiers = Vector(),
    tan = present
  )

  val eatPred = Predication.Verbal.apply(
    subject = Argument.ProForm.who,
    verb = Lexicon.Verb(eat),
    isPassive = false,
    arguments = Vector(Argument.ProForm.what),
    tan = TAN(Some(Tense.Finite.Modal("might".lowerCase)), false, false, false)
  )

  val wantEat = Predication.Verbal(
    subject = Argument.ProForm.who,
    verb = Lexicon.Verb(want),
    isPassive = false,
    arguments = Vector(Argument.ToInfinitive(Some(eatPred), false, Set())),
    tan = TAN(Some(Tense.Finite.Present), true, false, false)
  )

  val wantDo = Predication.Verbal(
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
    wantDo.render(clauseType, includeSubject, None),
    (clauseType, includeSubject)
  )

  test("view rendered") {
    rendered.foreach {
      case (res, (clauseType, inclSubj)) =>
        val inclSubjStr = if(inclSubj) " (with subject)" else ""
        println(s"\n$clauseType$inclSubjStr")
        res match {
          case Validated.Valid(tree) => println(LabeledTree.showGloss(tree.value))
          case Validated.Invalid(errs) =>
            errs.toList.foreach(err => println("\t" + err.msg + "\n\t" + err.component.toString))
        }
    }
  }

  test("ask questions") {
    val somePaths = None :: wantEat.argumentPaths.toList.map(Some(_))
    somePaths.foreach { path =>
      val question = wantEat.renderQuestion(path)
      println("\n" + path.fold("N/A")(_.show))
      question match {
        case Validated.Valid(tree) => println(LabeledTree.showGloss(tree))
        case Validated.Invalid(errs) =>
          errs.toList.foreach(err => println("\t" + err.msg + "\n\t" + err.component.toString))
      }
    }
  }

  // TODO: for each path, extract the corresponding pronoun,
  // get its possible filler templates,
  // unify them with all possible predications in the context
  test("produce answer candidates/templates") {
    wantEat.argumentPaths
      .collect { case ex: ExtractionPath => ex }
      .map { extractionPath =>
        wantEat.render(
          ClauseType.Inverted,
          includeSubject = true,
          path = Some(extractionPath)
        ).andThen {
          case Component.WithExtraction(questionClauseTree, extractions) =>
            require(extractions.size == 1)
            val (arg, focusPath) = extractions.head
            arg.render(ArgPosition.Subj, Some(focusPath)).andThen {
              case Component.WithExtraction(focusedArgTree, answerExtractions) =>
                require(answerExtractions.size == 0)
                val answerProForm: Argument.ProForm = ??? // should recover from extraction
                val answerTemplates = answerProForm.instances
                Validated.valid((focusedArgTree |+| questionClauseTree) -> answerTemplates)
            }
        }
      }.foreach {
        case Validated.Valid((question, answers)) =>
          println("\n" + LabeledTree.showGloss(question))
          answers.foreach(println)
        case Validated.Invalid(err) =>
          println(err)
      }
  }

    // def renderQuestion(
    //   path: Option[ArgumentPath.Descent]
    // ): Component.RenderResultOf[LabeledTree[String, String]] = {
    //   val clauseType =
    //     if(path.forall(_.isExtraction)) ClauseType.Inverted
    //     else ClauseType.Finite
    //   render(clauseType, includeSubject = true, path = path).andThen {
    //     case WithExtraction(tree, extractions) =>
    //       extractions.headOption match {
    //         case None => valid(tree)
    //         case Some((arg, path)) =>
    //           arg.render(ArgPosition.Subj, Some(path)).andThen {
    //             case WithExtraction(argTree, argExtractions) =>
    //               if(argExtractions.nonEmpty) invalid("Should not have a nested extraction.")
    //               else valid(argTree |+| tree)
    //           }
    //       }
    //   }
    // }

  // TODO
  // test("argument extraction paths are always valid") {
  // }
  // test("predication extraction paths are always valid") {
  // }
}
