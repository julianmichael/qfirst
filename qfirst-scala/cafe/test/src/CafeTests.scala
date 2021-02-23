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
    println(pred)
    println(path)
    pred.render(
      inverted,
      path = path
    ).andThen { case (questionClauseTree, Extraction(arg, focusPath, swap)) =>
        // println("made it!")
        // println(LabeledTree.showGloss(questionClauseTree))
        arg.render(ArgPosition.Subj, swap, focusPath).map {
          case (focusedArgTree, ProFormExtraction(pro, swap)) =>
            val answers = answerNPs
              .map(Right(_))
              .map(swap.replace)
              .map(_.map(_.render(finite)))
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

  def getNounPro(animate: Boolean): Argument.ProForm.Noun =
    if(animate) Argument.ProForm.who else Argument.ProForm.what

  def getPrepAndMiscPrefix(prepString: LowerCaseString) = {
    val prepTokens = prepString.split(" ").toList
    prepTokens.reverse match {
      case (doWord @ ("do"|"doing")) :: "to" :: rest =>
        val prep = if(rest.isEmpty) None else Some(rest.reverse.mkString(" ").lowerCase)
        val miscPrefix = Some(s"to $doWord".lowerCase)
        prep -> miscPrefix
      case (doWord @ ("do"|"doing")) :: rest =>
        val prep = if(rest.isEmpty) None else Some(rest.reverse.mkString(" ").lowerCase)
        val miscPrefix = Some(doWord.lowerCase)
        prep -> miscPrefix
      case _ => Some(prepString) -> None
    }
  }

  def getAlignedPredications(
    verb: InflectedForms, question: QuestionLabel
  ): Set[(Predication.Clausal, ArgumentPath.Descent[Extraction])] = {
    val frames = ClauseResolution.getFramesWithAnswerSlots(
      verb, question.questionSlots
    ).filterNot { case (frame, slot) =>
        // remove 'where' locative/adv ambiguity, because we're collapsing them
        // TODO maybe don't bother...
        slot == qasrl.Obj2 && frame.args.get(qasrl.Obj2).exists(_ == qasrl.Locative)
    }
    frames.flatMap { case (frame, slot) =>
      val frameArgs = frame.structure.args
      val subj = getNounPro(frameArgs.get(qasrl.Subj).get.isAnimate)
      val obj = frameArgs.get(qasrl.Obj).map(_.isAnimate).map(getNounPro)
      val trailingArgSets: Vector[Vector[Argument.NonSubject]] =
        frameArgs.get(qasrl.Obj2).fold(Vector(Vector[Argument.NonSubject]())) {
          case qasrl.Noun(isAnimate) => Vector(Vector(getNounPro(isAnimate)))
          case qasrl.Locative => Vector(Vector(Argument.ProForm.where))
          case qasrl.Prep(prepStr, obj2Opt) =>
            val (prepsOpt, doWordsOpt) = getPrepAndMiscPrefix(prepStr)
            doWordsOpt match {
              case Some(doWords) =>
                // no object follows the preps, so must be particles
                val prts = prepsOpt.foldMap(preps =>
                  Vector(
                    Argument.Oblique(
                      Some(
                        Predication.Particulate(
                          NonEmptyList.fromList(
                            preps.split(" ").toList.map(_.lowerCase).map(Lexicon.Particle)
                          ).get))))
                )

                // object must be 'something' if do-words are present
                require(obj2Opt.exists(n => !n.isAnimate))

                // val prepChain = 
                // TODO maybe consider the possibility of extraction from between the two
                // which I think is ignored in frame resolution.
                // but also it happens so rarely it might be fine for this particular purpose.

                val doSomething = Predication.Verbal.doSomething(
                  subject = Argument.ProForm.who,
                  tan = present
                )

                doWords.toString match {
                  case "do" => Vector(prts :+ Argument.BareInfinitive(Some(doSomething)))
                  case "to do" => Vector(prts :+ Argument.ToInfinitive(Some(doSomething), false, Set()))
                    // TODO: get all the right/possible combinations of preps, prts, gerunds, obj, etc.
                  case _ => Vector() // zz. todo. vvv XXX
                  // case "doing" => Vector(
                  //   Argument.Gerund(false),
                  //   Argument.Progressive(false)
                  // )
                  // case "to doing" => Argument.Gerund(false) // TODO participle?
                }
              case None => prepsOpt match {
                case None => Vector(obj2Opt.foldMap(n => Vector(getNounPro(n.isAnimate))))
                case Some(preps) =>
                  val prtStrings = NonEmptyList.fromList(
                    preps.split(" ").toList.map(_.lowerCase).map(Lexicon.Particle)
                  ).get
                  val prtObl = Argument.Oblique(Some(Predication.Particulate(prtStrings)))
                  obj2Opt match {
                    case None => Vector(Vector(prtObl))
                    case Some(obj) =>
                      val prepStrings = NonEmptyList.fromList(
                        preps.split(" ").toList.map(_.lowerCase).map(Lexicon.Preposition)
                      ).get
                      val nounPro = getNounPro(obj.isAnimate)
                      val pp = Predication.Prepositional(prepStrings, nounPro)
                      val ppObl = Argument.Oblique(Some(pp))
                      Vector(Vector(prtObl, nounPro), Vector(ppObl))
                  }
              }
            }
        }
      val advArg = slot match {
        case qasrl.Adv(wh) => wh.toString match {
          case "where" => Vector(Argument.ProForm.where)
          case "when"  => Vector(Argument.ProForm.when)
          case "how"   => Vector(Argument.ProForm.how)
          case "why"   => Vector(Argument.ProForm.why)
          case _ => Vector()
        }
        case _ => Vector()
      }
      trailingArgSets.map { trailingArgs =>
        val args: Vector[Argument.NonSubject] = obj.toVector ++ trailingArgs ++ advArg
        val argSeq = slot match {
          case qasrl.Subj => ArgPosition.Subj :: Nil
          case qasrl.Obj => ArgPosition.Arg(0) :: Nil
          case qasrl.Obj2 =>
            require(trailingArgs.nonEmpty)
            val init = ArgPosition.Arg(args.size - 1) // no adv arg, so we're last
            trailingArgs.last match {
              case Argument.Oblique(Some(Predication.Prepositional(_, _, _)), _) =>
                init :: ArgPosition.Arg(0) :: Nil
              case _: Argument.Clausal => // do-phrase
                init :: ArgPosition.Arg(0) :: Nil
              case _ =>
                init :: Nil
            }
          case qasrl.Adv(_) => ArgPosition.Arg(args.size - 1) :: Nil
        }
        val argPath = {
          import ArgumentPath._
          argSeq.foldRight[ArgumentPath[Extraction]](
            End(Extract(End(Focus))))(
            Descent(_, _)).asInstanceOf[ArgumentPath.Descent[Extraction]]
        }
        val tense = frame.tense match {
          case t: Tense.Finite => Some(t)
          case _ =>
            println(frame.tense)
            None
        }
        val pred = Predication.Verbal(
          subject = subj,
          verb = Lexicon.Verb(verb),
          isPassive = frame.isPassive,
          arguments = args,
          tan = TAN(
            tense = tense,
            isPerfect = frame.isPerfect,
            isProgressive = frame.isProgressive,
            isNegated = frame.isNegated
          )
        )
        pred -> argPath
      }
    }
  }

  test("align to QA-SRL") {
    val qasrlPath = Paths.get("../qasrl/data/qasrl-v2_1/orig/dev.jsonl.gz")
    IO.fromTry(Data.readQasrlDataset(qasrlPath)).flatMap { data =>
      data.sentences.take(8).toList.traverse { case (sid, sentence) =>
        IO {
          println("\n\n" + sid)
          println(Text.render(sentence.sentenceTokens))
          sentence.verbEntries.foreach { case (verbIndex, verb) =>
            println(s"($verbIndex) ${verb.verbInflectedForms.stem}")
            verb.questionLabels
              // .filter(p => (p._1.endsWith("do?") || p._1.endsWith("doing?")))
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

              getAlignedPredications(verb.verbInflectedForms, qLabel).foreach {
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
