package qfirst.cafe

import cats.data.NonEmptyList
import cats.implicits._

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.implicits._

import qasrl.Tense
import qasrl.data.QuestionLabel
import qasrl.labeling.ClauseResolution

object Conversion {

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
                val prts: Vector[Argument.Oblique] = prepsOpt.foldMap(preps =>
                  preps.split(" ").toVector
                    .map(_.lowerCase)
                    .map(Lexicon.Particle)
                    .map(prt => Argument.Oblique(Some(Predication.Particulate(None, prt))))
                )

                // object must be 'something' if do-words are present
                require(obj2Opt.exists(n => !n.isAnimate))

                // val prepChain = 
                // TODO maybe consider the possibility of extraction from between the two
                // which I think is ignored in frame resolution.
                // but also it happens so rarely it might be fine for this particular purpose.

                val doSomething = Predication.Verbal.doSomething(
                  subject = Argument.ProForm.who,
                  tan = TAN(None, false, false, false)
                )

                doWords.toString match {
                  case "do" => Vector(prts :+ Argument.BareInfinitive(Some(doSomething)))
                  case "to do" => Vector(prts :+ Argument.ToInfinitive(Some(doSomething), false))
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
                  val prepStrs = preps.split(" ").toVector.map(_.lowerCase)
                  val prts = prepStrs
                    .map(Lexicon.Particle)
                    .map(Predication.Particulate(None, _))
                    .map(prt => Argument.Oblique(Some(prt)))
                  obj2Opt match {
                    case None => Vector(prts)
                    case Some(obj) =>
                      val remPrts = prts.init
                      val nounPro = getNounPro(obj.isAnimate)
                      val prep = Argument.Oblique(
                        Some(
                          Predication.Prepositional(
                            None,
                            Lexicon.Preposition(prepStrs.last),
                            nounPro
                          )
                        )
                      )
                      Vector(prts :+ nounPro, remPrts :+ prep)
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
              case Argument.Oblique(Some(Predication.Prepositional(_, _, _, _)), _) =>
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
          None,
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

}
