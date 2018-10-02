import qasrl.data.AnswerSpan

package object qfirst extends Implicits {
  def overlaps(x: AnswerSpan, y: AnswerSpan): Boolean = {
    x.begin <= y.end && y.begin <= x.end
  }
}
