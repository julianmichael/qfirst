import qasrl.data.AnswerSpan

package object qfirst {
  def overlaps(x: AnswerSpan, y: AnswerSpan): Boolean = {
    x.begin <= y.end && y.begin <= x.end
  }

  implicit class RichAny[A](val a: A) extends AnyVal {
    def <|[B](f: A => B): B = f(a)
  }
}
