package qfirst

trait Implicits {
  final implicit class RichAny[A](val a: A) {
    def <|[B](f: A => B): B = f(a)
  }
}
