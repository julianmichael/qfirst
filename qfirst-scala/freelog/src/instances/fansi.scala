package freelog.instances

import cats.Monoid
import _root_.fansi.Str

object fansi extends FansiInstances

trait FansiInstances {
  implicit def fansiStrMonoid: Monoid[Str] = new Monoid[Str] {
    def empty: Str = Str("")
    def combine(x: Str, y: Str): Str = x ++ y
  }
}
