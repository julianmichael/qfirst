// package qfirst.paraphrase.models

// case class Logits(val values: Vector[Double]) extends AnyVal {
//   def size: Int = values.size
//   def +(that: Logits) = {
//     if(this.size != that.size) throw new IllegalArgumentException("Adding logit vectors of unequal sizes ${this.size} and ${that.size}")
//     else Logits((this.values, that.values).zipped.map(_ + _).toVector)
//   }
// }
