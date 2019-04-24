// package qfirst.paraphrase.models

// import scala.util.Random
// import cats.implicits._

// import breeze.linalg.DenseVector

// class DenseMultinomial private (val probabilities: DenseVector[Double]) {
//   def size: Int = probabilities.size
//   def sample(rand: Random) = qfirst.paraphrase.models.sample(probabilities, rand)
//   // TODO equals and stuff
// }
// object DenseMultinomial {
//   // assume normalized
//   def fromNormalized(probs: Vector[Double]): DenseMultinomial = new DenseMultinomial(probs)
//   def fromUnnormalized(unnormProbs: Vector[Double]): DenseMultinomial = {
//     val normalizer = unnormProbs.sum
//     new DenseMultinomial(unnormProbs.map(_ / normalizer))
//   }
//   def fromLogits(logits: Vector[Double]): DenseMultinomial = fromUnnormalized(logits.map(math.exp))
// }
