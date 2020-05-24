package qfirst.metrics

object Functions {

  def harmonicMean(x: Double, y: Double) = {
    val denom = x + y
    if(denom == 0.0) 0.0 else {
      2 * x * y / (x + y)
    }
  }

  def weightedHarmonicMean(beta: Double, x: Double, y: Double) = {
    val betaSq = beta * beta
    val denom = (betaSq * x) + y
    if(denom == 0.0) 0.0 else {
      (1 + betaSq) * x * y / denom
    }
  }
}
