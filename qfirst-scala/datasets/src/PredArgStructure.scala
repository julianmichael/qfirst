package qfirst.datasets

import io.circe.generic.JsonCodec

@JsonCodec case class PredArgStructure[Pred, Arg](
  predicateIndex: Int,
  predicate: Pred,
  arguments: List[(String, Arg)])
