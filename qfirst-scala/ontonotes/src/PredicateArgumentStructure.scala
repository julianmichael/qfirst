package qfirst.ontonotes

import jjm.ling.ESpan

import io.circe.generic.JsonCodec

/** Represents a single predicate--argument structure.
  *
  * A CoNLL sentence contains a list of these;
  * the words in the head and argument spans will be the same words
  * that appear in the CoNLL sentence itself.
  *
  * @param pred the predicate of the PAS
  * @param arguments the argument spans
  */
@JsonCodec case class PredicateArgumentStructure(
  predicate: Predicate,
  arguments: List[(String, ESpan)])

/** Represents the predicate of a predicate--argument structure. */
@JsonCodec case class Predicate(
  index: Int,
  lemma: String,
  sense: String)
