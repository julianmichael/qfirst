package qfirst.frame

import jjm.ling.ESpan
import jjm.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class PropBankRoleLabel(
  framesetId: String,
  role: String
) {
  override def toString = s"$framesetId.$role"
}
object PropBankRoleLabel {

  // the pred itself, discourse markers, negations, and auxiliaries we don't care about
  def roleLabelIsIrrelevant(l: String) = {
    l == "V" || l.contains("DIS") || l.contains("NEG") || l.contains("MOD") ||
      l.contains("C-") || l.contains("R-") ||
      l == "rel"// || l == "Support"
  }

  import qfirst.datasets.PropBankPredicate

  def isArgRelevant(predIndex: Int, pred: PropBankPredicate, roleLabel: String, argSpan: ESpan) =
    !roleLabelIsIrrelevant(roleLabel) &&
      !Auxiliaries.auxiliaryVerbs.contains(pred.lemma.lowerCase) &&
      !argSpan.contains(predIndex)
}
