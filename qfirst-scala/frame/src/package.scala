package qfirst

import qfirst.ClauseResolution.ArgStructure
import qasrl.ArgumentSlot

package object paraphrase extends qfirst.paraphrase.PackagePlatformExtensions {
  def getArgumentSlotsForClauseTemplate(clauseTemplate: ArgStructure): Set[ArgumentSlot] = {
    (clauseTemplate.args.keys.toList: List[ArgumentSlot]).filter {
      case qasrl.Obj2 => clauseTemplate.args.get(qasrl.Obj2) match {
        case Some(qasrl.Prep(_, None)) => false
        case _ => true
      }
      case _ => true
    }.toSet
  }
}
