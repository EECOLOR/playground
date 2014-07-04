package org.qirx.cms.execution

import org.qirx.cms.construction.Branching
import org.qirx.cms.construction.Branched
import org.qirx.cms.machinery.Program
import scala.language.higherKinds 
import org.qirx.cms.machinery.Apply

object BranchingHandler extends (Branching ~> ({ type T[x] = Program[Any, Either[x, Any]]})#T) {

  def transform[x] = {
    case Branched(value) => value
  } 
    
}