package org.qirx.littlespec.runner

import org.qirx.littlespec.Specification
import org.qirx.littlespec.fragments.Result

trait SpecificationRunner {
  def run(specification:Specification):Seq[Result]
}

class DefaultSpecificationRunner extends SpecificationRunner {
  def run(specification: Specification) = specification.executeFragments()
}