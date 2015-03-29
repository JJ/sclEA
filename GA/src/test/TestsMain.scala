package test

import org.scalatest.run

object TestsMain extends App {
  run(new VariationSpec())
  println()
  run(new SelectionSpec())
}