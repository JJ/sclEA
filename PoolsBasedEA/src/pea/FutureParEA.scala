package pea

import ea.{Problem, TIndEval}

trait FutureParEA {
  this: Problem =>

  def runParCEvals(): TIndEval = {
    new TIndEval(null, config.rand.nextInt(45))
  }
}
