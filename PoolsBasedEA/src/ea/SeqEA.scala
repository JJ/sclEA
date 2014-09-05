package ea

trait SeqEA {
  this: Problem =>

  def runSeqCEvals(): TIndEval = {
    config.ff = fitnessFunction
    Evaluator.config = config
    Reproducer.config = config

    var bestSolution = new TIndEval(null, -1)
    var p2Eval = getPop()
    Evaluations = 0
    while (Evaluations < config.Evaluations) {
      var indEvals = Evaluator.evaluate(p2Eval.toList)
      Evaluations += indEvals.length
      indEvals = indEvals.sortWith(_.compareTo(_) > 0)
      p2Eval = Reproducer.reproduce(indEvals.toList)
      if (bestSolution._2 < indEvals(0)._2) {
        bestSolution = indEvals(0)
      }
      bestSolution = indEvals(0)
    }
    bestSolution
  }

}
