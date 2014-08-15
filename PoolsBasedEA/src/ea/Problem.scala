package ea

import ea.entities.ExperimentConfig
import pea.FutureParEA

abstract class Problem extends FutureParEA {

  var config: ExperimentConfig = _
  var Emigrations: Int = _
  var Evaluations: Long = _

  def fitnessFunction(ind: TIndividual): Long

  def qualityFitnessFunction(v: Long): Boolean

  def doWhenQualityFitnessTrue(i: TIndEval)

  private[this] def genIndividual(): TIndividual = {
    val res = new TIndividual()
    for (i <- 1 to config.ChromosomeSize) {
      res += config.rand.nextInt(2).asInstanceOf[Byte]
    }
    res
  }

  def getPop(): TPopulation = {
    val res = new TPopulation()
    for (i <- 1 to config.PopSize) {
      res += genIndividual()
    }
    res
  }

  def runSeqCEvals(): TIndEval = {
    config.ff = fitnessFunction

    Evaluator.config = config
    Reproducer.config = config
    var p2Eval = getPop()
    var indEvals = Evaluator.evaluate(p2Eval.toList)
    indEvals = indEvals.sortWith(_.compareTo(_) > 0)
    Evaluations = indEvals.length
    while (Evaluations < config.Evaluations) {
      p2Eval = Reproducer.reproduce(indEvals.toList)
      indEvals = Evaluator.evaluate(p2Eval.toList)
      indEvals = indEvals.sortWith(_.compareTo(_) > 0)
      Evaluations += indEvals.length
    }
    indEvals(0)
  }

  def runSeqFitnessQuality(): TIndEval = {
    config.setData(fitnessFunction, qualityFitnessFunction, doWhenQualityFitnessTrue)

    Evaluator.config = config
    Reproducer.config = config
    var p2Eval = getPop()
    var indEvals = Evaluator.evaluate(p2Eval.toList)
    indEvals = indEvals.sortWith(_.compareTo(_) > 0)
    Evaluations = indEvals.length
    var solutionReached = false
    val df = config.df
    val solutionReachedFunction = (ind: TIndEval) => {
      df(ind)
      solutionReached = true
    }
    config.df = solutionReachedFunction
    while (!solutionReached) {
      p2Eval = Reproducer.reproduce(indEvals.toList)
      indEvals = Evaluator.evaluate(p2Eval.toList)
      indEvals = indEvals.sortWith(_.compareTo(_) > 0)
      Evaluations += indEvals.length
    }
    indEvals(0)
  }

}
