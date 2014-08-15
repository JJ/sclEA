package ea

import ea.entities.ExperimentConfig

class MaxOneProblem(config: ExperimentConfig) extends Problem(config) {

  override def fitnessFunction(ind: TIndividual): Long = {
    ind.count(_ == 1)
  }

  override def qualityFitnessFunction(v: Long): Boolean = {
    v > config.ChromosomeSize - 2
  }

  override def doWhenQualityFitnessTrue(i: TIndEval) {
  }
}
