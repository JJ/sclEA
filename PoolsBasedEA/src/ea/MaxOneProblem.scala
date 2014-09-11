package ea

import ea.entities.ExperimentConfig

class MaxOneProblem(conf: ExperimentConfig) extends Problem {
  config = conf
  override def fitnessFunction(ind: TIndividual): Long = {
    ind.count(_ == 1)
  }

  override def qualityFitnessFunction(v: Long): Boolean = {
    v > config.ChromosomeSize - 2
  }
}
