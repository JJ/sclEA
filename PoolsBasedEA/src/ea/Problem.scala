package ea

import ea.entities.ExperimentConfig

abstract class Problem {

  var config: ExperimentConfig = _
  var Evaluations: Long = _

  def fitnessFunction(ind: TIndividual): Long

  def qualityFitnessFunction(v: Long): Boolean

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

}
