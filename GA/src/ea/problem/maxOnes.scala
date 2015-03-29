package ea.problem

import ea._

object maxOnes {
  def fitness: TFitness = (ind: TInd) => ind.count(_ == 1)
}