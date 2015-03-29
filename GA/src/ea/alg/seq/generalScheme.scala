package ea.alg.seq

import ea._

trait GeneralScheme extends Algorithm with Variation with Selection with Evaluation {

  val evaluator = (ind: TInd) => new TIndEval(ind, fitness(ind))

  def evolve(): Unit = {
    val initPopulation = genInitPopulation() // initilise()
    var population = initPopulation.map(evaluator) // evalPopulation()
    do {
      val parents = parentsSelection(population) // parentSelection()
      val newInds = parents.map(crossover).flatten // recombinePairsOfParents()
      val newIndsMutated = newInds.map(mutation) // mutateOffspring()
      val newIndsEvaluated = newIndsMutated.map(evaluator) // evalNewPopulation()
      population = survivorsSelection(population, newIndsEvaluated) // selectIndividualsForTheNextGeneration()
    } while (!terminationCondition())
  }

}