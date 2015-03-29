package ea.problem

import ea._
import ea.alg.par.async.futuresPromises.FuturesPScheme
import ea.selection.{parentSelections, replacementSelections}
import ea.variation.{crossoverImpl, mutationImpl}


object maxOnesFuturesPConfiguration extends FuturesPScheme {

  def tournamentSize = 3

  def parentsSelection = parentSelections.tournamentSelection(tournamentSize, blockSize)

  def survAndParentsSelector = replacementSelections.survAndParentsSelector(rand, population, tournamentSize)

  def survivorsSelection = replacementSelections.tournamentReplacement(tournamentSize)

  def terminationCheck(ind: TInd, f: Long): Unit = {
    evaluations += 1
    quality = qualityFitness(f)
    if (f > bestSolution._2)
      bestSolution = new TIndEval(ind, f)
  }

  var cant = 0

  def fitness = ind => {
    val res = maxOnes.fitness(ind)
    terminationCheck(ind, res)
    //    println(evaluations)
    res
  }

  def crossover = crossoverImpl.twoPoint

  val pMutation = 0.1

  def mutation = mutationImpl.mutation(pMutation)

  def qualityFitness = fit => fit == chromosomeSize

  var quality = false

  def terminationCondition = () =>
    quality || evaluations >= maxNumberOfEvals

  var evaluations = 0

  def maxNumberOfEvals = 500000

  def popSize = 1000

  def chromosomeSize = 90

  def genIndividual(): TInd =
    (for (_ <- 1 to chromosomeSize) yield rand.nextInt(2).asInstanceOf[Byte]).toArray

  def genInitPopulation(): TPopNoEval =
    (for (_ <- 1 to popSize) yield genIndividual()).toArray

}