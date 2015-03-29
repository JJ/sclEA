/**
 * Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
 * Copyright 2015 by José Albert Cruz Almaguer.
 *
 * This program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http:www.gnu.org/licenses/agpl-3.0.txt) for more details.
 */

package ea.problem

import ea._
import ea.alg.par.CSPModel
import ea.selection.{parentSelections, replacementSelections}
import ea.variation.{crossoverImpl, mutationImpl}

trait MaxOnesParallelConfiguration extends CSPModel {

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
