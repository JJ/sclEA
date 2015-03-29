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
import ea.alg.seq.GeneralScheme
import ea.selection.{parentSelections, replacementSelections}
import ea.variation.{crossoverImpl, mutationImpl}


class MaxOnesSeqConfiguration extends GeneralScheme {

  val parentsCount = popSize / 2
  val tournamentSize = 3

  def parentsSelection = parentSelections.tournamentSelection(tournamentSize, parentsCount)

  def survivorsSelection = replacementSelections.tournamentReplacement(tournamentSize)

  var bestSolution = 0l

  def terminationCheck(f: Long): Unit = {
    evaluations += 1
    quality = qualityFitness(f)
    if (f > bestSolution)
      bestSolution = f
  }

  def fitness = ind => {
    val res = maxOnes.fitness(ind)
    terminationCheck(res)
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

  def maxNumberOfEvals = 5000

  def popSize = 100

  def chromosomeSize = 8

  def genIndividual(): TInd =
    (for (_ <- 1 to chromosomeSize) yield rand.nextInt(2).asInstanceOf[Byte]).toArray

  def genInitPopulation(): TPopNoEval =
    (for (_ <- 1 to popSize) yield genIndividual()).toArray

}
