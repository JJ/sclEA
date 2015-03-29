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