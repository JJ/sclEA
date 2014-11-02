/**
 * Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
 * Copyright 2013 by José Albert Cruz Almaguer.
 *
 * This program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http:www.gnu.org/licenses/agpl-3.0.txt) for more details.
 */

package problems

import scala.util.Random

object maxOnes extends protocols.Problem {

  val data = config.GAConfig.loadFromJSON("maxOnesConfig.json")
  
  val r = new Random()

//  def terminationCondition: Symbol = Symbol(data.getTerminationCondition())
  //    'fitnessTerminationCondition
  //'cantEvalsTerminationCondition

  def seqOutputFilename = data.getSeqOutputFilename()
  def parallelOutputFilename = data.getParallelOutputFilename()

  def evaluatorsCount = data.getEvaluatorsCount()
  def reproducersCount = data.getReproducersCount()

  def evaluatorsCapacity = data.getEvaluatorsCapacity()
  def reproducersCapacity = data.getReproducersCapacity()

  def popSize = data.getPopSize()

  def evaluations = data.getEvaluations()
  
  def repetitions = data.getRepetitions()

  def chromosomeSize = data.getChromosomeSize()

  def genInd(): List[AnyVal] =
    (for (i <- 1 to chromosomeSize) yield r.nextInt(2)).mkString.toList

  def function(ind: List[AnyVal]) = (for (e <- ind if e == '1') yield 1).size

  def fitnessTerminationCondition(ind: List[AnyVal], fit: Int): Boolean = ind.length - fit < 25

  def changeGen(g: Any): Any = if (g == '1') '0' else '1'

}