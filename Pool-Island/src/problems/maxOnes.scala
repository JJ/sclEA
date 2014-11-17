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

import seqEA.TIndividual

import scala.util.Random

object maxOnes extends protocols.Problem {

  val data = config.GAConfig.loadFromJSON("maxOnesConfig.json")

  val r = new Random()

  //  def terminationCondition: Symbol = Symbol(data.getTerminationCondition())
  //    'fitnessTerminationCondition
  //'cantEvalsTerminationCondition

  def seqOutputFilename = data.getSeqOutputFilename

  def parallelOutputFilename = data.getParallelOutputFilename

  def evaluatorsCount = data.getEvaluatorsCount

  def reproducersCount = data.getReproducersCount

  def evaluatorsCapacity = data.getEvaluatorsCapacity

  def reproducersCapacity = data.getReproducersCapacity

  def popSize = data.getPopSize

  def evaluations = data.getEvaluations

  def repetitions = data.getRepetitions

  def chromosomeSize = data.getChromosomeSize

  def genInd(): TIndividual = {
    val res = new TIndividual()
    for (i <- 1 to chromosomeSize) {
      res += r.nextInt(2).asInstanceOf[Byte]
    }
    res
  }

  def function(ind: TIndividual): Long = ind.count(_ == 1)

  def fitnessTerminationCondition(ind: TIndividual, fit: Long): Boolean = fit > chromosomeSize - 2

  def changeGen(aByte: Byte): Byte = if (aByte == 0) 1.asInstanceOf[Byte] else 0.asInstanceOf[Byte]
}