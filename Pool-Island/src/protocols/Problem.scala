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

package protocols

import seqEA.TIndividual

trait Problem {

//  def terminationCondition: Symbol
  def evaluatorsCount: Int
  def evaluatorsCapacity: Int
  def reproducersCount: Int
  def reproducersCapacity: Int

  def popSize: Int
  def chromosomeSize: Int

  def evaluations: Int

  def seqOutputFilename: String
  def parallelOutputFilename: String
  
  def function(ind: TIndividual): Long
  def fitnessTerminationCondition(ind: TIndividual, fit: Long): Boolean
  def genInd(): TIndividual

  def genInitPop() =
    for (i <- 1 to popSize) yield genInd()

  def changeGen(g: Byte): Byte
  
  def repetitions: Int

}