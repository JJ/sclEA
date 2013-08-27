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

package pea

object problem {

  def terminationCondition: Symbol =
    'fitnessTerminationCondition
  //'cantEvalsTerminationCondition

  def problemName =
    //  'maxOne
    'maxSAT

  def function(ind: List[AnyVal]): Int = conf.function(ind)

  def fitnessTerminationCondition(ind: List[AnyVal], fit: Int): Boolean = conf.fitnessTerminationCondition(ind, fit)

  def genInitPop() = conf.genInitPop()

  def evaluatorsCount = conf.evaluatorsCount
  def evaluatorsCapacity = conf.evaluatorsCapacity
  def reproducersCount = conf.reproducersCount
  def reproducersCapacity = conf.reproducersCapacity

  def evaluations = conf.evaluations

  def changeGen(g: Any): Any = conf.changeGen(g)

  var conf: protocols.Problem =
    problemName match {
      case 'maxOne => problems.maxOnes
      case _ => problems.maxSAT
    }

}