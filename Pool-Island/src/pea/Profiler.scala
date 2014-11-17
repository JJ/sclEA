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

import akka.actor.{ Actor, Props, ActorSystem, ActorRef }
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import Evaluator._

class Profiler extends Actor {

  var conf: mutable.HashMap[Symbol, Any] = _
  var manager: ActorRef = _
  var initEvol: Long = _
  var nIslands: Int = _
  var iterations: ArrayBuffer[(Int, Int, Double)] = _
  var emigrations: ArrayBuffer[Long] = _

  var evolutionDelay: Long = _
  var numberOfEvals: Long = _
  var bestSolution: Long = _

  def receive = {

    case ('init, pManager: ActorRef) =>
      //      println("Profiler started: ")

      manager = pManager
      emigrations = new ArrayBuffer[Long]()
      iterations = new ArrayBuffer[(Int, Int, Double)]

    case ('configuration, nConf: mutable.HashMap[Symbol, Any], nNIslands: Int) =>
      conf = nConf.clone()
      nIslands = nNIslands
      emigrations.clear()
      iterations.clear()

    case ('migration, (_: List[AnyVal], _: Int), t: Long) =>
      emigrations += t

    case ('initEvol, t: Long) =>
      initEvol = t

    case ('iteration, population: Iterable[List[AnyVal]]) =>

    /*      val popEval = population.map(i => problem.function(i))

      iterations += Tuple3(
        popEval.min,
        popEval.max,
        popEval.reduce((a: Int, b: Int) => a + b) / (popEval.size * 1.0))
*/

    case 'experimentEnd =>
      val reportData = mutable.HashMap[Symbol, Any]()
      reportData += ('evolutionDelay -> evolutionDelay)
      reportData += ('numberOfEvals -> numberOfEvals)
      reportData += ('nEmig -> emigrations.length)
      reportData += ('nIslands -> nIslands)
      reportData += ('bestSol -> bestSolution)
      reportData += ('reproducersCount -> conf('reproducersCount))
      reportData += ('evaluatorsCount -> conf('evaluatorsCount))

      reportData += ('evaluatorsCapacity -> conf('evaluatorsCapacity))
      reportData += ('reproducersCapacity -> conf('reproducersCapacity))
//      println("Y los 'reproducersCount: " + conf('reproducersCount))
      manager ! ('experimentEnd, reportData)

    case ('endEvol, t: Long, pNumberOfEvals: Int, pBestSolution: Long) =>
      //val evolutionDelay = (t - initEvol) / 1000.0
      evolutionDelay = t - initEvol
      numberOfEvals = pNumberOfEvals
      bestSolution = pBestSolution

    case 'finalize =>

  }
}