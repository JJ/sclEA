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
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import Evaluator._

class Profiler extends Actor {

  var conf: HashMap[Symbol, Any] = _
  var report: ActorRef = _
  
  var initEvol: Long = _
  var nIslands: Int = _
  var iterations: ArrayBuffer[(Int, Int, Double)] = _
  var emigrations: ArrayBuffer[Long] = _

  def receive = {

    case ('init, rprt: ActorRef) =>
//      println("Profiler started: ")
      
      report = rprt
      emigrations = new ArrayBuffer[Long]()
      iterations = new ArrayBuffer[(Int, Int, Double)]

    case ('configuration, nConf: HashMap[Symbol, Any], nNIslands: Int) =>
      conf = nConf
      nIslands = nNIslands
      emigrations.clear()
      iterations.clear()

    case ('migration, (i: String, f: Int), t: Long) =>
      emigrations += t

    case ('iteration, population: Iterable[String]) =>
      val fitnessEachInd = population.map(i => maxOnes(i))
      val min = fitnessEachInd.min
      val max = fitnessEachInd.max
      val ave = fitnessEachInd.foldLeft(0)((a: Int, b: Int) => a + b) / fitnessEachInd.size
      iterations += Tuple3(min, max, ave)

    case ('initEvol, t: Long) =>
      initEvol = t

    case ('endEvol, t: Long, numberOfEvals: Int) =>
      val evolutionDelay = (t - initEvol) / 1000.0
      report ! ('experimentEnd, evolutionDelay, emigrations.length, conf, nIslands, numberOfEvals)

    case 'finalize =>
  }
}