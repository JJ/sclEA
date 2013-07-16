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

import java.io._

class Report extends Actor {

  var results: ArrayBuffer[(Double, Int, HashMap[Symbol, Any], Int, Int)] = _
  var numberOfExperiments: Int = _
  var profiler: ActorRef = _
  var instances: ArrayBuffer[(() => Unit, String)] = _
  var system: ActorSystem = _

  def receive = {

    case ('init, pflr: ActorRef, sys: ActorSystem) =>
      //      println("Report started: ")
      system = sys
      profiler = pflr
      instances = new ArrayBuffer[(() => Unit, String)]()
      results = ArrayBuffer[(Double, Int, HashMap[Symbol, Any], Int, Int)]()

    case ('experimentEnd,
      evolutionDelay: Double, nEmig: Int,
      conf: HashMap[Symbol, Any], nIslands: Int,
      numberOfEvals: Int) =>

      results += Tuple5(evolutionDelay, nEmig, conf, nIslands, numberOfEvals)
      numberOfExperiments -= 1
      if (numberOfExperiments == 0) {
        val w = new PrintWriter(new File("results.csv"))
        w.write("EvolutionDelay,NumberOfEvals,Emigrations,EvaluatorsCount,ReproducersCount,IslandsCount\n")

        for (
          (evolutionDelay1, nEmig1,
            conf1, nIslands1,
            numberOfEvals1) <- results
        ) {
          val ec = conf1('evaluatorsCount).asInstanceOf[Int]
          val rc = conf1('reproducersCount).asInstanceOf[Int]
          w.write(s"$evolutionDelay1,$numberOfEvals1,$nEmig1,$ec,$rc,$nIslands1\n")
        }

        w.close()

        system.shutdown()
        sheduling.ShedulingUtility.shutdown()
      }

    case ('session, funs: List[(() => Unit, String)]) =>

      instances ++= funs
      numberOfExperiments = funs.length
      self ! 'mkExperiment

    case 'mkExperiment =>
      if (!instances.isEmpty) {
        val (now, name) = instances.remove(0)
        now()

        println(s"Doing experiment: $name")
      }

  }

}