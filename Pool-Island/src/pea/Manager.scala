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
import java.util.Date

class Manager extends Actor {

  var results: ArrayBuffer[HashMap[Symbol, Any]] = _
  var profiler: ActorRef = _
  var instances: ArrayBuffer[(() => Unit, String)] = _
  var system: ActorSystem = _

  def receive = {

    case ('init, pflr: ActorRef, sys: ActorSystem) =>
      //      println("Report started: ")
      profiler = pflr
      system = sys
      instances = new ArrayBuffer[(() => Unit, String)]()
      results = ArrayBuffer[HashMap[Symbol, Any]]()

    case ('experimentEnd,
      reportData: HashMap[Symbol, Any]) =>
      // (println (format "Best fitness: %1d at %2d" (nth reportData 5) (.getTime (Date.))))
      //      println("Best fitness: " + reportData('bestSol).asInstanceOf[Int] + " at " + new Date().getTime())
      results += reportData
      if (instances.isEmpty) {
        println("All ends!")

        val w = new PrintWriter(new File(problem.parallelOutputFilename))
        w.write("EvolutionDelay,Evaluations,Emigrations,EvaluatorsCount,ReproducersCount,IslandsCount,BestSol\n")

        for (r <- results) {
          val ec = r.asInstanceOf[HashMap[Symbol, Any]]('conf).asInstanceOf[HashMap[Symbol, Any]]('evaluatorsCount).asInstanceOf[Int]
          val rc = r.asInstanceOf[HashMap[Symbol, Any]]('conf).asInstanceOf[HashMap[Symbol, Any]]('reproducersCount).asInstanceOf[Int]
          val evolutionDelay = r.asInstanceOf[HashMap[Symbol, Any]]('evolutionDelay).asInstanceOf[Long]
          val numberOfEvals = r.asInstanceOf[HashMap[Symbol, Any]]('numberOfEvals).asInstanceOf[Int]
          val nEmig = r.asInstanceOf[HashMap[Symbol, Any]]('nEmig).asInstanceOf[Int]
          val nIslands = r.asInstanceOf[HashMap[Symbol, Any]]('nIslands).asInstanceOf[Int]
          val bestSol = r.asInstanceOf[HashMap[Symbol, Any]]('bestSol).asInstanceOf[Int]

          w.write(s"$evolutionDelay,$numberOfEvals,$nEmig,$ec,$rc,$nIslands,$bestSol\n")
        }
        w.close()

        system.shutdown()
        sheduling.ShedulingUtility.shutdown()

      } else
        self ! 'mkExperiment

    case 'mkExperiment =>
      if (!instances.isEmpty) {

        val (now, name) = instances.remove(0)

        val tt = new Date().getTime()

        println(s"Doing experiment: $name (time -> $tt)")

        now()

      }

    case ('session, funs: List[(() => Unit, String)]) =>

      instances ++= funs
      self ! 'mkExperiment

  }

}