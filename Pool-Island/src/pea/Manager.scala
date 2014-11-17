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

import akka.actor.{Actor, ActorRef, ActorSystem}
import com.google.gson.Gson
import ea.entities.HybridParRes

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}

class Manager extends Actor {

  var results: ArrayBuffer[mutable.HashMap[Symbol, Any]] = _
  var profiler: ActorRef = _
  var instances: ArrayBuffer[(() => Unit, String)] = _
  var system: ActorSystem = _

  def receive = {

    case ('init, pflr: ActorRef, sys: ActorSystem) =>
      profiler = pflr
      system = sys

    case ('experimentEnd,
    reportData: mutable.HashMap[Symbol, Any]) =>
      //      println("All ends!")
      val res = new HybridParRes()
      val ec = reportData.asInstanceOf[mutable.HashMap[Symbol, Any]]('evaluatorsCount).asInstanceOf[Int]
      res.setEvaluatorsCount(ec)
      val rc = reportData.asInstanceOf[mutable.HashMap[Symbol, Any]]('reproducersCount).asInstanceOf[Int]
      res.setReproducersCount(rc)
      val evolutionDelay = reportData.asInstanceOf[mutable.HashMap[Symbol, Any]]('evolutionDelay).asInstanceOf[Long]
      res.setEvolutionDelay(evolutionDelay)
      val numberOfEvals = reportData.asInstanceOf[mutable.HashMap[Symbol, Any]]('numberOfEvals).asInstanceOf[Long]
      res.setNumberOfEvals(numberOfEvals)
      val nEmig = reportData.asInstanceOf[mutable.HashMap[Symbol, Any]]('nEmig).asInstanceOf[Int]
      res.setEmigrations(nEmig)
      val nIslands = reportData.asInstanceOf[mutable.HashMap[Symbol, Any]]('nIslands).asInstanceOf[Int]
      res.setNumberOfIslands(nIslands)
      val bestSol = reportData.asInstanceOf[mutable.HashMap[Symbol, Any]]('bestSol).asInstanceOf[Long]
      res.setBestSol(bestSol)
      val eCap = reportData.asInstanceOf[mutable.HashMap[Symbol, Any]]('evaluatorsCapacity).asInstanceOf[Int]
      res.setEvaluatorsCapacity(eCap)
      val rCap = reportData.asInstanceOf[mutable.HashMap[Symbol, Any]]('reproducersCapacity).asInstanceOf[Int]
      res.setReproducersCapacity(rCap)

      val g = new Gson()
      println(g.toJson(res))

      system.shutdown()
      sheduling.ShedulingUtility.shutdown()

    case ('mkExperiment, (app: Function0[Unit], name: String)) =>
      app()

  }

}