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

import java.util.Date

class Manager extends Actor {

  var pools: Set[ActorRef] = _
  var profiler: ActorRef = _
  var report: ActorRef = _
  var endEvol: Boolean = _
  var numberOfEvals: Int = _
  var system: ActorSystem = _

  def receive = {

    case ('init, conf: HashMap[Symbol, Any]) =>

      pools = conf('pools).asInstanceOf[Set[ActorRef]]
      profiler = conf('profiler).asInstanceOf[ActorRef]
      report = conf('report).asInstanceOf[ActorRef]
      system = conf('system).asInstanceOf[ActorSystem]

      profiler ! ('initEvol, new Date().getTime)
      endEvol = false
      numberOfEvals = 0

      for (p <- pools) { // All executing units to work!
        p ! ('setPoolsManager, self)
        p ! 'sReps
        p ! 'sEvals
      }

    case ('solutionReachedByPoolManager, _: ActorRef) =>
      for (p <- pools)
        p ! 'solutionReachedbyAny

    case ('endEvol, t: Long) =>
      if (!endEvol) {
        endEvol = true
        profiler ! ('endEvol, t, numberOfEvals)
      }

    case ('evalDone, _) =>
      numberOfEvals += 1

    case ('poolManagerEnd, pid: ActorRef) =>
      if (pools.contains(pid)) {
        pools -= pid
        system.stop(pid)
      }

      if (pools.isEmpty) {
        self ! 'finalize
      }

    case 'finalize =>
//      println("Manager finalized!")
      report ! 'mkExperiment
  }

}