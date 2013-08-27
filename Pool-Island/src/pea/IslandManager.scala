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
import scala.collection.mutable.ArrayBuffer

class IslandManager extends Actor {

  var pools: Set[ActorRef] = _
  var profiler: ActorRef = _
  var manager: ActorRef = _
  var endEvol: Boolean = _
  var numberOfEvals: Int = _
  var system: ActorSystem = _

  var solutions: ArrayBuffer[(List[AnyVal], Int)] = _

  def receive = {

    case 'start =>
      profiler ! ('initEvol, new Date().getTime())

      for (p <- pools) { // All executing units to work!
        p ! 'sReps
        p ! 'sEvals
      }

    case ('init, conf: HashMap[Symbol, Any]) =>
      pools = conf('pools).asInstanceOf[Set[ActorRef]]
      profiler = conf('profiler).asInstanceOf[ActorRef]
      manager = conf('manager).asInstanceOf[ActorRef]
      system = conf('system).asInstanceOf[ActorSystem]

      endEvol = false
      numberOfEvals = 0

      solutions = ArrayBuffer[(List[AnyVal], Int)]()

    case ('evalDone, pid: ActorRef, n: Int) =>
      if (pools contains pid) {
        numberOfEvals += n
      }

    case ('poolManagerEnd, pid: ActorRef) =>

      if (pools.contains(pid)) {
        pools -= pid
        system.stop(pid)
      }

      if (pools.isEmpty) {
        self ! 'finalize
      }

    case ('solutionReached, _: ActorRef, sol: (List[AnyVal], Int)) =>
      if (!endEvol) {
        profiler ! ('endEvol, (new Date()).getTime(), numberOfEvals, sol._2)
        endEvol = true
      }

      for (p <- pools)
        p ! 'deactivate
      self ! 'finalize

    case ('numberOfEvaluationsReached, pid: ActorRef, bestSol: (List[AnyVal], Int)) =>
      solutions += bestSol
      pools -= pid
      if (pools.isEmpty) {
        profiler ! ('endEvol, (new Date()).getTime(), numberOfEvals, bestSolution()._2)
        endEvol = true
        pid ! 'finalize
      }

    case 'finalize =>
      system.stop(self)
  }

  def bestSolution() =
    solutions.reduce((a: (List[AnyVal], Int), b: (List[AnyVal], Int)) =>
      if (a._2 < b._2) b else a)

}