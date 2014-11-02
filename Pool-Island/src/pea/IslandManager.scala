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

import java.util.Date

import akka.actor.{Actor, ActorRef, ActorSystem}

import scala.collection.mutable.{ArrayBuffer, HashMap}

class IslandManager extends Actor {

  var pools: Set[ActorRef] = _
  var profiler: ActorRef = _
  var manager: ActorRef = _
  var endEvol: Boolean = _
  var cierre: Boolean = _
  var numberOfEvals: Int = _
//  var solutions: ArrayBuffer[(List[AnyVal], Int)] = _
  var bSolution: (List[AnyVal], Int) = (List(), -1)

  var system: ActorSystem = _

  def receive = {

    case 'start =>
      profiler !('initEvol, new Date().getTime())
      for (p <- pools) {
        // All executing units to work!
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

//      solutions = ArrayBuffer[(List[AnyVal], Int)]()
      cierre = false

    case ('evalDone, pid: ActorRef, n: Int, bs: (List[AnyVal], Int)) =>
      if (pools contains pid) {
        if (bSolution._2 < bs._2) {
          bSolution = bs
        }
        numberOfEvals += n
      }

    case ('poolManagerEnd, pid: ActorRef) =>
      if (pools.contains(pid)) {
        pools -= pid
      }
      if (pools.isEmpty && !cierre) {
        self ! 'finalize
        cierre = true
      }

    case 'deactivate =>
      for (p <- pools)
        p ! 'deactivate

    case ('solutionReached, pid: ActorRef, sol: (List[AnyVal], Int)) =>
      if (pools.contains(pid)) {
        if (!endEvol) {
          profiler !('endEvol, (new Date()).getTime(), numberOfEvals, sol._2)
          endEvol = true
        }
        self ! 'deactivate
      }

    case ('numberOfEvaluationsReached, pid: ActorRef, bs: (List[AnyVal], Int)) =>
      if (pools.contains(pid)) {
//        solutions += bestSol
        if (bSolution._2 < bs._2) {
          bSolution = bs
        }
        pools -= pid
        if (pools.isEmpty) {
          profiler !('endEvol, (new Date()).getTime(), numberOfEvals, bSolution._2)
          endEvol = true
        }
        pid ! 'finalizeAllWorkers
      }

    case 'finalize =>
      profiler ! 'experimentEnd
      system.stop(self)
  }

  //  def bestSolution() =
  //    solutions.reduce((a: (List[AnyVal], Int), b: (List[AnyVal], Int)) =>
  //      if (a._2 < b._2) b else a)

}