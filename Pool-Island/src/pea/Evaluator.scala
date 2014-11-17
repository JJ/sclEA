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

import akka.actor.{Actor, ActorRef}
import seqEA.{TIndEval, TIndividual}

import scala.collection.mutable.ArrayBuffer

object Evaluator {

  def evaluate(sels: List[TIndividual]):
  (Boolean, Boolean, List[TIndEval], TIndEval) = {
    if (sels.isEmpty) {
      (false, false, null, null)
    } else {
      var follow = true
      //      var i = 0
      val nSels = new ArrayBuffer[TIndEval]()
      var bSolution = new TIndEval(null, -1)
      val it = sels.iterator
      while (it.hasNext && follow) {
        val ind: TIndividual = it.next()
        val fit: Long = problem.function(ind)
        val nind = new TIndEval(ind, fit)
        if (fit > bSolution._2)
          bSolution = nind
        nSels += nind
        follow = !problem.fitnessTerminationCondition(ind, fit)
      }
      //      if (!follow)
      //        bSolution = nSels.last
      (true, !follow, nSels.toList, bSolution)
    }

  }

}

class Evaluator extends Actor {

  var manager: ActorRef = _
  var profiler: ActorRef = _

  def receive = {

    case ('init, pmanager: ActorRef, pflr: ActorRef) =>
      manager = pmanager
      profiler = pflr

    case ('evaluate, psels: List[TIndividual]) =>
      val (resEval, solFound, nSels, bs) = Evaluator.evaluate(sels = psels)
      if (resEval) {
        manager !('evalDone, self, nSels, bs)
        if (solFound)
          manager !('solutionReachedbyEvaluator, bs, self)
      } else
        manager !('evalEmpthyPool, self)

    case 'finalize =>
      manager !('evaluatorFinalized, self)

    case (a, b) =>
      println(b.getClass.getName)
      println()

  }

}