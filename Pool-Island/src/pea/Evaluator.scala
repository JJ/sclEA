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

import akka.actor.{ Actor, ActorRef }
import scala.collection.mutable.HashMap
import problems._

object Evaluator {

  def evaluate(sels: Iterable[List[AnyVal]],
    doIfFitnessTerminationCondition: (List[AnyVal], Int) => Unit): (Boolean, Iterable[(List[AnyVal], Int)]) = {

    if (sels.isEmpty) {
      (false, null)
    } else {
      val nSels = sels.map(

        (ind: List[AnyVal]) => {
          val fit = problem.function(ind)
          if (problem.terminationCondition == 'fitnessTerminationCondition) {
            if (problem.fitnessTerminationCondition(ind, fit)) {
              doIfFitnessTerminationCondition(ind, fit)
            }
          }

          (ind, fit)

        })

      (true, nSels)
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

    case ('evaluate, pTable: HashMap[List[AnyVal], (Int, Int)], n: Int) =>
      //      println('evaluate)
      val table = pTable.clone

      val (res, nSels) = Evaluator.evaluate(
        sels = table.filter(
          (a: (List[AnyVal], (Int, Int))) => a._2._2 == 1).keys.take(n),
        doIfFitnessTerminationCondition = (ind: List[AnyVal], fit: Int) => {
          manager ! ('solutionReachedbyEvaluator, (ind, fit), self)
        })

      if (res) {
        val pnSels = nSels.map(
          (p: (List[AnyVal], Int)) => (p._1, (p._2, 2)))

        manager ! ('add2Pool, pnSels)
        manager ! ('evalDone, self, pnSels.size)
      } else
        manager ! ('evalEmpthyPool, self)

    case 'finalize =>
      manager ! ('evaluatorFinalized, self)

  }

}