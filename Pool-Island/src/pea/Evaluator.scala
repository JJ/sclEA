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

object Evaluator {
  def maxOnes(L: String) = (for (l <- L if l == '1') yield l).length
}

import Evaluator._

class Evaluator extends Actor {

  var manager: ActorRef = _
  var profiler: ActorRef = _

  def receive = {

    case ('init, pmanager: ActorRef, pflr: ActorRef) =>
      manager = pmanager
      profiler = pflr

    case ('evaluate, pTable: HashMap[String, (Int, Int)], n: Int) =>
      //      println('evaluate)
      val table = pTable.clone
      val Sels = table.filter((a: (String, (Int, Int))) => a._2._2 == 1).keys.take(n)
      if (Sels.isEmpty) {
        manager ! ('evalEmpthyPool, self)
        //        println('evalEmpthyPool)
      } else {
        val NSels = Sels.map(
          (Ind: String) => {
            val F = maxOnes(Ind)

            if (F == Ind.length)
              manager ! ('solutionReachedbyEvaluator, self)

            (Ind, (F, 2))
          })

        manager ! ('add2Pool_Ind_Fit_State, NSels)
        manager ! ('evalDone, self)
        //        println("mandado evalDone")
      }

    case 'finalize =>
      manager ! ('evaluatorFinalized, self)

  }
}