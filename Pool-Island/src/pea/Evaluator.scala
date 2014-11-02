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

import scala.collection.mutable.{ArrayBuffer, HashMap}

object Evaluator {


  def evaluate(sels: Iterable[List[AnyVal]]): (Boolean, Boolean, Iterable[(List[AnyVal], Int)], (List[AnyVal], Int)) = {
    if (sels.isEmpty) {
      (false, false, null, null)
    } else {
      var follow = true
      var i = 0
      val nSels = new ArrayBuffer[(List[AnyVal], Int)]()
      var bSolution: (List[AnyVal], Int) = (List(), -1)
      val it = sels.iterator
      while (it.hasNext && follow) {
        val ind: List[AnyVal] = it.next
        val fit: Int = problem.function(ind)
        if (fit > bSolution._2) bSolution = (ind, fit)
        val tt: (List[AnyVal], Int) = (ind, fit)
        nSels += tt
        follow = !problem.fitnessTerminationCondition(ind, fit)
      }
      //      if (!follow)
      //        bSolution = nSels.last
      (true, !follow, nSels, bSolution)
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

      val (resEval, solFound, nSels, bs) = Evaluator.evaluate(
        sels = table.filter(
          (a: (List[AnyVal], (Int, Int))) => a._2._2 == 1).keys.take(n)
        //        ,
        //        doIfFitnessTerminationCondition = (ind: List[AnyVal], fit: Int) => {
        //          manager !('solutionReachedbyEvaluator, (ind, fit), self)
        //        }
      )

      if (resEval) {
        val pnSels = nSels.map(
          (p: (List[AnyVal], Int)) => (p._1, (p._2, 2)))
        manager !('evalDone, self, pnSels.size, bs)
        if (solFound)
          manager !('solutionReachedbyEvaluator, bs, self)
        else
          manager !('add2Pool, pnSels)

      } else
        manager !('evalEmpthyPool, self)

    case 'finalize =>
      manager !('evaluatorFinalized, self)

  }

}

//
//
//object Evaluator {
//
//  def takeAndMapUntilIncluding[T, T1](list: List[T],
//                                      f: T => (Boolean, T1),
//                                      accu: List[T1]): (Boolean, List[T1]) = list match {
//    case List() => (false, accu)
//    case h :: rest =>
//      val (follow, resPred) = f(h)
//      if (!follow) takeAndMapUntilIncluding(rest, f, resPred :: accu)
//      else (true, resPred :: accu)
//  }
//
//  def evaluate(sels: Iterable[List[AnyVal]]): (Boolean, Boolean, Iterable[(List[AnyVal], Int)], (List[AnyVal], Int)) = {
//    if (sels.isEmpty) {
//      (false, false, null, null)
//    } else {
//      val (success, nSels) = takeAndMapUntilIncluding[List[AnyVal], (List[AnyVal], Int)](sels.toList, ind => {
//        val fit = problem.function(ind)
//        val endF = problem.fitnessTerminationCondition(ind, fit)
//        (endF, (ind, fit))
//      }, List())
//      val bSolution = if (success) nSels.head
//      else nSels.reduce((a: (List[AnyVal], Int), b: (List[AnyVal], Int)) =>
//        if (a._2 < b._2) b else a)
//      (true, success, nSels, bSolution)
//    }
//
//  }
//
//}
