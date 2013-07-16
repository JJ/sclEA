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
import scala.util.Random
import java.util.Date
import sheduling.ShedulingUtility
import java.util.concurrent.Callable

class PoolManager extends Actor {

  val table = HashMap[String, (Int, Int)]()

  var solutionReached: Boolean = _
  var migrantsDestination: List[ActorRef] = _
  var profiler: ActorRef = _
  var manager: ActorRef = _
  val pmConf = new HashMap[Symbol, Any]()

  var evals: Set[ActorRef] = _
  var reps: Set[ActorRef] = _

  var system: ActorSystem = _

  var poolSize: Int = _

  val r = new Random()

  def log() {
    //    val c1 = table.filter((a: (String, (Int, Int))) => a._2._2 == 1).size
    //    val c2 = table.filter((a: (String, (Int, Int))) => a._2._2 == 2).size
    //    
    //    println("En 1: " + c1)
    //    println("En 2: " + c2)

    //    val cant = table.keys.size
    //    println("T: " + cant)

  }

  def adjustPool(newInds: Set[String]) {

    val cant2Drop = table.size - poolSize

    val alreadyEvaluatedkeys = table.filter((a: (String, (Int, Int))) => a._2._2 == 2 && !newInds.contains(a._1)).keys

    if (alreadyEvaluatedkeys.size >= cant2Drop) {
      val alreadyEvaluated = (alreadyEvaluatedkeys.map(i => (i, table(i)._1))).toList
      
      val alreadyEvaluated2Trim = alreadyEvaluated.sortWith(
        (p1: (String, Int), p2: (String, Int)) => p1._2 < p2._2).take(cant2Drop)
        
      table --= alreadyEvaluated2Trim.map(_._1)
      
    } else {
      val n1 = alreadyEvaluatedkeys.size
      table --= alreadyEvaluatedkeys
      
      val noEvaluatedkeys = table.filter((a: (String, (Int, Int))) => a._2._2 == 1 && !newInds.contains(a._1)).keys
      val noEvaluated2Trim = noEvaluatedkeys.take(cant2Drop - n1)
      
      table --= noEvaluated2Trim
      
      val n2 = table.size - poolSize
      if (n2 > 0) {
        table --= table.keys.take(n2)
      }
      
    }
  }

  def receive = {

    case ('init, conf: HashMap[Symbol, Any]) =>
      pmConf('evaluatorsCount) = conf('evaluatorsCount)
      pmConf('reproducersCount) = conf('reproducersCount)
      pmConf('evaluatorsCapacity) = conf('evaluatorsCapacity)
      pmConf('reproducersCapacity) = conf('reproducersCapacity)

      evals = Set[ActorRef]()
      reps = Set[ActorRef]()
      solutionReached = false

      profiler = conf('profiler).asInstanceOf[ActorRef]
      manager = conf('manager).asInstanceOf[ActorRef]
      system = conf('system).asInstanceOf[ActorSystem]

      table.clear
      table ++= (for (p <- conf('population).asInstanceOf[Iterable[String]]) yield (p, (-1, 1)))
      poolSize = table.size

      evals ++= (for (i <- 1 to conf('evaluatorsCount).asInstanceOf[Int]) yield system.actorOf(Props[Evaluator]))

      reps ++= (for (i <- 1 to conf('reproducersCount).asInstanceOf[Int]) yield system.actorOf(Props[Reproducer]))

      for (a <- evals ++ reps) {
        a ! ('init, self, profiler)
      }

    case ('updatePool, newPool: HashMap[String, (Int, Int)]) =>
      //      println("updatePool")
      table ++= newPool
      adjustPool(newPool.keys.toSet)
    //      log()

    case ('add2Pool_Ind_Fit_State, newPool: Iterable[(String, (Int, Int))]) =>
      //      println("add2Pool_Ind_Fit_State")
      log()
      table ++= newPool
      adjustPool(newPool.map(_._1).toSet)
    //      log()
    //      println("----------------------------------------------------------------------")
    //
    //    case ('add2Pool, individuos: Iterable[(String, Int, Int)]) =>
    //      //      println("add2Pool")
    //      table ++= (for ((i, f, s) <- individuos) yield (i, (f, s)))
    ////      log()

    case ('reproducerFinalized, pid: ActorRef) =>
      if (reps.contains(pid)) {
        reps -= pid
        system.stop(pid)
        self ! 'workerFinalized
      }

    case ('evaluatorFinalized, pid: ActorRef) =>
      if (evals.contains(pid)) {
        evals -= pid
        system.stop(pid)
        self ! 'workerFinalized
      }

    case 'workerFinalized =>
      if (evals.isEmpty && reps.isEmpty) {
        self ! 'finalize
      }

    case ('evolveDone, pid: ActorRef) =>
      if (solutionReached)
        pid ! 'finalize
      //        system.stop(pid)
      else {
        if (r.nextInt() % 2 == 0) {
          pid ! ('emigrateBest, table, migrantsDestination(r.nextInt(migrantsDestination.length)))
        }
        pid ! ('evolve, table, pmConf('reproducersCapacity).asInstanceOf[Int])

      }

    case ('evalDone, pid: ActorRef) =>
      if (solutionReached)
        pid ! 'finalize
      //        system.stop(pid)
      else {
        pid ! ('evaluate, table, pmConf('evaluatorsCapacity).asInstanceOf[Int])
        manager ! ('evalDone, self)
      }

    case 'sReps =>
      for (e <- reps)
        e ! ('evolve, table, pmConf('reproducersCapacity).asInstanceOf[Int])

    case 'sEvals =>
      for (e <- evals)
        e ! ('evaluate, table, pmConf('evaluatorsCapacity).asInstanceOf[Int])

    case 'solutionReachedbyAny =>
      if (!solutionReached) {
        solutionReached = true
        for (e <- reps union evals)
          e ! 'finalize
        //        system.stop(e)
      }

    case ('solutionReachedbyEvaluator, _: ActorRef) =>
      if (!solutionReached) {
        solutionReached = true
        manager ! ('endEvol, new Date().getTime)
        manager ! ('solutionReachedByPoolManager, self)
      }

    case ('evalEmpthyPool, pid: ActorRef) =>
      ShedulingUtility.send_after(50, new Callable[Any] {
        def call() = {
          pid ! ('evaluate, table, pmConf('evaluatorsCapacity).asInstanceOf[Int])
        }
      })

    case ('repEmpthyPool, pid: ActorRef) =>
      ShedulingUtility.send_after(50, new Callable[Any] {
        def call() = {
          pid ! ('evolve, table, pmConf('reproducersCapacity).asInstanceOf[Int])
        }
      })

    case ('migrantsDestination, dests: List[ActorRef]) =>
      migrantsDestination = dests

    case ('migration, (i: String, f: Int)) =>
      table += (i -> (f, 2))

    case ('setPoolsManager, pmanager: ActorRef) =>
      manager = pmanager

    case 'finalize =>
      manager ! ('poolManagerEnd, self)

  }

}