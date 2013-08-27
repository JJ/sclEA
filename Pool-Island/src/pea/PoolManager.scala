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

import akka.event.LoggingReceive

import akka.event.Logging

object PoolManager {

  var poolSize: Int = _

}

class PoolManager extends Actor {

  val log = Logging(context.system, this)

  var table: HashMap[List[AnyVal], (Int, Int)] = _
  val pmConf = new HashMap[Symbol, Any]()

  var active: Boolean = _
  var migrantsDestination: List[ActorRef] = _
  var profiler: ActorRef = _
  var manager: ActorRef = _
  var system: ActorSystem = _

  var evals: Set[ActorRef] = _
  var reps: Set[ActorRef] = _

  var evaluations: Int = _

  val r = new Random()

  //def receive = LoggingReceive {
  def receive = {

    case ('initEvaluations, cant: Int) =>
      evaluations = cant

    case ('init, conf: HashMap[Symbol, Any]) =>

      log.debug("init")

      pmConf('evaluatorsCount) = conf('evaluatorsCount)
      pmConf('reproducersCount) = conf('reproducersCount)
      pmConf('evaluatorsCapacity) = conf('evaluatorsCapacity)
      pmConf('reproducersCapacity) = conf('reproducersCapacity)

      evals = Set[ActorRef]()
      reps = Set[ActorRef]()
      active = true

      profiler = conf('profiler).asInstanceOf[ActorRef]
      manager = conf('manager).asInstanceOf[ActorRef]
      system = conf('system).asInstanceOf[ActorSystem]

      table = HashMap[List[AnyVal], (Int, Int)]()
      table ++= (for (p <- conf('population).asInstanceOf[Iterable[List[AnyVal]]]) yield (p, (-1, 1)))
      PoolManager.poolSize = table.size

      evals ++= (for (i <- 1 to conf('evaluatorsCount).asInstanceOf[Int])
        yield system.actorOf(Props[Evaluator]))

      reps ++= (for (i <- 1 to conf('reproducersCount).asInstanceOf[Int])
        yield system.actorOf(Props[Reproducer]))

      for (a <- evals ++ reps) {
        a ! ('init, self, profiler)
      }

    case ('updatePool, newPool: HashMap[List[AnyVal], (Int, Int)]) =>
      log.debug("updatePool")
      table = newPool

    case ('add2Pool, newPool: Iterable[(List[AnyVal], (Int, Int))]) =>
      log.debug("add2Pool")
      table ++= newPool

    case ('migrantsDestination, dests: List[ActorRef]) =>
      log.debug("migrantsDestination")
      migrantsDestination = dests

    case ('migration, (i: List[AnyVal], f: Int)) =>
      log.debug("migration")
      table += (i -> (f, 2))

    case ('evaluatorFinalized, pid: ActorRef) =>
      if (evals contains pid) {
        log.debug("evaluatorFinalized")
        evals -= pid
        system.stop(pid)
        self ! 'workerFinalized
      }

    case ('reproducerFinalized, pid: ActorRef) =>
      if (reps contains pid) {
        log.debug("reproducerFinalized")
        reps -= pid
        system.stop(pid)
        self ! 'workerFinalized
      }

    case 'workerFinalized =>
      if (evals.isEmpty && reps.isEmpty) {
        log.debug("finalized")
        self ! 'finalize
      }

    case ('evolveDone, pid: ActorRef) =>
      log.debug("evolveDone")
      if (active) {
        if (r.nextInt() % 2 == 0) {
          pid ! ('emigrateBest, table, migrantsDestination(r.nextInt(migrantsDestination.length)))
        }
        pid ! ('evolve, table, pmConf('reproducersCapacity).asInstanceOf[Int])
      } else {
        pid ! 'finalize
        //        system.stop(pid)
      }

    case ('evalDone, pid: ActorRef, n: Int) =>
      log.debug("evalDone")
      if (active) {

        manager ! ('evalDone, self, n)

        val evaluatorsCapacity = problem.terminationCondition match {
          case 'fitnessTerminationCondition => pmConf('evaluatorsCapacity).asInstanceOf[Int]
          case _ =>
            evaluations -= n
            Math.min(evaluations, pmConf('evaluatorsCapacity).asInstanceOf[Int])
        }

        if (evaluatorsCapacity > 0) {
          pid ! ('evaluate, table, evaluatorsCapacity)
        } else {
          evaluationsDone()
        }

      } else {
        pid ! 'finalize
        //        system.stop(pid)
      }

    case 'sReps =>
      log.debug("sReps")
      for (e <- reps)
        e ! ('evolve, table, 0)

    case 'sEvals =>
      log.debug("sEvals")
      for (e <- evals)
        e ! ('evaluate, table, 0)

    case 'deactivate =>
      log.debug("deactivate")
      active = false

    case ('solutionReachedbyEvaluator, (ind: List[AnyVal], fit: Int), pid: ActorRef) =>
      log.debug("solutionReachedbyEvaluator")
      if (active) {
        manager ! ('solutionReached, self, (ind, fit))
        self ! 'finalize
        active = false
      }

    case 'evaluationsDone =>
      log.debug("evaluationsDone")
      evaluationsDone()

    case ('evalEmpthyPool, pid: ActorRef) =>
      log.debug("evalEmpthyPool")
      val time = 100
      ShedulingUtility.send_after(time, new Callable[Any] {
        def call() = {
          //log.debug ("eval")
          pid ! ('evaluate, table, pmConf('evaluatorsCapacity).asInstanceOf[Int])
        }
      })

    case ('repEmpthyPool, pid: ActorRef) =>
      log.debug("repEmpthyPool")
      val time = 50
      ShedulingUtility.send_after(time, new Callable[Any] {
        def call() = {
          //log.debug ("rep")
          pid ! ('evolve, table, pmConf('reproducersCapacity).asInstanceOf[Int])
        }
      })

    case 'finalize =>
      log.debug("finalize")
      manager ! ('poolManagerEnd, self)

  }

  def evaluationsDone() {
    // (send (.manager self) islandManager/numberOfEvaluationsReached *agent* bestSol)
    if (active) {
      manager ! ('numberOfEvaluationsReached, self, bestSolution())
      self ! 'finalize
      active = false
    }

    for (e <- evals ++ reps)
      e ! 'finalize

  }

  def bestSolution(): (AnyRef, Int) = {
    val evals = table.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 2).toList
    if (evals.isEmpty) (null, -1)
    else {
      val red = evals.reduce(
        (a: (List[AnyVal], (Int, Int)), b: (List[AnyVal], (Int, Int))) =>
          if (a._2._1 < b._2._1) b else a)

      (red._1, red._2._1)
    }

  }

  def logTable() {

    val c1 = table.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 1).size
    val c2 = table.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 2).size
    val cant = table.keys.size

    println("En 1: " + c1)
    println("En 2: " + c2)

    println("Total: " + cant + "\n")

  }

}