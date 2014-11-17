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

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import pea.ds.{EvaluatorsPool, ReproducersPool}
import seqEA.{TIndEval, TIndividual}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.util.Random

//import java.util.Date

import java.util.concurrent.Callable

import akka.event.Logging
import sheduling.ShedulingUtility

object PoolManager {
  var poolSize: Int = _
}

class ActorR(p2Rep: ReproducersPool[TIndEval], pmConf: mutable.HashMap[Symbol, Any]) extends Actor {
  override def receive: Receive = {
    case ('sendItWork, target: ActorRef) =>
      target !('evolve, p2Rep.extractElements(
        pmConf('reproducersCapacity).asInstanceOf[Int]).toList)
  }
}

//object ActorR {
//  def props(p2Rep: ReproducersPool[TIndEval], pmConf: HashMap[Symbol, Any]): Props = Props(new ActorR(p2Rep, pmConf))
//}

class ActorE(p2Eval: EvaluatorsPool[TIndividual]) extends Actor {
  override def receive: Receive = {
    case ('sendItWork, target: ActorRef, n: Int) =>
      target !('evaluate, p2Eval.extractElements(n).toList)
  }
}

//object ActorE {
//  def props(p2Eval: EvaluatorsPool[TIndividual]): Props = Props(new ActorE(p2Eval))
//}

class PoolManager extends Actor {
  val log = Logging(context.system, this)

  val p2Rep = new ReproducersPool[TIndEval]()
  val p2Eval = new EvaluatorsPool[TIndividual]()

  val pmConf = new mutable.HashMap[Symbol, Any]()

  var active: Boolean = _
  var migrantsDestination: List[ActorRef] = _
  var profiler: ActorRef = _
  var manager: ActorRef = _
  var system: ActorSystem = _

  var evals: Set[ActorRef] = _
  var reps: Set[ActorRef] = _

  var evaluations: Int = _

  val r = new Random()

  var addrJobs: ActorRef = _

  var addeJobs: ActorRef = _

  //def receive = LoggingReceive {
  def receive = {

    case ('initEvaluations, cant: Int) =>
      evaluations = cant

    case ('init, conf: mutable.HashMap[Symbol, Any]) =>

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

      p2Eval.append(conf('population).asInstanceOf[List[TIndividual]])

      PoolManager.poolSize = p2Eval.length()

      evals ++= (for (i <- 1 to conf('evaluatorsCount).asInstanceOf[Int])
      yield system.actorOf(Props[Evaluator]))

      reps ++= (for (i <- 1 to conf('reproducersCount).asInstanceOf[Int])
      yield system.actorOf(Props[Reproducer]))

      //      val tt = Util.createActorR(system, p2Rep, pmConf)
      //      if (tt == null) {
      //        println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      //        System.exit(0)
      //      }

      addrJobs = system.actorOf(Props(classOf[ActorR], p2Rep, pmConf))
      addeJobs = system.actorOf(Props(classOf[ActorE], p2Eval))

      for (a <- evals ++ reps) {
        a !('init, self, profiler)
      }

    //    case ('updatePool, newPool: HashMap[List[AnyVal], (Int, Int)]) =>
    //      log.debug("updatePool")
    //      p2Eval = newPool
    //
    //    case ('add2Pool, newPool: Iterable[(List[AnyVal], (Int, Int))]) =>
    //      log.debug("add2Pool")
    //      p2Eval ++= newPool

    case ('migrantsDestination, dests: List[ActorRef]) =>
      log.debug("migrantsDestination")
      migrantsDestination = dests

    case ('migration, i: TIndEval) =>
      log.debug("migration")
      p2Rep.append(List(i))
      p2Rep.removeWorstN(1)

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

    case ('solutionReachedbyEvaluator, indEval: TIndEval, pid: ActorRef) =>
      log.debug("solutionReachedbyEvaluator")
      if (active) {
        manager !('solutionReached, self, indEval)
        self ! 'finalize
        active = false
      }

    case ('reproductionDone, pid: ActorRef, nInds: List[TIndividual]) =>
      log.debug("reproductionDone")
      if (active) {
        p2Eval.append(nInds)
        if (r.nextInt() % 2 == 0) {
          // migration
          // TODO: migration
          // migrantsDestination(r.nextInt(migrantsDestination.length)) ! bestSol
        }
        addrJobs !('sendItWork, pid)
      } else {
        pid ! 'finalize
        //        system.stop(pid)
      }

    case ('evalDone, pid: ActorRef, nSels: List[TIndEval], bs: TIndEval) =>
      //      println("evalDone")
      log.debug("evalDone")
      if (active) {
        manager !('evalDone, self, nSels.length, bs) // Report to principal manager
        p2Rep.append(nSels) // TODO: Ver si hay que eliminar alguno...
        val evaluatorsCapacity: Int = {
          val teval = evaluations - nSels.length
          evaluations = if (teval > 0) teval else 0
          Math.min(evaluations, problem.evaluatorsCapacity)
        }
        //        println(evaluations)
        if (evaluatorsCapacity > 0) {
          addeJobs !('sendItWork, pid, evaluatorsCapacity)
        } else {
          if (active) {
            manager !('numberOfEvaluationsReached, self, bs)
            //      self ! 'finalize
            active = false
          }
        }
      } else {
        pid ! 'finalize
        //        system.stop(pid)
      }

    case 'finalizeAllWorkers =>
      for (e <- evals ++ reps)
        e ! 'finalize

    case 'deactivate =>
      log.debug("deactivate")
      active = false
      self ! 'finalizeAllWorkers

    //    case 'evaluationsDone =>
    //      log.debug("evaluationsDone")
    //      evaluationsDone()

    case 'sReps =>
      log.debug("sReps")
      for (e <- reps)
        e !('evolve, p2Rep.extractElements(0).toList)

    case 'sEvals =>
      log.debug("sEvals")
      for (e <- evals) {
        e !('evaluate, p2Eval.extractElements(0).toList)
      }
    case ('evalEmpthyPool, pid: ActorRef) =>
      log.debug("evalEmpthyPool")
      val time = 100
      ShedulingUtility.send_after(time, new Callable[Any] {
        def call() = {
          //log.debug ("eval")
          addeJobs !('sendItWork, pid, pmConf('evaluatorsCapacity).asInstanceOf[Int])
        }
      })

    case ('repEmpthyPool, pid: ActorRef) =>
      log.debug("repEmpthyPool")
      val time = 50
      ShedulingUtility.send_after(time, new Callable[Any] {
        def call() = {
          //log.debug ("rep")
          addrJobs !('sendItWork, pid)
        }
      })

    case 'finalize =>
      log.debug("finalize")
      manager !('poolManagerEnd, self)
      system.stop(self)
  }

  //  def logTable() {
  //
  //    val c1 = table.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 1).size
  //    val c2 = table.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 2).size
  //    val cant = table.keys.size
  //
  //    println("En 1: " + c1)
  //    println("En 2: " + c2)
  //
  //    println("Total: " + cant + "\n")
  //
  //  }

}