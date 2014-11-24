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
import scala.util.Random

//import java.util.Date

import java.util.concurrent.Callable

import akka.event.Logging
import sheduling.ShedulingUtility

object PoolManager {
  var poolSize: Int = _
}

class ActorER(p2Rep: ReproducersPool[TIndEval], reproducersCapacity: Int, p2Eval: EvaluatorsPool[TIndividual]) extends Actor {
  override def receive: Receive = {
    case ('sendItReproduce, target: ActorRef) =>
      target !('evolve, p2Rep.extractElements(reproducersCapacity).toList)
    case ('sendItEvaluate, target: ActorRef, n: Int) =>
      target !('evaluate, p2Eval.extractElements(n).toList)
  }
}

//class ActorR(p2Rep: ReproducersPool[TIndEval], pmConf: mutable.HashMap[Symbol, Any]) extends Actor {
//  override def receive: Receive = {
//    case ('sendItWork, target: ActorRef) =>
//      target !('evolve, p2Rep.extractElements(
//        pmConf('reproducersCapacity).asInstanceOf[Int]).toList)
//  }
//}

//object ActorR {
//  def props(p2Rep: ReproducersPool[TIndEval], pmConf: HashMap[Symbol, Any]): Props = Props(new ActorR(p2Rep, pmConf))
//}

//class ActorE(p2Eval: EvaluatorsPool[TIndividual]) extends Actor {
//  override def receive: Receive = {
//    case ('sendItWork, target: ActorRef, n: Int) =>
//      target !('evaluate, p2Eval.extractElements(n).toList)
//  }
//}

//object ActorE {
//  def props(p2Eval: EvaluatorsPool[TIndividual]): Props = Props(new ActorE(p2Eval))
//}

class PoolManager extends Actor {
  val log = Logging(context.system, this)

  val p2Rep = new ReproducersPool[TIndEval]()
  val p2Eval = new EvaluatorsPool[TIndividual]()

  val pmConf = new mutable.HashMap[Symbol, Any]()

  var active: Boolean = _
  var evals: Set[ActorRef] = _
  var reps: Set[ActorRef] = _
  var evaluations: Int = _

  val r = new Random()
  var addJobs: ActorRef = _
//  var addrJobs: ActorRef = _
//  var addeJobs: ActorRef = _
  var migrantsDestination: List[ActorRef] = _
  var profiler: ActorRef = _
  var manager: ActorRef = _
  var system: ActorSystem = _
  var bSolution: TIndEval = new TIndEval(null, -1)

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

      addJobs = system.actorOf(Props(classOf[ActorER], p2Rep,
        pmConf('reproducersCapacity).asInstanceOf[Int], p2Eval))
      //      addrJobs = system.actorOf(Props(classOf[ActorR], p2Rep, pmConf))
      //      addeJobs = system.actorOf(Props(classOf[ActorE], p2Eval))

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
      //      p2Rep.appendAndRemoveWorstN(List(i), 1)
      profiler !('migration, i._2)

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
      if (ops % 5 == 0) {
        //        println("reproductionDone: " + nInds.size)
      }
      if (active) {
        p2Eval.append(nInds)
        if (r.nextDouble() < 0.001 && bSolution._1 != null) {
          migrantsDestination(r.nextInt(migrantsDestination.length)) !('migration, bSolution)
        }
        addJobs !('sendItReproduce, pid)
      } else {
        pid ! 'finalize
        //        system.stop(pid)
      }

    case ('evalDone, pid: ActorRef, nSels: List[TIndEval], bs: TIndEval) =>
      //      println("evalDone")
      log.debug("evalDone")
      if (active) {
        manager !('evalDone, self, nSels.length, bs) // Report to principal manager
        p2Rep.append(nSels)
        if (bSolution._2 < bs._2) {
          bSolution = bs
        } else {
          p2Rep.appendAndRemoveWorstN(List(bSolution), 1)
        }
        val evaluatorsCapacity: Int = {
          val teval = evaluations - nSels.length
          evaluations = if (teval > 0) teval else 0
          Math.min(evaluations, problem.evaluatorsCapacity)
        }
        logPoolsSolEvals()
        if (evaluatorsCapacity > 0) {
          addJobs !('sendItEvaluate, pid, evaluatorsCapacity)
        } else {
          if (active) {
            manager !('numberOfEvaluationsReached, self, bs)
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
          addJobs !('sendItEvaluate, pid, pmConf('evaluatorsCapacity).asInstanceOf[Int])
        }
      })

    case ('repEmpthyPool, pid: ActorRef) =>
      log.debug("repEmpthyPool")
      val time = 50
      ShedulingUtility.send_after(time, new Callable[Any] {
        def call() = {
          //log.debug ("rep")
          addJobs !('sendItReproduce, pid)
        }
      })

    case 'finalize =>
      log.debug("finalize")
      manager !('poolManagerEnd, self)
      system.stop(self)
  }

  var ops = 0

  def logPoolsSolEvals(): Unit = {
    ops += 1
    if (ops > 20) {
      ops = 0
      println("Pendings evaluations: " + evaluations + ", p2Eval.length: " + p2Eval.length() + ", p2Rep.length: " + p2Rep.length() + ", bestSol: " + bSolution._2)
    }
  }
}