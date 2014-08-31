package pea

import java.util.Comparator
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, ActorRef}
import ea._
import pea.ds.{EvaluatorsPool, ReproducersPool}

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContextExecutor, Future, Promise}

class PoolManagerCEvals(problem: Problem, cEvaluations: Int, manager: ActorRef, eContext: ExecutionContextExecutor) extends Actor {

  private[this] implicit val executionContext = eContext

  var migrantsDestiny: ActorRef = _
  var Evaluations: Int = _
  var Emigrations: Int = _

  var p2Rep: ReproducersPool[TIndEval] = _
  var p2Eval: EvaluatorsPool[TIndividual] = _

  override def receive: Receive = {
    case ('migrantsDestiny, mDestiny: ActorRef) =>
      migrantsDestiny = mDestiny

    case ('migrate, newMigrant: TIndEval) =>
      p2Rep.append(List(newMigrant))
      p2Rep.removeWorstN(1)
      Emigrations += 1

    case 'start =>
      p2Rep = new ReproducersPool[TIndEval]()
      p2Eval = new EvaluatorsPool[TIndividual](problem.getPop())
      var bestSolution = new TIndEval(null, -1)
      val pResultObtained = Promise[Unit]()
      pResultObtained.future.onSuccess({
        case _ => manager !('resultObtained, bestSolution, Evaluations, Emigrations)
      })
      Evaluations = 0
      val nFutures = new AtomicInteger(0)
      def mkFuture[T](inds2Eval: List[T], // input of the function to be excecuted by the actual future
                      feeder: => List[T], // producer of the input for the new future created at onSuccess of the actual future,
                      // needed because it is responsability of the central excecution, it'snt of any future.
                      beginAction: List[T] => Any, // function to be excecuted by the actual future
                      endAction: (Any) => Unit, // function to be excecuted when the actual future ends
                      cond: => Boolean // function that control the repetition of the algorithm.
                       ): Future[Any] = {
        val res = Future {
          beginAction(inds2Eval)
        }
        nFutures.incrementAndGet()
        res.onSuccess {
          case eResult: Any =>
            endAction(eResult)
            val nVal = nFutures.decrementAndGet()
            if (cond) {
              val ninds2Eval = feeder
              mkFuture(ninds2Eval, feeder, beginAction, endAction, cond)
            }
            else {
              pResultObtained.trySuccess()
              if (nVal == 0) {
                manager ! 'allFuturesFinished
              }
            }
        }
        res
      }

      for (i <- 1 to problem.config.EvaluatorsCount) {
        val inds2Eval1: List[TIndividual] = p2Eval.extractElements(problem.config.EvaluatorsCapacity).toList
        mkFuture[TIndividual](inds2Eval1, p2Eval.extractElements(problem.config.EvaluatorsCapacity).toList,
          (inds2Eval: List[TIndividual]) => {
            val evals = Evaluator.evaluate(inds2Eval.toList)
            evals.sortWith(_.compareTo(_) > 0)
          }, (r1: Any) => {
            val eResult = r1.asInstanceOf[ArrayBuffer[TIndEval]]
            if (eResult.length > 0) {
              p2Rep.append(eResult)
              Evaluations += eResult.length
              if (bestSolution._2 < eResult(0)._2) {
                bestSolution = eResult(0)
              }
            }
          }, Evaluations < cEvaluations)
      }

      for (i <- 1 to problem.config.ReproducersCount) {
        val iEvals1 = p2Rep.extractElements(problem.config.ReproducersCapacity).toList
        mkFuture[TIndEval](iEvals1, p2Rep.extractElements(problem.config.ReproducersCapacity).toList,
          (iEvals: List[TIndEval]) => {
            Reproducer.reproduce(iEvals.toList)
          }, (r1: Any) => {
            val rResult = r1.asInstanceOf[TPopulation]
            if (rResult.length > 0) {
              p2Eval.append(rResult)
              if (problem.config.rand.nextInt(100) % 2 == 0 && bestSolution._1 != null) {
                migrantsDestiny !('migrate, bestSolution.clone())
              }
            }
          }, Evaluations < cEvaluations)
      }

  }

}
