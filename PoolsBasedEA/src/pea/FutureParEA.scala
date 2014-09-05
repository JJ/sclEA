package pea

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger

import ea._
import pea.ds.{EvaluatorsPool, ReproducersPool}

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}

trait FutureParEA {
  this: Problem =>

  def runParCEvals(resultObtained: (TIndEval, Long) => Unit): Unit = {
    val executor = Executors.newCachedThreadPool()
    implicit val executionContext = ExecutionContext.fromExecutor(executor)
    config.setData(fitnessFunction, (v) => false, (i) => {})
    Evaluator.config = config
    Reproducer.config = config

    val p2Rep = new ReproducersPool[TIndEval]()
    val p2Eval = new EvaluatorsPool[TIndividual](getPop())
    var bestSolution = new TIndEval(null, -1)
    Evaluations = 0
    val nFutures = new AtomicInteger(0)

    val pResultObtained = Promise[Unit]()
    pResultObtained.future.onSuccess({
      case _ => resultObtained(bestSolution, Evaluations)
    })
    def mkWorker[T](inds2Eval: List[T], // input of the function to be excecuted by the actual future
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
            mkWorker(ninds2Eval, feeder, beginAction, endAction, cond)
          }
          else {
            pResultObtained.trySuccess()
            if (nVal == 0) {
              executor.shutdown()
            }
          }
      }
      res
    }

    for (i <- 1 to config.EvaluatorsCount) {
      val inds2Eval1: List[TIndividual] = p2Eval.extractElements(config.EvaluatorsCapacity).toList
      mkWorker[TIndividual](inds2Eval1, p2Eval.extractElements(config.EvaluatorsCapacity).toList,
        (inds2Eval: List[TIndividual]) => {
          val evals = Evaluator.evaluate(inds2Eval.toList)
          evals.sortWith(_.compareTo(_) > 0)
        }, (result: Any) => {
          val eResult = result.asInstanceOf[ArrayBuffer[TIndEval]]
          if (eResult.length > 0) {
            p2Rep.append(eResult)
            Evaluations += eResult.length
            if (bestSolution._2 < eResult(0)._2) {
              bestSolution = eResult(0)
            }
          }
        }, Evaluations < config.Evaluations)
    }

    for (i <- 1 to config.ReproducersCount) {
      val iEvals1 = p2Rep.extractElements(config.ReproducersCapacity).toList
      mkWorker[TIndEval](iEvals1, p2Rep.extractElements(config.ReproducersCapacity).toList,
        (iEvals: List[TIndEval]) => {
          Reproducer.reproduce(iEvals.toList)
        }, (result: Any) => {
          val rResult = result.asInstanceOf[TPopulation]
          if (rResult.length > 0) {
            p2Eval.append(rResult)
          }
        }, Evaluations < config.Evaluations)
    }
  }
}