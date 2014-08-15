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

  private[this] val executor = Executors.newCachedThreadPool()
  private[this] implicit val executionContext = ExecutionContext.fromExecutor(executor)

  def runParCEvals(resultObtained: (TIndEval) => Unit) {
    config.setData(fitnessFunction, qualityFitnessFunction, doWhenQualityFitnessTrue)
    Evaluator.config = config
    Reproducer.config = config
    val pResultObtained = Promise[TIndEval]()
    pResultObtained.future.onSuccess({ case r => resultObtained(r)})
    val p2Rep = new ReproducersPool[TIndEval]()
    val p2Eval = new EvaluatorsPool[TIndividual](getPop())
    Evaluations = 0
    var bestSolution = new TIndEval(null, -1)
    val nFutures = new AtomicInteger(0)
    val resultGiven = new AtomicInteger(0)
    def mkFuture(beginAction: => Any, endAction: (Any) => Unit, cond: => Boolean): Future[Any] = {
      val res = Future {
        beginAction
      }
      nFutures.incrementAndGet()
      res.onSuccess {
        case eResult: Any =>
          endAction(eResult)
          val nVal = nFutures.decrementAndGet()
          if (cond)
            mkFuture(beginAction, endAction, cond)
          else {
            if (!pResultObtained.isCompleted) {
              pResultObtained.success(bestSolution)
            }
            if (nVal == 0) {
              executor.shutdown()
            }
          }
      }
      res
    }

    for (i <- 1 to config.EvaluatorsCount)
      mkFuture({
        val inds2Eval = p2Eval.extractElements(config.EvaluatorsCapacity)
        var evals = Evaluator.evaluate(inds2Eval.toList)
        evals = evals.sortWith(_.compareTo(_) > 0)
        evals
      }, (r1: Any) => {
        val eResult = r1.asInstanceOf[ArrayBuffer[TIndEval]]
        if (eResult.length > 0) {
          p2Rep.append(eResult)
          Evaluations += eResult.length
          if (bestSolution._2 < eResult(0)._2) {
            bestSolution = eResult(0)
          }
        }
      }, Evaluations < config.Evaluations)


    for (i <- 1 to config.ReproducersCount)
      mkFuture({
        val iEvals = p2Rep.extractElements(config.ReproducersCapacity)
        val res = Reproducer.reproduce(iEvals.toList)
        res
      }, (r1: Any) => {
        val rResult = r1.asInstanceOf[TPopulation]
        if (rResult.length > 0) {
          p2Eval.append(rResult)
        }
      }, Evaluations < config.Evaluations)

  }
}
