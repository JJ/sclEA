package pea

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, ActorRef}
import ea._
import pea.ds.{EvaluatorsPool, ReproducersPool}

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContextExecutor, Future, Promise}

class PoolManagerFitnessQuality(problem: Problem, manager: ActorRef, eContext: ExecutionContextExecutor) extends Actor {

  private[this] implicit val executionContext = eContext

  var migrantsDestiny: ActorRef = _
  var Evaluations: Int = _
  var Emigrations: Int = _
  var resultObtained: Boolean = false
  var active: Boolean = true

  val p2Rep = new ReproducersPool[TIndEval]()
  val p2Eval = new EvaluatorsPool[TIndividual](problem.getPop())

  override def receive: Receive = {

    case 'resultObtained =>
      active = false
      if (!resultObtained) {
        manager !('resultObtained, new TIndEval(null, -1), Evaluations, Emigrations)
        active = false
        resultObtained = true
      }

    case ('migrantsDestiny, mDestiny: ActorRef) =>
      migrantsDestiny = mDestiny

    case ('migrate, newMigrant: TIndEval) =>
      p2Rep.append(List(newMigrant))
      p2Rep.removeWorstN(1)
      Emigrations += 1

    case 'start =>
      val pResultObtained = Promise[TIndEval]()
      pResultObtained.future.onSuccess({
        case r => if (!resultObtained) {
          manager !('resultObtained, r, Evaluations, Emigrations)
          resultObtained = true
        }
      })
      Evaluations = 0
      var bestSolution = new TIndEval(null, -1)
      val nFutures = new AtomicInteger(0)
      def mkFuture[T](inds2Eval: List[T], feeder: => List[T], beginAction: List[T] => Any, endAction: (Any) => Unit, cond: => Boolean): Future[Any] = {
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
              if (!pResultObtained.isCompleted) {
                pResultObtained.success(bestSolution)
              }
              if (nVal == 0) {
                manager ! 'allFuturesFinished
              }
            }
        }
        res
      }
      val originalDoWhenFind = problem.config.df
      val newDoWhenFind: TIndEval => Unit = (iEval: TIndEval) => {
        originalDoWhenFind(iEval)
        bestSolution = iEval
        active = false
      }
      problem.config.df = newDoWhenFind
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
          }, active)
      }

      for (i <- 1 to problem.config.ReproducersCount) {
        val iEvals1 = p2Rep.extractElements(problem.config.ReproducersCapacity).toList
        mkFuture[TIndEval](iEvals1, p2Rep.extractElements(problem.config.ReproducersCapacity).toList,
          (iEvals: List[TIndEval]) => {
            val res = Reproducer.reproduce(iEvals.toList)
            res
          }, (r1: Any) => {
            val rResult = r1.asInstanceOf[TPopulation]
            if (rResult.length > 0) {
              p2Eval.append(rResult)
              if (problem.config.rand.nextInt(100) % 2 == 0 && bestSolution._1 != null) {
                migrantsDestiny !('migrate, bestSolution.clone())
              }
            }
          }, active)
      }

  }

}
