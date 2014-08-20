package pea

import akka.actor.{Actor, ActorSystem, Props}
import ea.{Evaluator, Problem, Reproducer, TIndEval}

import scala.concurrent.{ExecutionContextExecutor, Promise}

class PoolManagerControlerFitnessQuality(problem: Problem, resultObtained: (TIndEval, Int, Int) => Unit, system: ActorSystem, eContext: ExecutionContextExecutor, allFufuresFinished: => Unit) extends Actor {

  var Evaluations: Int = _
  var Emigrations: Int = _
  var BestSolution = new TIndEval(null, -1)

  private[this] var islandsFinished: Int = _
  private[this] var islandsFuturesFinished: Int = _
  private[this] implicit val executionContext = eContext

//  val allFuturesFinishedPromise = Promise[Boolean]()
//  val allFuturesFinishedFuture = allFuturesFinishedPromise.future
//  val resultObtainedPromise = Promise[Boolean]()
//  val resultObtainedFuture = resultObtainedPromise.future

  override def receive = {

    case ('resultObtained, ind: TIndEval, cEvals: Int, cEmigrations: Int) =>
      islandsFinished += 1
      Evaluations += cEvals
      Emigrations += cEmigrations
      if (BestSolution._1 == null) {
        BestSolution = ind
        for (i <- islands)
          i ! 'resultObtained
      }
      if (islandsFinished == problem.config.IslandsCount) {
        resultObtained(BestSolution, Evaluations, Emigrations)
//        resultObtainedPromise.success(true)
      }

    case 'start =>
      islandsFinished = 0
      islandsFuturesFinished = 0
      Evaluations = 0
      Emigrations = 0
      for (island <- islands)
        island ! 'start

    case 'allFuturesFinished =>
      islandsFuturesFinished += 1
      if (islandsFuturesFinished == problem.config.IslandsCount) {
        allFufuresFinished
//        allFuturesFinishedPromise.success(true)
      }

  }
//
//  for {
//    r1 <- allFuturesFinishedFuture
//    r1 <- resultObtainedFuture
//  } allFufuresFinished

  problem.config.setData(problem.fitnessFunction, problem.qualityFitnessFunction, problem.doWhenQualityFitnessTrue)
  Evaluator.config = problem.config
  Reproducer.config = problem.config

  val islands = for (i <- 0 to problem.config.IslandsCount - 1)
  yield system.actorOf(Props(new PoolManagerFitnessQuality(problem, self, eContext)))

  for (i <- 0 to problem.config.IslandsCount - 1)
    islands(i) !('migrantsDestiny, islands((i + 1) % problem.config.IslandsCount))

}
