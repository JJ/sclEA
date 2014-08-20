package pea

import akka.actor.{Actor, ActorSystem, Props}
import ea._

import scala.concurrent.ExecutionContextExecutor

class PoolManagerControlerCEvals(problem: Problem, resultObtained: (TIndEval, Int, Int) => Unit, system: ActorSystem, eContext: ExecutionContextExecutor, allFufuresFinished: => Unit) extends Actor {

  var Evaluations: Int = _
  var Emigrations: Int = _
  var BestSolution = new TIndEval(null, -1)

  private[this] var islandsFinished: Int = _
  private[this] var islandsFuturesFinished: Int = _
  private[this] implicit val executionContext = eContext

  override def receive = {

    case ('resultObtained, ind: TIndEval, cEvals: Int, cEmigrations: Int) =>
      islandsFinished += 1
      Evaluations += cEvals
      Emigrations += cEmigrations
      if (BestSolution._2 < ind._2)
        BestSolution = ind
      if (islandsFinished == problem.config.IslandsCount)
        resultObtained(BestSolution, Evaluations, Emigrations)

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
      }

  }

  problem.config.setData(problem.fitnessFunction, problem.qualityFitnessFunction, problem.doWhenQualityFitnessTrue)
  Evaluator.config = problem.config
  Reproducer.config = problem.config

  val a = problem.config.Evaluations / problem.config.IslandsCount
  val rest = problem.config.Evaluations % problem.config.IslandsCount
  val islands = for (i <- 0 to problem.config.IslandsCount - 1)
  yield system.actorOf(Props(new PoolManagerCEvals(problem, a + (if (i < rest) 1 else 0), self, eContext)))

  for (i <- 0 to problem.config.IslandsCount - 1)
    islands(i) !('migrantsDestiny, islands((i + 1) % problem.config.IslandsCount))

}
