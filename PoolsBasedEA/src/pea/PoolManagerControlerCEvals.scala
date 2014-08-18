package pea

import akka.actor.{Actor, ActorSystem, Props}
import ea._

import scala.concurrent.ExecutionContextExecutor

class PoolManagerControlerCEvals(problem: Problem, resultObtained: (TIndEval, Int, Int) => Unit, system: ActorSystem, eContext: ExecutionContextExecutor, allFufuresFinished: ()=> Unit) extends Actor {

  var Evaluations = 0
  var Emigrations = 0
  var BestSolution = new TIndEval(null, -1)

  private[this] var islandsFinished = 0
  private[this] var islandsFuturesFinished = 0

  override def receive = {

    case ('resultObtained, ind: TIndEval, cEvals: Int, cEmigrations: Int) =>
      islandsFinished += 1
      Evaluations += cEvals
      Emigrations += cEmigrations
      if (BestSolution._2 < ind._2)
        BestSolution = ind

      if (islandsFinished == 2)
        resultObtained(BestSolution, Evaluations, Emigrations)

    case 'start =>
      i1 ! 'start
      i2 ! 'start
  }

  //  def runParCEvals(resultObtained: (TIndEval) => Unit): Unit = {
  problem.config.setData(problem.fitnessFunction, problem.qualityFitnessFunction, problem.doWhenQualityFitnessTrue)
  Evaluator.config = problem.config
  Reproducer.config = problem.config
  val islandsFuturesFinishedControl = () => {
    islandsFuturesFinished += 1
    if (islandsFuturesFinished == 2) {
      allFufuresFinished()
    }
  }

  val i1 = system.actorOf(Props(new PoolManagerCEvals(problem, problem.config.Evaluations, self, eContext, islandsFuturesFinishedControl)))
  val i2 = system.actorOf(Props(new PoolManagerCEvals(problem, problem.config.Evaluations, self, eContext, islandsFuturesFinishedControl)))

  i2 !('migrantsDestiny, i1)
  i1 !('migrantsDestiny, i2)

}
