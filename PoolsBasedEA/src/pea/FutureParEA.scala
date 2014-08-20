package pea

import java.util.concurrent.Executors

import akka.actor.{ActorSystem, Props}
import ea._

import scala.concurrent.ExecutionContext


trait FutureParEA {
  this: Problem =>

  var system: ActorSystem = _

  def runParCEvals(resultObtained: (TIndEval, Int, Int) => Unit): Unit = {
    val executor = Executors.newCachedThreadPool()
    implicit val executionContext = ExecutionContext.fromExecutor(executor)
    system = ActorSystem("main")
    val controler = system.actorOf(Props(new PoolManagerControlerCEvals(this, resultObtained, system, executionContext, {
      executor.shutdown()
      system.shutdown()
    })))
    controler ! 'start
  }

  def runParFitnessQuality(resultObtained: (TIndEval, Int, Int) => Unit): Unit = {
    val executor = Executors.newCachedThreadPool()
    implicit val executionContext = ExecutionContext.fromExecutor(executor)
    system = ActorSystem("main")
    val controler = system.actorOf(Props(new PoolManagerControlerFitnessQuality(this, resultObtained, system, executionContext, {
      executor.shutdown()
      system.shutdown()
    })))
    controler ! 'start
  }

}
