package pea

import java.util.concurrent.Executors

import akka.actor.{ActorSystem, Props}
import ea._

import scala.concurrent.ExecutionContext


trait FutureParEA {
  this: Problem =>

  private[this] val executor = Executors.newCachedThreadPool()
  private[this] implicit val executionContext = ExecutionContext.fromExecutor(executor)
  val system = ActorSystem("main")

  def runParCEvals(resultObtained: (TIndEval, Int, Int) => Unit): Unit = {
    val resObtained = (ind: TIndEval, cEvals: Int, cEmmigrants: Int) => {
      resultObtained(ind, cEvals, cEmmigrants)
    }
    val controler = system.actorOf(Props(new PoolManagerControlerCEvals(this, resObtained, system, executionContext, () => {executor.shutdown(); system.shutdown()})))
    controler ! 'start
  }

}
