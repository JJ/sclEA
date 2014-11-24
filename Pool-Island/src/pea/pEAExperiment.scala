package pea

import java.util.concurrent.Executors

import akka.actor.{ActorSystem, Props}

import scala.concurrent.ExecutionContext

//import com.typesafe.config.ConfigFactory

object pEAExperiment {
  val executor = Executors.newCachedThreadPool()
  implicit val executionContext = ExecutionContext.fromExecutor(executor)
  var system: ActorSystem = ActorSystem("pEAs")

  def run() {

    sheduling.ShedulingUtility.start()

    val eProfiler = system.actorOf(Props[Profiler])
    val eManager = system.actorOf(Props[Manager])

    eManager !('init, eProfiler, system)
    eProfiler !('init, eManager)

    eManager !('mkExperiment, (() => Experiment.r2(eProfiler, eManager), "r2"))

    //    eManager !('session,
    //      (for (_ <- 1 to problem.repetitions)
    //      yield (() => Experiment.r2(eProfiler, eManager), "r2")).toList)

  }
}