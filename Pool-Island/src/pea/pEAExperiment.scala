package pea

import akka.actor.{ActorSystem, Props}

//import com.typesafe.config.ConfigFactory

object pEAExperiment {

  var system: ActorSystem = ActorSystem("pEAs")

  def run() {

    sheduling.ShedulingUtility.start()

    val eProfiler = system.actorOf(Props[Profiler])
    val eManager = system.actorOf(Props[Manager])

    eManager !('init, eProfiler, system)
    eProfiler !('init, eManager)

    eManager !('session,
      (for (_ <- 1 to problem.repetitions)
      yield (() => Experiment.r2(eProfiler, eManager), "r2")).toList)

  }
}