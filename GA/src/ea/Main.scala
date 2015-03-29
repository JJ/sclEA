package org

import ea.problem._

object Main extends App {

  //for (_ <- 1 to 10) {
  //  val obj = maxOnesSeqConfiguration
  //  obj.evolve()
  //  println("Best solution: " + obj.bestSolution)
  //  println("Evaluations: " + obj.evaluations)
  //}

  val obj = maxOnesFuturesPConfiguration

  obj.evolve()

  obj.log()
  println()
  println("Best solution: " + obj.bestSolution)
  println("Evaluations: " + obj.evaluations)

}