package com.uci.cemc

import java.io.{File, FileReader}

import com.google.gson.Gson
import ea.MaxOneProblem
import ea.entities.ExperimentConfig

object Main extends App {
  val g = new Gson()
  val fr = new FileReader(new File("configMaxOnes.json"))
  val cnf = g.fromJson(fr, classOf[ExperimentConfig])
  val obj = new MaxOneProblem(cnf)
  println(obj.runSeqCEvals()._2)
}
