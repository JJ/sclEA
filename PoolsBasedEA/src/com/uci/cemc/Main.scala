package com.uci.cemc

import java.io.{File, FileReader}

import com.google.gson.Gson
import ea.MaxSATProblem
import ea.entities.ExperimentConfig

object Main extends App {
  val g = new Gson()
  //  val fr = new FileReader(new File("configMaxOnes.json"))
  val fr = new FileReader(new File("configMaxSAT.json"))
  val cnf = g.fromJson(fr, classOf[ExperimentConfig])
  //  val obj = new MaxOneProblem(cnf)
  val obj = new MaxSATProblem(cnf, "./problems/uf100-01.cnf")
  println(obj.runSeqCEvals()._2)
}
