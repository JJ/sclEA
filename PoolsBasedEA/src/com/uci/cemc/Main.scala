package com.uci.cemc

import java.io.{File, FileReader}
import java.util.Date

import com.google.gson.Gson
import ea.entities.{ExperimentConfig, ParRes, SeqRes}
import ea.{MaxSATProblem, SeqEA}
import pea.FutureParEA

object Main extends App {

  val g = new Gson()
  val fr = new FileReader(new File("configMaxSAT.json"))
  val cnf = g.fromJson(fr, classOf[ExperimentConfig])
  val obj = new MaxSATProblem(cnf, "./problems/uf100-01.cnf") with SeqEA with FutureParEA

  if (args.length < 1) println("Missing parameters.")
  else {
    args(0) match {

      case "par" =>
        val initTime = System.nanoTime
        obj.runParCEvals(
          (indRes, ev) => {
            val endTime = System.nanoTime
            val res = new ParRes(ev, cnf.EvaluatorsCapacity, cnf.ReproducersCapacity, cnf.EvaluatorsCount, cnf.ReproducersCount, endTime - initTime, indRes._2)
            println(g.toJson(res))
          })

      case _ => // case "seq"
        val initTime = System.nanoTime
        val indRes = obj.runSeqCEvals()
        val endTime = System.nanoTime
        val res = new SeqRes(endTime - initTime, indRes._2, obj.Evaluations)
        println(g.toJson(res))
    }
  }


}
