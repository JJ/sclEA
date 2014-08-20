package com.uci.cemc

import java.io.{File, FileReader}
import java.util.Date

import com.google.gson.Gson
import ea.MaxSATProblem
import ea.entities.{ExperimentConfig, ParRes, SeqRes}
import pea.FutureParEA

object Main extends App {

  val g = new Gson()
  val fr = new FileReader(new File("configMaxSAT.json"))
  val cnf = g.fromJson(fr, classOf[ExperimentConfig])
  val obj = new MaxSATProblem(cnf, "./problems/uf100-01.cnf") with FutureParEA

  //  obj.runParCEvals((res, ev, em) => println(res._2))

  if (args.length < 1)
    println("Missing parameters")
  else {
    args(0) match {
      case "seqfq" =>
        val initTime = new Date().getTime * 1000
        val indRes = obj.runSeqFitnessQuality()
        val endTime = new Date().getTime * 1000
        val res = new SeqRes(endTime - initTime, indRes._2, obj.Evaluations)
        println(g.toJson(res))

      case "parfq" =>
        val initTime = new Date().getTime * 1000
        obj.runParFitnessQuality(
          (indRes, ev, em) => {
            val endTime = new Date().getTime * 1000
            val res = new ParRes(ev, cnf.EvaluatorsCapacity, cnf.ReproducersCapacity, cnf.EvaluatorsCount, cnf.ReproducersCount, cnf.IslandsCount, em, endTime - initTime, indRes._2)
            println(g.toJson(res))
          })

      case "parce" =>
        val initTime = new Date().getTime * 1000
        obj.runParCEvals(
          (indRes, ev, em) => {
            val endTime = new Date().getTime * 1000
            val res = new ParRes(ev, cnf.EvaluatorsCapacity, cnf.ReproducersCapacity, cnf.EvaluatorsCount, cnf.ReproducersCount, cnf.IslandsCount, em, endTime - initTime, indRes._2)
            println(g.toJson(res))
          })

      case _ =>
        val initTime = new Date().getTime * 1000
        val indRes = obj.runSeqCEvals()
        val endTime = new Date().getTime * 1000
        val res = new SeqRes(endTime - initTime, indRes._2, obj.Evaluations)
        println(g.toJson(res))

    }
  }


}
