package seqEA

import java.util.Date

import com.google.gson.Gson
import ea.entities.SeqRes
import pea._
import pea.ds.{EvaluatorsPool, ReproducersPool}

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

object seqEA_Experiment {

  def run() {

    var evaluations = problem.evaluations
    var solutionFound = false

    def terminationCondition(): Boolean = solutionFound || evaluations <= 0

    def runSeqEA(initPool: List[TIndividual]): (Boolean, Long, Int) = {
      evaluations = problem.evaluations
      solutionFound = false

      val p2Rep = new ReproducersPool[TIndEval]()
      val p2Eval = new EvaluatorsPool[TIndividual](initPool)

      var evalDone = 0
      var bSolution: TIndEval = new TIndEval(null, -1)

      while (!terminationCondition()) {
        //        println(evaluations + " --> " + bSolution._2)
        val evaluatorsCapacity = {
          val teval = evaluations - evalDone
          evaluations = if (teval > 0) teval else 0
          Math.min(evaluations, problem.evaluatorsCapacity)
        }
        var newEvalDone = 0
        if (evaluatorsCapacity > 0) {
          val (resEval, solFound, nSels, bs) = Evaluator.evaluate(
            sels = p2Eval.extractElements(evaluatorsCapacity).toList
          )
          if (resEval) {
            p2Rep.append(nSels)
            newEvalDone = nSels.size
            if (bSolution._2 < bs._2) bSolution = bs
            solutionFound = solFound
          }

          val subpop = p2Rep.extractElements(problem.reproducersCapacity).toList
          val (res, resultData) = Reproducer.evolve(
            subpop,
            parentsCount = subpop.size / 2
          )

          if (res) {
            val (noParents, nInds, bp) = resultData
            p2Eval.append(nInds)
          }

          evalDone = newEvalDone
        }
      }
      (solutionFound, bSolution._2, problem.evaluations - evaluations)

    }

    def testsRunSeqEA() = {

      //      val tt = new Date().getTime()
      //      println(s"Doing experiment (time -> $tt)")

      val initEvol = new Date().getTime

      val res = runSeqEA(problem.genInitPop().toList)

      (new Date().getTime - initEvol, res)
    }

    def calculatePopulationSize(): ArrayBuffer[(Long, Long, Int)] = {
      val nRes = new ArrayBuffer[(Long, Long, Int)]()
      val max = problem.repetitions
      println(max)
      var i = 0
      var found = true
      while (i < max && found) {
        println(1)
        val (time, (solFound, fitness, evals)) = testsRunSeqEA()
        println(2)
        found = solFound
        println(3)
        if (found) {
          nRes.append((time, fitness, evals))
        }
        println(4)
        i += 1
      }
      if (!found) {
        println()
        println("Solution don't found.")
      }
      nRes
    }

    //    val nRes: ArrayBuffer[(Long, Long, Int)] = calculatePopulationSize()

    val (evolutionDelay, (_, bestSol, evals)) = testsRunSeqEA()
    val res = new SeqRes(evolutionDelay, bestSol, evals)
    val g = new Gson()
    println(g.toJson(res))

    //    val nRes =
    //      for (_ <- 1 to problem.repetitions)
    //        yield testsRunSeqEA()
    //
    //
    //    val w = new PrintWriter(new File(problem.seqOutputFilename))
    //    w.write("EvolutionDelay,Evaluations,BestSol\n")
    //    for ((evolutionDelay, (_, bestSol, evals)) <- nRes) {
    //      w.write(s"$evolutionDelay,$evals,$bestSol\n")
    //    }
    //    w.close()
    //
    //    println("Ends!")
  }

}