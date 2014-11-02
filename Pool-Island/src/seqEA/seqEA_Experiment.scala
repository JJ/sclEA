package seqEA

import java.io._
import java.util.Date

import pea._

import scala.collection.mutable.{ArrayBuffer, HashMap}

object seqEA_Experiment {

  def run() {

    var evaluations = problem.evaluations
    var solutionFound = false

    def terminationCondition(): Boolean = solutionFound || evaluations <= 0

    def runSeqEA(initPool: HashMap[List[AnyVal], (Int, Int)]): (Boolean, Int, Int) = {
      evaluations = problem.evaluations
      solutionFound = false

      var pool = initPool.clone
      var evalDone = 0
      var bSolution: (List[AnyVal], Int) = (List(), -1)

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
            sels = pool.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 1).keys.take(evaluatorsCapacity)
          )
          if (resEval) {
            val pnSels = nSels.map((p: (List[AnyVal], Int)) => (p._1, (p._2, 2)))
            pool ++= pnSels
            newEvalDone = pnSels.size
            if (bSolution._2 < bs._2) bSolution = bs
            solutionFound = solFound
          }
          val tFiltered = pool.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 2).keys.toList
          val population = tFiltered.map(i => (i, pool(i)._1))
          val subpop = Reproducer.extractSubpopulation(population, problem.reproducersCapacity)
          val (res, resultData) =
            Reproducer.evolve(
              subpop,
              parentsCount = subpop.size / 2)

          if (res) {
            val (noParents, nInds, bestParents) = resultData
            pool = Reproducer.mergeFunction(
              pool, subpop,
              noParents, nInds,
              bestParents, initPool.size)
          }

          evalDone = newEvalDone
        }
      }
      (solutionFound, bSolution._2, problem.evaluations - evaluations)

    }

    def testsRunSeqEA() = {

      val tt = new Date().getTime()
      println(s"Doing experiment (time -> $tt)")

      val initEvol = new Date().getTime()

      val p = HashMap[List[AnyVal], (Int, Int)]()

      p ++= (for (i <- problem.genInitPop()) yield (i, (-1, 1)))

      val res = runSeqEA(p)

      (new Date().getTime() - initEvol, res)
    }

    def calculatePopulationSize(): ArrayBuffer[(Long, Int, Int)] = {
      val nRes = new ArrayBuffer[(Long, Int, Int)]()
      val max = problem.repetitions
      var i = 0
      var found = true
      while (i < max && found) {
        val (time, (solFound, fitness, evals)) = testsRunSeqEA()
        found = solFound
        if (found) {
          nRes.append((time, fitness, evals))
        }
        i += 1
      }
      if (!found) {
        println()
        println("Solution don't found.")
      }
      nRes
    }

    val nRes: ArrayBuffer[(Long, Int, Int)] = calculatePopulationSize()

    //    val nRes =
    //      for (_ <- 1 to problem.repetitions)
    //    {val (evolutionDelay, (_, bestSol, evals)) = testsRunSeqEA()
    //    yield (evolutionDelay, bestSol, evals)
    // }

    val w = new PrintWriter(new File(problem.seqOutputFilename))
    w.write("EvolutionDelay,Evaluations,BestSol\n")
    for ((evolutionDelay, bestSol, evals) <- nRes) {
      w.write(s"$evolutionDelay,$evals,$bestSol\n")
    }
    w.close()

    println("Ends!")
  }

}