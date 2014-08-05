package seqEA

import pea._

import scala.collection.mutable.HashMap

import java.util.Date
import java.io._

object seqEA_Experiment {

  def run() {

    var evaluations = problem.evaluations
    var solutionFound = false

    def bestSolution(pool: HashMap[List[AnyVal], (Int, Int)]): (List[AnyVal], Int) = {
      val sel = pool.filter(
        (a: (List[AnyVal], (Int, Int))) => a._2._2 == 2).keys
      val all = sel.map(i => (i, pool(i)._1))
      all.reduce((a: (List[AnyVal], Int), b: (List[AnyVal], Int)) =>
        if (a._2 < b._2) b else a)
    }

    def terminationCondition(): Boolean = {
      problem.terminationCondition match {
        case 'fitnessTerminationCondition =>
          solutionFound
        case _ =>
          evaluations == 0
      }
    }

    def runSeqEA(initPool: HashMap[List[AnyVal], (Int, Int)]): Int = {
      evaluations = problem.evaluations
      solutionFound = false

      var pool = initPool.clone
      var evalDone = 0

      while (!terminationCondition()) {

        val evaluatorsCapacity = problem.terminationCondition match {
          case 'fitnessTerminationCondition =>
            problem.evaluatorsCapacity
          case _ =>
            val teval = evaluations - evalDone
            evaluations = if (teval > 0) teval else 0

            Math.min(evaluations, problem.evaluatorsCapacity)
        }

        var newEvalDone = 0

        if (evaluatorsCapacity > 0) {
          val (resEval, nSels) = Evaluator.evaluate(
            sels = pool.filter(
              (a: (List[AnyVal], (Int, Int))) => a._2._2 == 1).keys.take(evaluatorsCapacity),
            doIfFitnessTerminationCondition =
              (ind: List[AnyVal], fit: Int) => {

                solutionFound = true
              })

          if (resEval) {
            val pnSels = nSels.map((p: (List[AnyVal], Int)) => (p._1, (p._2, 2)))
            pool ++= pnSels
            newEvalDone = pnSels.size
          }

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

      bestSolution(pool)._2

    }

    def testsRunSeqEA() = {

      val tt = new Date().getTime()
      println(s"Doing experiment (time -> $tt)")

      val initEvol = new Date().getTime()

      val p = HashMap[List[AnyVal], (Int, Int)]()

      p ++ (for (i <- problem.genInitPop()) yield (i, (-1, 1)))

      val res = runSeqEA(p)

      (new Date().getTime() - initEvol, res)
    }

    val nRes =
      for (_ <- 1 to problem.repetitions)
      yield testsRunSeqEA()

    val w = new PrintWriter(new File(problem.seqOutputFilename))
    w.write("EvolutionDelay,BestSol\n")

    for ((evolutionDelay, bestSol) <- nRes) {
      w.write(s"$evolutionDelay,$bestSol\n")
    }

    w.close()

    println("Ends!")
  }

}