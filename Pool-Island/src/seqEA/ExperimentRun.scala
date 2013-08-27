/**
 * Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
 * Copyright 2013 by José Albert Cruz Almaguer.
 *
 * This program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http:www.gnu.org/licenses/agpl-3.0.txt) for more details.
 */

package seqEA

import pea._
import scala.collection.mutable.HashMap

import java.util.Date
import java.io._

object ExperimentRun extends App {

  var evaluations = problem.evaluations
  var solutionFound = false

  def bestSolution(pool: HashMap[List[AnyVal], (Int, Int)]): Int = {
    val sel = pool.filter(
      (a: (List[AnyVal], (Int, Int))) => a._2._2 == 2).keys
    val all = sel.map(i => (i, pool(i)._1))
    all.reduce((a: (List[AnyVal], Int), b: (List[AnyVal], Int)) =>
      if (a._2 < b._2) b else a)._2
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
          evaluations -= evalDone
          Math.min(evaluations, problem.evaluatorsCapacity)
      }

      var newEvalDone = 0
      if (evaluatorsCapacity > 0) {
        val (resEval, nSels) = Evaluator.evaluate(
          sels = pool.filter(
            (a: (List[AnyVal], (Int, Int))) => a._2._2 == 1).keys.take(evaluatorsCapacity),
          doIfFitnessTerminationCondition = (ind: List[AnyVal], fit: Int) => {
            solutionFound = true
          })

        if (resEval) {
          val pnSels = nSels.map(
            (p: (List[AnyVal], Int)) => (p._1, (p._2, 2)))
          pool ++= pnSels
          newEvalDone += pnSels.size
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

    bestSolution(pool)

  }

  def testsRunSeqEA() = {
    val initEvol = new Date().getTime()

    val res = runSeqEA(
      HashMap[List[AnyVal], (Int, Int)]() ++ (for (i <- problem.genInitPop()) yield (i, (-1, 1))))

    (new Date().getTime() - initEvol, res)
  }

  val nRes =
    for (_ <- 1 to 20)
      yield testsRunSeqEA()

  val w = new PrintWriter(new File("../../results/book2013/sclEA/seqResults.csv"))
  w.write("EvolutionDelay,BestSol\n")

  for ((evolutionDelay, bestSol) <- nRes) {
    w.write(s"$evolutionDelay,$bestSol\n")
  }

  w.close()

  println("Ends!")
}