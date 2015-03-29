package ea.selection

import ea._

import scala.collection.immutable.BitSet
import scala.reflect._

object replacementSelections {

  //  def ageBasedReplacement: TSelection = (p: TPopEval) => {
  //    null
  //  }

  def tournamentReplacement(k: Int)
                           (implicit rand: {def nextInt(n: Int): Int}): TSelection[(TPopEval, TPopEval), TPopEval] =
    (p: (TPopEval, TPopEval)) => {
      val indexesCount = p._2.size
      @annotation.tailrec
      def selectIndexes(iCount: Int, currentIndexes: BitSet): BitSet = {
        if (iCount >= indexesCount)
          currentIndexes
        else {
          val currentIndexesAsArray = currentIndexes.toArray(classTag[Int])
          val inds = for (_ <- 1 to k) yield {
            val pos = rand.nextInt(currentIndexesAsArray.size)
            currentIndexesAsArray(pos)
          }
          val worst = inds.reduce((a, b) => if (p._1(a)._2 < p._1(b)._2) a else b)
          selectIndexes(iCount + 1, currentIndexes - worst)
        }
      }

      val indexesToKeep = selectIndexes(0, BitSet.empty ++ p._1.indices)
      val res = (for {i <- p._1.indices
                      if indexesToKeep.contains(i)} yield p._1(i)).toArray
      res ++ p._2
    }


  def survAndParentsSelector(implicit rand: {def nextInt(n: Int): Int},
                             population: Array[TIndEval], tournamentSize: Int): TSelection[TPopEval, TParents] =
    (newInds: TPopEval) => {
      def f(a: (Int, Int), b: (TIndEval, Int)) = {
        val currentMax = population(a._1)._2
        val vMax = if (currentMax < b._1._2) b._2 else a._1
        val currentMin = population(a._2)._2
        val vMin = if (currentMin > b._1._2) b._2 else a._2
        (vMax, vMin)
      }
      for (ind <- newInds) yield {
        val inds = for (_ <- 1 to tournamentSize) yield {
          val pos = rand.nextInt(population.size)
          (population(pos), pos)
        } // take k random individuals
        val (v1, v2) = inds.foldLeft((0, 0))(f) // best and worst fitness
        val bestIndividual = population(v1)
        val worst = v2
        val parent2 = population(rand.nextInt(population.size))
        population.update(worst, ind)
        (bestIndividual._1, parent2._1) // a pair of the best and other randomly selected
      }
    }


}
