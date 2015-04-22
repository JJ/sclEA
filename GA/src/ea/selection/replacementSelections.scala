/**
 * Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
 * Copyright 2015 by José Albert Cruz Almaguer.
 *
 * This program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http:www.gnu.org/licenses/agpl-3.0.txt) for more details.
 */

package ea.selection

import ea._

import scala.collection.immutable.BitSet
import scala.reflect._

object replacementSelections {

  //  def ageBasedReplacement: TSelection = (p: TPopEval) => {
  //    null
  //  }

  def tournamentReplacement(k: Int)
                           (implicit rand: GARandom): TSelection[(TPopEval, TPopEval), TPopEval] =
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
          val worst = inds.reduce((a, b) => if (p._1(a).fit < p._1(b).fit) a else b)
          selectIndexes(iCount + 1, currentIndexes - worst)
        }
      }

      val indexesToKeep = selectIndexes(0, BitSet.empty ++ p._1.indices)
      val res = (for {i <- p._1.indices
                      if indexesToKeep.contains(i)} yield p._1(i)).toArray
      res ++ p._2
    }


  def survAndParentsSelector(implicit rand: GARandom,
                             population: Array[TIndEval], tournamentSize: Int): TSelection[TPopEval, TParents] =
    (newInds: TPopEval) => {
      def f(a: (Int, Int), b: (TIndEval, Int)) = {
        val currentMax = population(a._1).fit
        val vMax = if (currentMax < b._1.fit) b._2 else a._1
        val currentMin = population(a._2).fit
        val vMin = if (currentMin > b._1.fit) b._2 else a._2
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
        (bestIndividual.ind, parent2.ind) // a pair of the best and other randomly selected
      }
    }


}
