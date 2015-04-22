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

object parentSelections {

  //  def fitnessProportionalSelection: TSelection = (p: TPop) => {
  //    null
  //  }
  //
  //  def rankingSelection: TSelection = (p: TPop) => {
  //    null
  //  }
  //
  //  def rouletteWheel: TSelection = (p: TPop) => {
  //    null
  //  }

  // k > 2
  def tournamentSelection(k: Int, parentsCount: Int)
                         (implicit rand: GARandom): TSelection[TPopEval, TParents] =
    (p: TPopEval) => {
      (for (_ <- 1 to parentsCount) yield {
        val inds = for (_ <- 1 to k) yield p(rand.nextInt(p.size)) // take k random individuals
        val bestIndividual = inds.reduce((a, b) => if (a.fit < b.fit) b else a) // best fitness
        (bestIndividual.ind, p(rand.nextInt(p.size)).ind) // a pair of the best and other randomly selected
      }).toArray
    }

}