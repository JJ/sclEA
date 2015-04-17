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

package ea.variation

import ea.TInd.changeBit
import ea.{GARandom, TInd, TMutation}

import scala.concurrent.stm.skel.SimpleRandom

object mutationImpl {

  def mutation(pMutation: Double)(implicit rand: GARandom): TMutation = (ind: TInd) => {
    if (rand.nextDouble() < pMutation) {
      val pos = rand.nextInt(ind.size)
      ind.update(pos, changeBit(ind(pos)))
    }
    ind
  }

}
