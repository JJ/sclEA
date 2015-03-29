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

package test

import ea.TInd
import ea.variation.crossoverImpl.twoPoint
import ea.variation.mutationImpl.mutation
import org.scalatest._

class VariationSpec extends FlatSpec with Matchers {

  "A twoPoint crossover" should "be in average case" in {

    val ch1O = TInd(0, 0, 0, 0, 1, 0, 0, 0, 0)
    val ch2O = TInd(1, 1, 0, 1, 0, 0, 0, 0, 1)

    val ch1F = TInd(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val ch2F = TInd(1, 1, 0, 1, 1, 0, 0, 0, 1)

    val r = new RandomMock(2, 5) // (+2=4, +1=6)

    twoPoint(r)((ch1O, ch2O)) should be(Array(ch1F, ch2F))
  }

  it should "be in left corner case" in {

    val ch1O = TInd(0, 0, 1, 0, 1, 0, 0, 0, 0)
    val ch2O = TInd(1, 1, 0, 1, 0, 0, 0, 0, 1)

    val ch1F = TInd(0, 1, 0, 0, 1, 0, 0, 0, 0)
    val ch2F = TInd(1, 0, 1, 1, 0, 0, 0, 0, 1)

    val r = new RandomMock(-1, 2) // (+2=1, +1=3)

    twoPoint(r)((ch1O, ch2O)) should be(Array(ch1F, ch2F))
  }

  it should "be in right corner case" in {
    val ch1O = TInd(0, 0, 0, 0, 1, 0, 0, 1, 0)
    val ch2O = TInd(1, 1, 0, 1, 0, 0, 1, 0, 1)

    val ch1F = TInd(0, 0, 0, 0, 1, 0, 1, 0, 0)
    val ch2F = TInd(1, 1, 0, 1, 0, 0, 0, 1, 1)

    val r = new RandomMock(4, 7) // (+2=6, +1=8)

    twoPoint(r)((ch1O, ch2O)) should be(Array(ch1F, ch2F))
  }


  it should "be when the randoms repeates generally" in {
    val ch1O = TInd(0, 0, 0, 0, 1, 0, 0, 1, 0)
    val ch2O = TInd(1, 1, 0, 1, 0, 0, 1, 0, 1)

    val ch1F = TInd(0, 0, 0, 0, 0, 0, 0, 1, 0)
    val ch2F = TInd(1, 1, 0, 1, 1, 0, 1, 0, 1)

    val r = new RandomMock(3, 4) // (+2=5, +1=5) so (-1=4, 5)

    twoPoint(r)((ch1O, ch2O)) should be(Array(ch1F, ch2F))
  }


  it should "be when the randoms repeates in left corner" in {
    val ch1O = TInd(0, 0, 0, 0, 1, 0, 0, 1, 0)
    val ch2O = TInd(1, 1, 0, 1, 0, 0, 1, 0, 1)

    val ch1F = TInd(0, 1, 0, 0, 1, 0, 0, 1, 0)
    val ch2F = TInd(1, 0, 0, 1, 0, 0, 1, 0, 1)

    val r = new RandomMock(0, 1) // (+2=2, +1=2) so (-1=1, 2)

    twoPoint(r)((ch1O, ch2O)) should be(Array(ch1F, ch2F))
  }

  it should "be when the randoms repeates in right corner" in {
    val ch1O = TInd(0, 0, 0, 0, 1, 0, 0, 1, 0)
    val ch2O = TInd(1, 1, 0, 1, 0, 0, 1, 0, 1)

    val ch1F = TInd(0, 0, 0, 0, 1, 0, 0, 0, 0)
    val ch2F = TInd(1, 1, 0, 1, 0, 0, 1, 1, 1)

    val r = new RandomMock(6, 7) // (+2=8, +1=8) so (-1=7, 8)

    twoPoint(r)((ch1O, ch2O)) should be(Array(ch1F, ch2F))
  }

  "A mutation " should "be in average and extreme cases" in {

    val ind = TInd(0, 0, 0, 0, 1, 0, 0, 0, 0)
    val indMutated = TInd(0, 0, 1, 0, 1, 0, 0, 0, 0)
    val indMutated1 = TInd(1, 0, 0, 0, 1, 0, 0, 0, 0)
    val indMutated2 = TInd(0, 0, 0, 0, 1, 0, 0, 0, 1)

    val r = new RandomMock(2, 0, 8)
    r.setDoubles(0.02)

    mutation(0.1)(r)(ind.clone()) should be(indMutated)
    mutation(0.1)(r)(ind.clone()) should be(indMutated1)
    mutation(0.1)(r)(ind.clone()) should be(indMutated2)

    mutation(0.01)(r)(ind.clone()) should be(ind)
    mutation(0.01)(r)(ind.clone()) should be(ind)
    mutation(0.01)(r)(ind.clone()) should be(ind)
  }

}