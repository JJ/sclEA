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

package test.ea

import scala.collection.Seq

class RandomMock(lsInts: Int*) {
  private var indexInts: Int = 0
  private var indexDoubles: Int = _

  private var lsDoubles: Seq[Double] = _

  def setDoubles(ls: Double*): Unit = {
    lsDoubles = ls
    indexDoubles = 0
  }

  def nextInt(n: Int): Int = {
    val res = lsInts(indexInts)
    indexInts = (indexInts + 1) % lsInts.size
    res
  }

  def nextDouble(): Double = {
    val res = lsDoubles(indexDoubles)
    indexDoubles = (indexDoubles + 1) % lsDoubles.size
    res
  }
}
