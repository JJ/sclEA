package test

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
