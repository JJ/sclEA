package seqEA

import scala.collection.mutable.ArrayBuffer

class TIndividual extends ArrayBuffer[Byte] {
  override def clone(): TIndividual = {
    val res = new TIndividual()
    this.foreach(res += _)
    res
  }
}

class TPopulation extends ArrayBuffer[TIndividual] {
  override def clone(): TPopulation = {
    val res = new TPopulation()
    this.foreach(res += _)
    res
  }
}

class TIndEval(p1: TIndividual, p2: Long) extends Tuple2[TIndividual, Long](p1, p2) with Comparable[TIndEval] {
  override def compareTo(o: TIndEval): Int = this._2.compareTo(o._2)

  override def clone(): TIndEval = new TIndEval(this._1.clone(), this._2)
}
