package ea

import ea.entities.ExperimentConfig

import scala.collection.mutable.ArrayBuffer

class MaxSATProblem(config: ExperimentConfig, instanceFileName: String) extends Problem(config) {

  var clauseLength: Int = _
  var varsCount: Int = _
  var clausesCount: Int = _

  var clauses: ArrayBuffer[List[(Byte, Int)]] = _

  val it = scala.io.Source.fromFile(instanceFileName).getLines

  (1 to 5).foreach(_ => it.next)

  val spaceRE = """\s+0*"""

  val l1 = it.next
  val f1 = l1.split(spaceRE)
  it.next
  val l2 = it.next
  val f2 = l2.split(spaceRE)
  clauseLength = f1.last.toInt
  varsCount = f2(2).toInt
  config.ChromosomeSize = varsCount
  clausesCount = f2(3).toInt

  clauses = ArrayBuffer[List[(Byte, Int)]]()

  var l = it.next
  while (!l.contains("%")) {

    val intValues = l.trim.split(spaceRE).map(_.toInt)
    val nEntry = intValues.map(
      i => if (i < 0) (0.asInstanceOf[Byte], -i - 1) else (1.asInstanceOf[Byte], i - 1))

    clauses += nEntry.toList

    l = it.next
  }

  override def fitnessFunction(ind: TIndividual): Long = {
    clauses.count((c: List[(Byte, Int)]) => {
      c.exists(
        (e: (Byte, Int)) => ind(e._2) == e._1)
    })
  }

  override def qualityFitnessFunction(v: Long): Boolean = v > 405

  override def doWhenQualityFitnessTrue(i: TIndEval) {
  }

}
