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

package problems

import seqEA.TIndividual

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class TMaxSAT_Problem(val clauseLength: Int, val varsCount: Int, val clausesCount: Int, val clauses: List[List[(Byte, Int)]])

object maxSAT extends protocols.Problem {

  val r = new Random()

  val data = config.GAConfig.loadFromJSON("maxSATConfig.json")

  //  def terminationCondition: Symbol =  'fitnessTerminationCondition

  //    Symbol(data.getTerminationCondition())
  //    'fitnessTerminationCondition
  //'cantEvalsTerminationCondition

  def seqOutputFilename = data.getSeqOutputFilename

  def parallelOutputFilename = data.getParallelOutputFilename

  def evaluatorsCount = data.getEvaluatorsCount

  def reproducersCount = data.getReproducersCount

  def evaluatorsCapacity = data.getEvaluatorsCapacity

  def reproducersCapacity = data.getReproducersCapacity

  def popSize = data.getPopSize

  def evaluations = data.getEvaluations

  def repetitions = data.getRepetitions

  def chromosomeSize = solution.varsCount

  def genInd(): TIndividual = {
    val res = new TIndividual()
    for (i <- 1 to chromosomeSize) {
      res += r.nextInt(2).asInstanceOf[Byte]
    }
    res
  }

  def loadSolution(instanceFileName: String): TMaxSAT_Problem = {

    val it = scala.io.Source.fromFile(instanceFileName).getLines()

    (1 to 5).foreach(_ => it.next())

    val spaceRE = """\s+0*"""

    val l1 = it.next()
    val f1 = l1.split(spaceRE)
    it.next()
    val l2 = it.next()
    val f2 = l2.split(spaceRE)
    val clauseLength = f1.last.toInt
    val varsCount = f2(2).toInt
    val clausesCount = f2(3).toInt

    var clauses = ArrayBuffer[List[(Byte, Int)]]()

    var l = it.next()

    while (!l.contains("%")) {

      val intValues = l.trim.split(spaceRE).map(_.toInt)
      val nEntry = intValues.map(
        i => if (i < 0) (0.asInstanceOf[Byte], -i - 1) else (1.asInstanceOf[Byte], i - 1))

      clauses += nEntry.toList

      l = it.next()
    }

    new TMaxSAT_Problem(clauseLength, varsCount, clausesCount, clauses.toList)
  }

  val solution = loadSolution("./problems/uf100-01.cnf")

  def function(ind: TIndividual): Long = {
    solution.clauses.count((c: List[(Byte, Int)]) => {
      c.exists(
        (e: (Byte, Int)) => ind(e._2) == e._1)
    })
  }

  def fitnessTerminationCondition(ind: TIndividual, fit: Long): Boolean = fit >= 420

  def changeGen(aByte: Byte): Byte = if (aByte == 0) 1.asInstanceOf[Byte] else 0.asInstanceOf[Byte]

}