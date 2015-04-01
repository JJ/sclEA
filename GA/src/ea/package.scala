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


package object ea {

  import scala.util.Random

  type TInd = Array[Byte]

  object TInd {
    def apply(ls: Byte*): TInd = ls.toArray

    def changeBit(b: Byte): Byte = if (b == 0) 1 else 0
  }

  type TPopNoEval = Array[TInd]
  type TFitness = TInd => Long
  type TCrossover = ((TInd, TInd)) => TPopNoEval
  type TMutation = TInd => TInd
  type TQualityFitness = Long => Boolean
  type TParents = Array[(TInd, TInd)]

  class TIndEval(ind: TInd, fit: Long) extends Tuple2[TInd, Long](ind, fit) {
    override def toString(): String = "[" + this._1.mkString("") + " : " + this._2.toString + "]"
  }

  object TIndEval {
    def apply(ind: TInd, fit: Long) = new TIndEval(ind, fit)
  }


  type TPopEval = Array[TIndEval]
  type TSelection[I, O] = I => O

  trait Evaluation {
    def fitness: TFitness

    def qualityFitness: TQualityFitness
  }

  trait Variation {
    def crossover: TCrossover

    def mutation: TMutation
  }

  trait Selection {
    def parentsSelection: TSelection[TPopEval, TParents]

    def survivorsSelection: TSelection[(TPopEval, TPopEval), TPopEval]
  }

  trait Algorithm {
    def terminationCondition: () => Boolean

    def genInitPopulation(): TPopNoEval

    def genIndividual(): TInd

    def popSize: Int

    def chromosomeSize: Int

    def maxNumberOfEvals: Int

    implicit val rand = new Random()
  }

}