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

package pea

import akka.actor.{Actor, ActorRef}
import seqEA.{TIndEval, TIndividual}

import scala.collection.mutable
import scala.util.Random

object Reproducer {

  def extractSubpopulation(population: Iterable[(List[AnyVal], Int)], n: Int): List[(List[AnyVal], Int)] = {
    population.toList.sortWith(
      (a: (List[AnyVal], Int), b: (List[AnyVal], Int)) =>
        a._2 > b._2).take(n)
  }

  def bestParent(pop2r: List[TIndEval]): TIndEval =
    pop2r.reduce(
      (p1: TIndEval, p2: TIndEval) => if (p1._2 > p2._2) p1 else p2)

  def mergeFunction(
                     table: mutable.HashMap[List[AnyVal], (Int, Int)],
                     subpop: Iterable[(List[AnyVal], Int)],
                     noParents: Set[(List[AnyVal], Int)],
                     nInds: Iterable[List[AnyVal]],
                     bestParents: Iterable[(List[AnyVal], Int)],
                     //                     poolSize: Int): mutable.HashMap[List[AnyVal], (Int, Int)] = {
                     poolSize: Int) = {

    val l1 = for ((i, j) <- noParents ++ bestParents ++ subpop)
    yield (i, (j, 2))

    val l2 = for (i <- nInds)
    yield (i, (-1, 1))

    val sub1 = mutable.HashMap[List[AnyVal], (Int, Int)]() ++ (l1 ++ l2)
    val table1 = table.clone() -- sub1.keys
    val cant2drop = table1.size - (poolSize - sub1.size)

    val restOlds = table1 -- table.filter(
      (a: (List[AnyVal], (Int, Int))) => a._2._2 == 2).keys.take(cant2drop)

    val more2drop = (sub1.size + restOlds.size) - poolSize

    val result =
      if (more2drop > 0) {
        restOlds -- restOlds.filter(
          (a: (List[AnyVal], (Int, Int))) => a._2._2 == 1).keys.take(more2drop)
      } else {
        restOlds
      }

    result ++ sub1
  }

  def selectPop2Reproduce(
                           subpop: List[TIndEval],
                           parentsCount: Int) = {
    def select1from3() = {
      val tuple3 = (for (i <- 1 to 3) yield subpop(r.nextInt(subpop.length))).toList
      tuple3.reduce(
        (a: TIndEval, b: TIndEval) =>
          if (a._2 > b._2) a else b)
    }

    for (_ <- 1 to parentsCount * 2) yield select1from3()
  }

  def parentsSelector(population: List[TIndEval],
                      n: Int): List[(TIndEval, TIndEval)] = {
    val positions = (for (_ <- 1 to n)
    yield (r.nextInt(population.length),
        r.nextInt(population.length))).toList

    positions.map((x) => (population(x._1), population(x._2)))
  }


  def crossover(p: (TIndividual, TIndividual)): (TIndividual, TIndividual) = {
    val i1 = new TIndividual()
    val i2 = new TIndividual()
    val indLength = p._1.length
    val cPoint = r.nextInt(indLength - 1)
    for (i <- 0 to cPoint) {
      i1 += p._1(i)
      i2 += p._2(i)
    }
    for (i <- cPoint + 1 to indLength - 1) {
      i1 += p._2(i)
      i2 += p._1(i)
    }
    (i1, i2)
  }

  def flatt(parents: Iterable[(TIndEval, TIndEval)]) =
    (for ((i, _) <- parents) yield i) ++ (for ((_, i) <- parents) yield i)

  val r = new Random()

  def evolve(subpop: List[TIndEval],
             parentsCount: Int,
             doWhenLittle: () => Unit =
             () => {}): (Boolean, (Set[TIndEval], List[TIndividual], Iterable[TIndEval])) = {

    if (subpop.size < 3) {
      doWhenLittle()
      (false, null)
    } else {
      val pop2r = selectPop2Reproduce(subpop, parentsCount)
      val parents2use = parentsSelector(pop2r.toList, parentsCount)
      val crossover1 = (a: (TIndEval, TIndEval)) => crossover(a._1._1, a._2._1)
      val nIndsByPair = parents2use.map(crossover1)
      val nInds = (for ((i, _) <- nIndsByPair) yield i) ++ (for ((_, i) <- nIndsByPair) yield i)
      val noParents = subpop.toSet diff flatt(parents2use.toList).toSet
      val bestParents = List(bestParent(pop2r.toList))

      (true, (noParents, nInds, bestParents))
    }
  }

}

class Reproducer extends Actor {

  var manager: ActorRef = _
  var profiler: ActorRef = _

  def receive = {

    case ('init, pmanager: ActorRef, pflr: ActorRef) =>
      manager = pmanager
      profiler = pflr

    case ('evolve, subpop: List[TIndEval]) =>
      val (res, resultData) =
        Reproducer.evolve(
          subpop,
          parentsCount = subpop.length / 2,
          doWhenLittle = () => {
            manager !('repEmpthyPool, self)
          })

      if (res) {
        val (noParents, nInds, bestParents) = resultData
        //        manager !('updatePool,
        //          Reproducer.mergeFunction(
        //            table, subpop,
        //            noParents, nInds,
        //            bestParents, PoolManager.poolSize))
        manager !('reproductionDone, self, nInds)
        profiler !('iteration, nInds)
      }
      else {
        manager !('reproductionDone, self, List())
      }
    //    case ('emigrateBest, pTable: mutable.HashMap[List[AnyVal], (Int, Int)], destination: ActorRef) =>
    //      val table = pTable.clone()
    //      val sels = table.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 2)
    //      if (sels.size > 0) {
    //        val res = sels.reduce(
    //          (a: (List[AnyVal], (Int, Int)), b: (List[AnyVal], (Int, Int))) =>
    //            if (a._2._2 > b._2._2) a else b)
    //
    //        destination !('migration, (res._1, res._2._1))
    //        profiler !('migration, (res._1, res._2._1), new Date().getTime)
    //      }

    case 'finalize =>
      manager !('reproducerFinalized, self)

  }

}