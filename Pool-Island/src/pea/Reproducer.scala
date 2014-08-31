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

import java.util.Date

import akka.actor.{Actor, ActorRef}

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.util.Random

object Reproducer {

  def extractSubpopulation(population: List[(List[AnyVal], Int)], n: Int): List[(List[AnyVal], Int)] = {
    population.sortWith(
      (a: (List[AnyVal], Int), b: (List[AnyVal], Int)) =>
        a._2 > b._2).take(n)
  }

  def bestParent(pop2r: List[(List[AnyVal], Int)]): (List[AnyVal], Int) =
    pop2r.reduce(
      (p1: (List[AnyVal], Int), p2: (List[AnyVal], Int)) => if (p1._2 > p2._2) p1 else p2)

  def mergeFunction(
                     table: mutable.HashMap[List[AnyVal], (Int, Int)],
                     subpop: Iterable[(List[AnyVal], Int)],
                     noParents: Set[(List[AnyVal], Int)],
                     nInds: Iterable[List[AnyVal]],
                     bestParents: Iterable[(List[AnyVal], Int)],
                     poolSize: Int): mutable.HashMap[List[AnyVal], (Int, Int)] = {

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
                           subpop: List[(List[AnyVal], Int)],
                           parentsCount: Int) = {
    def select1from3() = {
      val tuple3 = (for (i <- 1 to 3) yield subpop(r.nextInt(subpop.length))).toList
      tuple3.reduce(
        (a: (List[AnyVal], Int), b: (List[AnyVal], Int)) =>
          if (a._2 > b._2) a else b)
    }

    for (_ <- 1 to parentsCount * 2) yield select1from3()
  }

  def parentsSelector(population: List[(List[AnyVal], Int)],
                      n: Int): List[((List[AnyVal], Int), (List[AnyVal], Int))] = {
    val positions = (for (_ <- 1 to n)
    yield (r.nextInt(population.length),
        r.nextInt(population.length))).toList

    positions.map((x) => (population(x._1), population(x._2)))
  }

  def crossover(parents: ((List[AnyVal], Int), (List[AnyVal], Int))): (List[AnyVal], List[AnyVal]) = {

    val ((ind1, _), (ind2, _)) = parents
    val indLength = ind1.size
    val crossPoint = r.nextInt(indLength)

    val (a1, a2) = ind1.splitAt(crossPoint + 1)
    val (b1, b2) = ind2.splitAt(crossPoint + 1)

    val child1 = a1 ++ b2

    val muttationPoint = r.nextInt(indLength)

    val (m1, m2) = child1.splitAt(muttationPoint - 1)

    val b3 = m1 ++ List(problem.changeGen(m2.head)) ++ m2.tail

    (b3.asInstanceOf[List[AnyVal]], b1 ++ a2)
  }

  def flatt(parents: Iterable[((List[AnyVal], Int), (List[AnyVal], Int))]) =
    (for ((i, _) <- parents) yield i) ++ (for ((_, i) <- parents) yield i)

  val r = new Random()

  def evolve(
              subpop: List[(List[AnyVal], Int)],
              parentsCount: Int,
              doWhenLittle: () => Unit = () => {}): (Boolean, (Set[(List[AnyVal], Int)], Iterable[List[AnyVal]], Iterable[(List[AnyVal], Int)])) = {

    if (subpop.size < 3) {
      doWhenLittle()
      (false, null)
    } else {
      val pop2r = selectPop2Reproduce(subpop, parentsCount)
      val parents2use = parentsSelector(pop2r.toList, parentsCount)
      val nIndsByPair = parents2use.map(crossover)
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

    case ('evolve, pTable: mutable.HashMap[List[AnyVal], (Int, Int)], n: Int) =>
      val table = pTable.clone()
      val tFiltered = table.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 2).keys.toList
      val population = tFiltered.map(i => (i, table(i)._1))

      val subpop = Reproducer.extractSubpopulation(population, n)

      val (res, resultData) =
        Reproducer.evolve(
          subpop,
          parentsCount = n / 2,
          doWhenLittle = () => {
            manager !('repEmpthyPool, self)
          })

      if (res) {
        val (noParents, nInds, bestParents) = resultData
        manager !('updatePool,
          Reproducer.mergeFunction(
            table, subpop,
            noParents, nInds,
            bestParents, PoolManager.poolSize))
        manager !('evolveDone, self)
        profiler !('iteration, nInds)
      }

    case ('emigrateBest, pTable: mutable.HashMap[List[AnyVal], (Int, Int)], destination: ActorRef) =>
      val table = pTable.clone()
      val sels = table.filter((a: (List[AnyVal], (Int, Int))) => a._2._2 == 2)
      if (sels.size > 0) {
        val res = sels.reduce(
          (a: (List[AnyVal], (Int, Int)), b: (List[AnyVal], (Int, Int))) =>
            if (a._2._2 > b._2._2) a else b)

        destination !('migration, (res._1, res._2._1))
        profiler !('migration, (res._1, res._2._1), new Date().getTime)
      }

    case 'finalize =>
      manager !('reproducerFinalized, self)

  }

}