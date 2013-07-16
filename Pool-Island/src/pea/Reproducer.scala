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
import akka.actor.{ Actor, Props, ActorSystem, ActorRef }
import scala.collection.mutable.HashMap
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import sun.security.util.Length

class Reproducer extends Actor {

  val r = new Random()

  var manager: ActorRef = _
  var profiler: ActorRef = _

  def extractSubpopulation(pTable: HashMap[String, (Int, Int)], n: Int) = {
    val table = pTable.clone
    val a = table.filter((a: (String, (Int, Int))) => a._2._2 == 2).keys.toList
    val sels = (a.map(i => (i, table(i)._1))).toList
    sels.sortWith(
      (p1: (String, Int), p2: (String, Int)) => p1._2 > p2._2).take(n)
  }

  def selectPop2Reproduce(subpop: List[(String, Int)], parentsCount: Int) = {

    def select1from3() = {
      val r3 = (for (i <- 1 to 3) yield subpop(r.nextInt(subpop.length))).toList
      r3.foldLeft(r3(0))(
        (a: (String, Int), b: (String, Int)) =>
          if (a._2 > b._2) a else b)
    }

    (for (_ <- 1 to parentsCount * 2) yield select1from3()).toList
  }

  def parentsSelector(pop: List[(String, Int)], n: Int): List[((String, Int), (String, Int))] = {
    val positions = (for (_ <- (1 to n)) yield (r.nextInt(pop.length), r.nextInt(pop.length))).toList
    positions.map((x) => (pop(x._1), pop(x._2)))
  }

  /*  def parentsSelector(pop: List[(String, Int)], n: Int): List[((String, Int), (String, Int))] = {
	  val i1 = pop.head
			  val rest = pop.tail
			  if (rest.isEmpty) {
				  List((i1, i1))
			  } else {
				  val n1 = r.nextInt(rest.length)
						  val head = (i1, rest(n1))
						  if (n == 1)
							  List(head)
							  else {
								  val (a, b) = rest.splitAt(n1)
										  (head :: parentsSelector(a ::: b.tail, n - 1))
							  }
			  }
  }
*/
  def crossover(parents: ((String, Int), (String, Int))): (String, String) = {

    val ((ind1, _), (ind2, _)) = parents

    val crossPoint = r.nextInt(ind1.length)
    val (a1, a2) = ind1.splitAt(crossPoint + 1)
    val (b1, b2) = ind2.splitAt(crossPoint + 1)
    val child1 = a1 + b2

    val muttationPoint = r.nextInt(ind1.length)
    val (m1, m2) = child1.splitAt(muttationPoint - 1)
    val b3 = m1 + changeB(m2.head) + m2.tail
    (b3, b1 + a2)
  }

  def changeB(b: Character) = if (b == '1') '0' else '1'

  def flatt(parents: List[((String, Int), (String, Int))]) =
    (for ((i, _) <- parents) yield i) ++ (for ((_, i) <- parents) yield i)

  //  def bestParent(pop2r: List[(String, Int)]) = pop2r.sortWith(
  //    (p1: (String, Int), p2: (String, Int)) => p1._2 > p2._2).head

  def bestParent(pop2r: List[(String, Int)]): (String, Int) =
    pop2r.tail.foldRight(pop2r.head)((p1: (String, Int), p2: (String, Int)) => if (p1._2 > p2._2) p1 else p2)

  def receive = {

    case ('init, pmanager: ActorRef, pflr: ActorRef) =>
      manager = pmanager
      profiler = pflr

    case ('evolve, table: HashMap[String, (Int, Int)], n: Int) =>
      val subpop = extractSubpopulation(table, n)

      if (subpop.length < 3)
        manager ! ('repEmpthyPool, self)
      else {
        val parentsCount = n / 2
        val pop2r = selectPop2Reproduce(subpop, parentsCount)
        val parents = parentsSelector(pop2r, parentsCount)
        val nInds = parents.map(crossover)
        val nIndsFlatt = ((for ((i, _) <- nInds) yield i) ++ (for ((_, i) <- nInds) yield i)).toSet
        val noParents = subpop.toSet -- flatt(parents).toSet
        val bestParents = List(bestParent(pop2r))

        manager ! ('updatePool, updatePool(table, subpop, noParents, nIndsFlatt, bestParents))
        manager ! ('evolveDone, self)
        manager ! ('iteration, nIndsFlatt)

      }

    case ('emigrateBest, pTable: HashMap[String, (Int, Int)], destination: ActorRef) =>
      val table = pTable.clone
      val sels = table.filter((a: (String, (Int, Int))) => a._2._2 == 2)
      if (sels.size > 0) {
        val res = sels.foldLeft(sels.head)(
          (a: (String, (Int, Int)), b: (String, (Int, Int))) =>
            if (a._2._2 > b._2._2) a else b)
        destination ! ('migration, (res._1, res._2._1))
        profiler ! ('migration, (res._1, res._2._1), new Date().getTime)
      }

    case 'finalize =>
      manager ! ('reproducerFinalized, self)

  }

  def updatePool(
    table: HashMap[String, (Int, Int)], subpop: Iterable[(String, Int)],
    noParents: Iterable[(String, Int)], nIndsFlatt: Set[String],
    bestParents: Iterable[(String, Int)]): HashMap[String, (Int, Int)] = {

    val res = table.clone
    val newParents = noParents ++ bestParents
    val lNewParents = newParents.map((a: (String, Int)) => (a._1, (a._2, 2)))
    val lNewInds = nIndsFlatt.map((_, (-1, 1)))

    res --= subpop.map(_._1)
    res ++= lNewParents
    res ++= lNewInds

    res

  }

}