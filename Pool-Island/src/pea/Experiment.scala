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

import scala.util.Random
import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import scala.collection.mutable.HashMap

object Experiment {

  def init() {

    system = ActorSystem("pEAs")

    val profiler = system.actorOf(Props[Profiler], "profiler")
    val report = system.actorOf(Props[Report], "report")

    report ! ('init, profiler, system)
    profiler ! ('init, report)

    report ! ('session, List(
      (() => r1(profiler, report), "r1"),
      (() => r1(profiler, report), "r2"))
      )

  }

  var system: ActorSystem = _

  def genInd(N: Int) = {
    val r = new Random()
    (for (i <- 1 to N) yield r.nextInt(2)).mkString
  }

  def genInitPop(PopSize: Int, ChromosomeSize: Int) = for (i <- 1 to PopSize) yield genInd(ChromosomeSize)

  def r1(profiler: ActorRef, report: ActorRef) {
    val conf = new HashMap[Symbol, Any]()
    conf += ('evaluatorsCount -> 2) //10
    conf += ('evaluatorsCapacity -> 50)
    conf += ('reproducersCount -> 1)
    conf += ('reproducersCapacity -> 50) // 50
    conf += ('report -> report)
    conf += ('profiler -> profiler)

    profiler ! ('configuration, conf, 2)

    val P1 = system.actorOf(Props[PoolManager])
    val P2 = system.actorOf(Props[PoolManager])

    val m = system.actorOf(Props[Manager])
    P1 ! ('init, conf.clone ++= List(('population, genInitPop(256, 128)), ('system, system), ('manager, m)))
    P2 ! ('init, conf.clone ++= List(('population, genInitPop(256, 128)), ('system, system), ('manager, m)))

    P1 ! ('migrantsDestination, List(P2))
    P2 ! ('migrantsDestination, List(P1))

    val mconf = HashMap[Symbol, Any]()
    mconf += ('pools -> Set(P1, P2))
    mconf += ('profiler -> profiler)
    mconf += ('report -> report)
    mconf += ('system -> system)

    m ! ('init, mconf)

  }
}