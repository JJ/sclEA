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

import scala.collection.mutable.HashMap
import akka.actor.{ Actor, ActorRef, Props, ActorSystem }

object Experiment {

  def r1(pprofiler: ActorRef, pmanager: ActorRef) {
    val conf = new HashMap[Symbol, Any]()
    conf += ('evaluatorsCount -> problem.evaluatorsCount)
    conf += ('evaluatorsCapacity -> problem.evaluatorsCapacity)
    conf += ('reproducersCount -> problem.reproducersCount)
    conf += ('reproducersCapacity -> problem.reproducersCapacity)

    conf += ('manager -> pmanager)
    conf += ('profiler -> pprofiler)

    pprofiler ! ('configuration, conf, 1)

    val p1 = ExperimentRun.system.actorOf(Props[PoolManager])

    val mIslandManager = ExperimentRun.system.actorOf(Props[IslandManager])

    p1 ! ('init, conf.clone ++= List(
      ('population, problem.genInitPop()),
      ('system, ExperimentRun.system),
      ('manager, mIslandManager)))

    p1 ! ('migrantsDestination, List(p1))

    val pools = Set[ActorRef](p1)
    val mconf = HashMap[Symbol, Any]()
    mconf += ('pools -> pools)
    mconf += ('profiler -> pprofiler)
    mconf += ('manager -> pmanager)
    mconf += ('system -> ExperimentRun.system)

    mIslandManager ! ('init, mconf)

    val poolsCount = pools.size
    val cociente = problem.evaluations / poolsCount
    val resto = problem.evaluations % poolsCount
    val (primeros, ultimos) = pools.splitAt(resto)

    for (p <- primeros)
      p ! ('initEvaluations, cociente + 1)

    for (p <- ultimos)
      p ! ('initEvaluations, cociente)

    mIslandManager ! 'start

  }

  def r2(pprofiler: ActorRef, pmanager: ActorRef) {
    val conf = new HashMap[Symbol, Any]()
    conf += ('evaluatorsCount -> problem.evaluatorsCount)
    conf += ('evaluatorsCapacity -> problem.evaluatorsCapacity)
    conf += ('reproducersCount -> problem.reproducersCount)
    conf += ('reproducersCapacity -> problem.reproducersCapacity)

    conf += ('manager -> pmanager)
    conf += ('profiler -> pprofiler)

    pprofiler ! ('configuration, conf, 2)

    val p1 = ExperimentRun.system.actorOf(Props[PoolManager])
    val p2 = ExperimentRun.system.actorOf(Props[PoolManager])

    val mIslandManager = ExperimentRun.system.actorOf(Props[IslandManager])

    p1 ! ('init, conf.clone ++= List(
      ('population, problem.genInitPop()),
      ('system, ExperimentRun.system),
      ('manager, mIslandManager)))

    p2 ! ('init, conf.clone ++= List(
      ('population, problem.genInitPop()),
      ('system, ExperimentRun.system),
      ('manager, mIslandManager)))

    p1 ! ('migrantsDestination, List(p2))
    p2 ! ('migrantsDestination, List(p1))

    val pools = Set[ActorRef](p1, p2)

    val mconf = HashMap[Symbol, Any]()

    mconf += ('pools -> pools)
    mconf += ('profiler -> pprofiler)
    mconf += ('manager -> pmanager)
    mconf += ('system -> ExperimentRun.system)

    mIslandManager ! ('init, mconf)

    val poolsCount = pools.size
    val cociente = problem.evaluations / poolsCount
    val resto = problem.evaluations % poolsCount
    val (primeros, ultimos) = pools.splitAt(resto)

    for (p <- primeros)
      p ! ('initEvaluations, cociente + 1)

    for (p <- ultimos)
      p ! ('initEvaluations, cociente)

    mIslandManager ! 'start

  }
}