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

package ea.alg.par

import scala.language.postfixOps

import java.util.concurrent.{ArrayBlockingQueue, Executors}

import akka.actor.ActorSystem
import ea._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

object Event extends Enumeration {
  val parentSelectionEvent = Value
}

trait CSPModel extends Algorithm with Variation with Selection with Evaluation {

  val solChanCapacity = 1
  val newIndsChanCapacity = 100
  val indsEvalsChanCapacity = 100
  val popParentSelChanCapacity = 100
  val parentsChanCapacity = 100

  val solChan = new ArrayBlockingQueue[TIndEval](solChanCapacity)
  val newIndsChan = new ArrayBlockingQueue[TPopNoEval](newIndsChanCapacity)
  val indsEvalsChan = new ArrayBlockingQueue[TPopEval](indsEvalsChanCapacity)
  val popParentSelChan = new ArrayBlockingQueue[Event.Value](popParentSelChanCapacity)
  val parentsChan = new ArrayBlockingQueue[TParents](parentsChanCapacity)

  def variator(parents: TParents): TPopNoEval = {
    val newInds = parents.map(crossover).flatten
    newInds.map(mutation)
  }

  def evaluator(pop: TPopNoEval) = pop.map(ind => new TIndEval(ind, fitness(ind)))

  var survAndParentsSelectorExecutions = 0
  var survivorsSelectionExecutions = 0
  var parentsSelectionExecutions = 0

  //  var selectorCount = 0
  var evaluatorExecutions = 0
  var variatorExecutions = 0

  def log(): Unit = {
    println("survAndParentsSelectorExecutions: " + survAndParentsSelectorExecutions + "\nsurvivorsSelectionExecutions: " + survivorsSelectionExecutions
      + "\nparentsSelectionExecutions: " + parentsSelectionExecutions + "\nevaluatorExecutions: " + evaluatorExecutions + "\nvariatorExecutions: " + variatorExecutions)
  }

  val variatorsCount = 5
  val selectorsCount = 3
  val evaluatorsCount = 5

  def tournamentSize: Int

  val blockSize = 50

  var solutionReached = false

  var population: Array[TIndEval] = new Array[TIndEval](0)

  def evolve(): Unit = {
    val executor = Executors.newCachedThreadPool()
    implicit val executionContext = ExecutionContext.fromExecutor(executor)
    implicit val system = ActorSystem()

    val initPopulation = genInitPopulation()
    dividePopulation(initPopulation).foreach(newIndsChan.put)

    // pEvaluator
    for (_ <- 1 to evaluatorsCount)
      mkWorker({
        val x = newIndsChan.poll()
        if (terminationCondition()) {
          solChan.put(bestSolution)
          solutionReached = true
          akka.pattern.after(1 second, using = system.scheduler)(Future {
            //            executor.shutdown() // Until include logging in the future use
            //            system.shutdown()
            System.exit(0)
          })
        }
        else if (x != null) {
          indsEvalsChan.put(evaluator(x))
          evaluatorExecutions += 1
        }
      }, !solutionReached)

    // pSelector
    //    for (_ <- 1 to selectorsCount)
    mkWorker({
      val eParentsSelection = popParentSelChan.poll()
      val newIndsEvaluated = indsEvalsChan.poll()

      if (eParentsSelection != null && newIndsEvaluated != null) {
        if (population.size > 0)
          parentsChan.put(survAndParentsSelector(newIndsEvaluated))
        popParentSelChan.put(Event.parentSelectionEvent)
        survAndParentsSelectorExecutions += 1
      } else if (newIndsEvaluated != null) {
        if (population.size > 0) {
          population = survivorsSelection(population, newIndsEvaluated)
          popParentSelChan.put(Event.parentSelectionEvent)
        }
        else
          population = newIndsEvaluated
        survivorsSelectionExecutions += 1
      } else if (eParentsSelection != null) {
        if (population.size > 0)
          parentsChan.put(parentsSelection(population))
        popParentSelChan.put(Event.parentSelectionEvent)
        parentsSelectionExecutions += 1
      }

    }, !solutionReached)

    // pVariator
    for (_ <- 1 to variatorsCount)
      mkWorker({
        val x = parentsChan.poll()
        if (x != null) {
          variatorExecutions += 1
          newIndsChan.put(variator(x))
        }
      }, !solutionReached)

    bestSolution = solChan.take()
  }

  protected def mkWorker(actions: => Unit, cond: => Boolean)(implicit executionContext: ExecutionContext, system: ActorSystem): Unit

  var bestSolution: TIndEval = new TIndEval(null, 0)

  def dividePopulation(pop: TPopNoEval): Array[TPopNoEval] = pop.grouped(blockSize).toArray

  def survAndParentsSelector: TSelection[TPopEval, TParents]

  /*
  def survSelector(newIndsEvaluated: TPopEval): Unit = {
    // works over (read/write) the population
    population = survivorsSelection(population, newIndsEvaluated)
  }

  def parentsSelector() = {
    // works over (read) the population
    parentsSelection(population)
  }
  */

}