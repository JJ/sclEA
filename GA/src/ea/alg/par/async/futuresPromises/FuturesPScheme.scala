package ea.alg.par.async.futuresPromises

import java.util.concurrent.{ArrayBlockingQueue, Executors}

import akka.actor.ActorSystem
import ea._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

object Event extends Enumeration {
  val parentSelectionEvent = Value
}

trait FuturesPScheme extends Algorithm with ParAlgorithm with Variation with Selection with Evaluation {

  var survAndParentsSelectorCount = 0
  var survivorsSelectionCount = 0
  var parentsSelectionCount = 0

  //  var selectorCount = 0
  var evaluatorCount = 0
  var variatorCount = 0

  def log(): Unit = {
    println("survAndParentsSelectorCount: " + survAndParentsSelectorCount + "\nsurvivorsSelectionCount: " + survivorsSelectionCount
      + "\nparentsSelectionCount: " + parentsSelectionCount + "\nevaluatorCount: " + evaluatorCount + "\nvariatorCount: " + variatorCount)
  }

  val variatorsCount = 5
  val selectorsCount = 3
  val evaluatorsCount = 5

  def tournamentSize: Int

  val blockSize = 50

  var solutionReached = false

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

  var population: Array[TIndEval] = new Array[TIndEval](0)

  def dividePopulation(pop: TPopNoEval): Array[TPopNoEval] = pop.grouped(blockSize).toArray

  def evolve(): Unit = {
    val executor = Executors.newCachedThreadPool()
    implicit val executionContext = ExecutionContext.fromExecutor(executor)
    val system = ActorSystem()

    val initPopulation = genInitPopulation()
    dividePopulation(initPopulation).foreach(newIndsChan.put)

    // pEvaluator
    for (_ <- 1 to evaluatorsCount)
      mkWorker({
        //        println("pEvaluator")
        val x = newIndsChan.poll()
        if (terminationCondition()) {
          solChan.put(bestSolution)
          solutionReached = true
          akka.pattern.after(2 second, using = system.scheduler)(Future {
            executor.shutdownNow()
            system.shutdown()
            //            System.exit(0)
          })
        }
        else if (x != null) {
          indsEvalsChan.put(evaluator(x))
          evaluatorCount += 1
        }
      }, !solutionReached)

    // pSelector
    //    for (_ <- 1 to selectorsCount)
    mkWorker({
      //        println("selector")
      val eParentsSelection = popParentSelChan.poll()
      val newIndsEvaluated = indsEvalsChan.poll()

      if (eParentsSelection != null && newIndsEvaluated != null) {
        //            println("pSelector survAndParentsSelector "+ population.size)
        if (population.size > 0)
          parentsChan.put(survAndParentsSelector(newIndsEvaluated))
        popParentSelChan.put(Event.parentSelectionEvent)
        survAndParentsSelectorCount += 1
      } else if (newIndsEvaluated != null) {
        //            println("pSelector newIndsEvaluated " + population.size)
        if (population.size > 0) {
          population = survivorsSelection(population, newIndsEvaluated)
          popParentSelChan.put(Event.parentSelectionEvent)
        }
        else
          population = newIndsEvaluated
        survivorsSelectionCount += 1
      } else if (eParentsSelection != null) {
        //            println("pSelector parentsSelection " + population.size)
        if (population.size > 0)
          parentsChan.put(parentsSelection(population))
        popParentSelChan.put(Event.parentSelectionEvent)
        parentsSelectionCount += 1
      }

    }, !solutionReached)

    // pVariator
    for (_ <- 1 to variatorsCount)
      mkWorker({
        //        println("pVariator")
        val x = parentsChan.poll()
        if (x != null) {
          variatorCount += 1
          newIndsChan.put(variator(x))
          //          println(" variatorCount " + variatorCount)
        }

      }, !solutionReached)

    bestSolution = solChan.take()
  }

  private def mkWorker(actions: => Unit, cond: => Boolean)(implicit executionContext: ExecutionContext): Unit = {
    val res = Future {
      actions
    }
    res.onSuccess {
      case _ => if (cond) {
        mkWorker(actions, cond)(executionContext)
      }
    }
    res.onFailure {
      case t =>
        println("Failure: " + t)
    }
  }

  var bestSolution: TIndEval = new TIndEval(null, 0)

  def evaluator(pop: TPopNoEval) = pop.map(ind => new TIndEval(ind, fitness(ind)))

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

  def variator(parents: TParents) = {
    val newInds = parents.map(crossover).flatten
    newInds.map(mutation)
  }

}