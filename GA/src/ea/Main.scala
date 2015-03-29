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

package ea

import ea.alg.par.async._
import ea.problem._

object Main extends App {

  //  for (_ <- 1 to 10) {
  //    val obj = new MaxOnesSeqConfiguration
  //    obj.evolve()
  //    println("Best solution: " + obj.bestSolution)
  //    println("Evaluations: " + obj.evaluations)
  //  }

  val obj = new MaxOnesParallelConfiguration with ActorsParallelScheme
//    val obj = new MaxOnesParallelConfiguration with FuturesParallelScheme

  obj.evolve()

  obj.log()
  println()
  println("Best solution: " + obj.bestSolution)
  println("Evaluations: " + obj.evaluations)

}