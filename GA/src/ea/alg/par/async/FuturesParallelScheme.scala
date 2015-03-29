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

package ea.alg.par.async

import akka.actor.ActorSystem

import scala.concurrent.{ExecutionContext, Future}

trait FuturesParallelScheme {

  def mkWorker(actions: => Unit, cond: => Boolean)(implicit executionContext: ExecutionContext, system: ActorSystem): Unit = {
    val res = Future {
      actions
    }
    res.onSuccess {
      case _ => if (cond) {
        mkWorker(actions, cond) //(executionContext)
      }
    }
    res.onFailure {
      case t =>
        println("Failure: " + t)
    }
  }

}
