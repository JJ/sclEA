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

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import com.typesafe.config.ConfigFactory

object ExperimentRun extends App {

  sheduling.ShedulingUtility.start()

  val customConf = ConfigFactory.parseString(
    """
akka {

	event-handlers = ["akka.event.slf4j.Slf4jEventHandler"]
	
	loglevel = "DEBUG"
	
	stdout-loglevel = "DEBUG"
	
	log-config-on-start = on
	
	actor {
	
		debug {
			# enable function of LoggingReceive, which is to log any received message at
			# DEBUG level
			receive = on
		}
	
	}
}
      """)

  //var system: ActorSystem = ActorSystem("pEAs", ConfigFactory.load(customConf))
  var system: ActorSystem = ActorSystem("pEAs")

  val eProfiler = system.actorOf(Props[Profiler])
  val eManager = system.actorOf(Props[Manager])

  eManager ! ('init, eProfiler, system)
  eProfiler ! ('init, eManager)

  eManager ! ('session,
    (for (_ <- 1 to 20) yield (() => Experiment.r2(eProfiler, eManager), "r2")).toList)

}