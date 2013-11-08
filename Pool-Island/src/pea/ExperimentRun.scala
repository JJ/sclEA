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


object ExperimentRun extends App {

   if (args.length > 0 && args(0)=="seq")
     seqEA.seqEA_Experiment.run()
     else
       pEAExperiment.run()

}

//  val customConf = ConfigFactory.parseString(
//    """
//akka {
//
//	event-handlers = ["akka.event.slf4j.Slf4jEventHandler"]
//	
//	loglevel = "DEBUG"
//	
//	stdout-loglevel = "DEBUG"
//	
//	log-config-on-start = on
//	
//	actor {
//	
//		debug {
//			# enable function of LoggingReceive, which is to log any received message at
//			# DEBUG level
//			receive = on
//		}
//	
//	}
//}
//      """)

  //var system: ActorSystem = ActorSystem("pEAs", ConfigFactory.load(customConf))
