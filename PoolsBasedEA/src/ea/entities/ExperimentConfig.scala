package ea.entities

import ea._

import scala.util.Random

class ExperimentConfig extends BaseConfig {

  val rand = new Random()
  var ff: TIndividual => Long = _
  var qf: Long => Boolean = v => false
  var df: TIndEval => Unit = (iEval) => {}

  def setData(ff: TIndividual => Long, qf: Long => Boolean, df: TIndEval => Unit) {
    this.ff = ff
    this.qf = qf
    this.df = df
  }
}
