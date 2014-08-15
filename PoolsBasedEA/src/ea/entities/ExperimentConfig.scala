package ea.entities

import ea._

import scala.util.Random

class ExperimentConfig extends BaseConfig {

  val rand = new Random()
  var ff: TFitnessFunction = _
  var qf: TQualityF = _
  var df: Tdo = _

  def setData(ff: TFitnessFunction, qf: TQualityF, df: Tdo): Unit = {
    this.ff = ff
    this.qf = qf
    this.df = df
  }
}
