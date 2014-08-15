package ea

import ea.entities.ExperimentConfig

import scala.collection.mutable.ArrayBuffer

object Evaluator {

  var config: ExperimentConfig = _

  def evaluate(p2Eval: List[TIndividual]) = {
    val res = ArrayBuffer[TIndEval]()
    var mejorEncontrado = false
    var i = 0
    while (i < p2Eval.length && !mejorEncontrado) {
      val sol = config.ff(p2Eval(i))
      val nEntry = new TIndEval(p2Eval(i), sol)
      if (config.qf(sol)) {
        mejorEncontrado = true
        config.df(nEntry)
      }
      res += nEntry
      i += 1
    }
    res
  }

}
