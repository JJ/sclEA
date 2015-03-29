package ea.selection

import ea._

object parentSelections {

  //  def fitnessProportionalSelection: TSelection = (p: TPop) => {
  //    null
  //  }
  //
  //  def rankingSelection: TSelection = (p: TPop) => {
  //    null
  //  }
  //
  //  def rouletteWheel: TSelection = (p: TPop) => {
  //    null
  //  }

  // k > 2
  def tournamentSelection(k: Int, parentsCount: Int)
                         (implicit rand: {def nextInt(n: Int): Int}): TSelection[TPopEval, TParents] =
    (p: TPopEval) => {
      (for (_ <- 1 to parentsCount) yield {
        val inds = for (_ <- 1 to k) yield p(rand.nextInt(p.size)) // take k random individuals
        val bestIndividual = inds.reduce((a, b) => if (a._2 < b._2) b else a) // best fitness
        (bestIndividual._1, p(rand.nextInt(p.size))._1) // a pair of the best and other randomly selected
      }).toArray
    }

}