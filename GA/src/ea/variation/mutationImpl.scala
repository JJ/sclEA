package ea.variation

import ea.TInd.changeBit
import ea.{TInd, TMutation}

object mutationImpl {

  def mutation(pMutation: Double)(implicit rand: {def nextDouble(): Double; def nextInt(n: Int): Int}): TMutation = (ind: TInd) => {
    if (rand.nextDouble() < pMutation) {
      val pos = rand.nextInt(ind.size)
      ind.update(pos, changeBit(ind(pos)))
    }
    ind
  }

}
