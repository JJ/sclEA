package ea

import ea.entities.ExperimentConfig

import scala.collection.mutable.ArrayBuffer

object Reproducer {

  var config: ExperimentConfig = _

  def enhanceParents(pop: List[TIndEval]): TPopulation = {
    val res = new TPopulation()
    val n = pop.length
    for (i <- 0 to n - 1) {
      for (j <- 0 to n - i - 1) {
        res += pop(i)._1.clone()
      }
    }
    res
  }

  def parentsSelector(pop: TPopulation, n: Int): List[(TIndividual, TIndividual)] = {
    val res = new ArrayBuffer[(TIndividual, TIndividual)]()
    val nPar = pop.length
    for (i <- 0 to n - 1) {
      val m1 = config.rand.nextInt(nPar)
      var m2 = config.rand.nextInt(nPar)
      val i1 = pop(m1).clone()
      var i2 = pop(m2).clone()
      while (i1.eq(i2)) {
        m2 = config.rand.nextInt(nPar)
        i2 = pop(m2).clone()
      }
      res += Tuple2[TIndividual, TIndividual](i1, i2)
    }
    res.toList
  }

  def crossover(p: (TIndividual, TIndividual)): (TIndividual, TIndividual) = {
    val i1 = new TIndividual()
    val i2 = new TIndividual()
    val indLength = p._1.length
    val cPoint = config.rand.nextInt(indLength - 1)
    for (i <- 0 to cPoint) {
      i1 += p._1(i)
      i2 += p._2(i)
    }
    for (i <- cPoint + 1 to indLength - 1) {
      i1 += p._2(i)
      i2 += p._1(i)
    }
    (i1, i2)
  }

  def changeGen(aByte: Byte): Byte = if (aByte == 0) 1.asInstanceOf[Byte] else 0.asInstanceOf[Byte]

  def mutate(ind: TIndividual) {
    val pos = config.rand.nextInt(ind.length)
    ind(pos) = changeGen(ind(pos))
  }

  def reproduce(iEvals: List[TIndEval]): TPopulation = {
    val res = new TPopulation()
    val lenSubPop = iEvals.length
    val p2Rep = enhanceParents(iEvals)
    val parents = parentsSelector(p2Rep, lenSubPop / 2)
    for (ind <- parents) {
      val i = crossover(ind)
      res += i._1
      res += i._2
    }

    if (lenSubPop % 2 == 1) {
      res += iEvals(0)._1
    }

    for (i <- 0 to res.length - 1) {
      if (config.rand.nextDouble() < config.PMutation)
        mutate(res(i))
    }

    res
  }

}
