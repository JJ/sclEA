package pea

object problem {

  def terminationCondition: Symbol =
    'fitnessTerminationCondition
  //'cantEvalsTerminationCondition

  def problemName =
    //  'maxOne
    'maxSAT

  def function(ind: List[AnyVal]): Int = conf.function(ind)

  def fitnessTerminationCondition(ind: List[AnyVal], fit: Int): Boolean = conf.fitnessTerminationCondition(ind, fit)

  def genInitPop() = conf.genInitPop()

  def evaluatorsCount = conf.evaluatorsCount
  def evaluatorsCapacity = conf.evaluatorsCapacity
  def reproducersCount = conf.reproducersCount
  def reproducersCapacity = conf.reproducersCapacity

  def evaluations = conf.evaluations

  def changeGen(g: Any): Any = conf.changeGen(g)

  var conf: protocols.Problem =
    problemName match {
      case 'maxOne => problems.maxOnes
      case _ => problems.maxSAT
    }

}