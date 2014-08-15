package ea

import scala.collection.mutable.ArrayBuffer

class TIndividual extends ArrayBuffer[Byte] {}

class TPopulation extends ArrayBuffer[TIndividual] {}

class TIndEval(p1: TIndividual, p2: Long) extends Tuple2[TIndividual, Long](p1, p2) {}

abstract class TFitnessFunction extends (TIndividual => Long) {}

abstract class TQualityF extends (Long => Boolean)

abstract class Tdo extends (TIndEval => Unit)

