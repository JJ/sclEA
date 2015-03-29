package ea.variation

import ea.{TCrossover, TInd}

object crossoverImpl {

  //  def onePoint: TCrossover = (p1: TInd, p2: TInd) => {
  //    (p2, p1)
  //  }

  def twoPoint(implicit rand: {def nextInt(n: Int): Int}): TCrossover = (parents: (TInd, TInd)) => {
    val (p1, p2) = parents
    var pto1 = rand.nextInt(p1.size - 2) + 2
    val pto2 = rand.nextInt(p1.size - 1) + 1
    if (pto2 == pto1)
      pto1 -= 1

    val (pt1, pt2) = if (pto1 > pto2) (pto2, pto1) else (pto1, pto2)

    val (part1a, part2aTemp) = p1 splitAt pt1
    val (part1b, part2bTemp) = p2 splitAt pt1
    val (part2a, part3a) = part2aTemp splitAt (pt2 - pt1)
    val (part2b, part3b) = part2bTemp splitAt (pt2 - pt1)

    Array(part1a ++ part2b ++ part3a, part1b ++ part2a ++ part3b)
  }

}
