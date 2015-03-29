package test

import ea.selection.parentSelections._
import ea.selection.replacementSelections._
import ea.{TInd, TIndEval}
import org.scalatest.{FlatSpec, Matchers}

class SelectionSpec extends FlatSpec with Matchers {

  "A survAndParentsSelector selection" should "select and update" in {

    val i0 = TInd(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val i1 = TInd(0, 0, 0, 0, 0, 0, 0, 0, 1)
    val i2 = TInd(0, 0, 0, 0, 0, 0, 0, 1, 0)
    val i3 = TInd(0, 0, 0, 0, 0, 0, 1, 0, 0)
    val i4 = TInd(0, 0, 0, 0, 0, 1, 0, 0, 0)
    val i5 = TInd(0, 0, 0, 0, 1, 0, 0, 0, 0)

    val r = new RandomMock(0, 1, 2, 0, 3, 0, 2, 2)
    val pop: Array[TIndEval] = Array(TIndEval(i0, 11), TIndEval(i1, 20), TIndEval(i2, 5), TIndEval(i3, 70))

    val newsInds = Array(TIndEval(i4, 3), TIndEval(i5, 8))

    survAndParentsSelector(r, pop, 3)(newsInds) should be(Array((i1, i0), (i3, i4)))

    pop should be(Array(TIndEval(i0, 11), TIndEval(i1, 20), TIndEval(i5, 8), TIndEval(i3, 70)))

  }

  "A tournamentReplacement selection" should "update" in {

    val i0 = TInd(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val i1 = TInd(0, 0, 0, 0, 0, 0, 0, 0, 1)
    val i2 = TInd(0, 0, 0, 0, 0, 0, 0, 1, 0)
    val i3 = TInd(0, 0, 0, 0, 0, 0, 1, 0, 0)
    val i4 = TInd(0, 0, 0, 0, 0, 1, 0, 0, 0)
    val i5 = TInd(0, 0, 0, 0, 1, 0, 0, 0, 0)

    val r = new RandomMock(0, 1, 2, 1, 0, 2)
    val pop: Array[TIndEval] =
      Array(TIndEval(i0, 11), TIndEval(i1, 20), TIndEval(i2, 5), TIndEval(i3, 70))

    val newsInds = Array(TIndEval(i4, 3), TIndEval(i5, 8))

    tournamentReplacement(3)(r)((pop, newsInds)) should be(Array(TIndEval(i1, 20), TIndEval(i3, 70), TIndEval(i4, 3), TIndEval(i5, 8)))

  }

  "A tournamentSelection selection" should "select" in {

    val i0 = TInd(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val i1 = TInd(0, 0, 0, 0, 0, 0, 0, 0, 1)
    val i2 = TInd(0, 0, 0, 0, 0, 0, 0, 1, 0)
    val i3 = TInd(0, 0, 0, 0, 0, 0, 1, 0, 0)
    val i4 = TInd(0, 0, 0, 0, 0, 1, 0, 0, 0)
    val i5 = TInd(0, 0, 0, 0, 1, 0, 0, 0, 0)

    val r = new RandomMock(0, 1, 2, 3, 0, 3, 2, 0)
    val pop: Array[TIndEval] =
      Array(TIndEval(i0, 11), TIndEval(i1, 20), TIndEval(i2, 5), TIndEval(i3, 70))

    tournamentSelection(3, 2)(r)(pop) should be(Array((i1, i3), (i3, i0)))

  }

}
