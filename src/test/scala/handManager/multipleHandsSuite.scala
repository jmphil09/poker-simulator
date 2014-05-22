package handManager

import org.scalatest._

class multipleHandsSuite extends FlatSpec with Matchers {

   val handListTest2 = List(Straight("2H 3D 4S 5C 6D"), OnePair("3H 3S 4D 2S 6D"))
   val handListTest3 = handListTest2 ++ List(Straight("2H 3D 4S 5C 6D"))
   val handListTest4 = handListTest3 ++ List(FullHouse("2H 2D 2S 5C 5D"))
   val handListTest5 = handListTest4 ++ List(FullHouse("2H 2D 7S 7C 7D"))
   val handListTest6 = handListTest5 ++ List(FullHouse("2H 2D 2S 5C 5D"))
   val handListTest7 = handListTest6 ++ List(StraightFlush("TH JH QH KH AH"))

  "compareMultipleHands" should "compare 2 hands" in {
    0 should be (0)
  }

  it should "compare 3 hands" in {
    1 should be (1)
  }

  it should "compare 4 hands" in {
    1 should be (1)
  }

  it should "compare 5 hands" in {
    1 should be (1)
  }

  it should "compare 6 hands" in {
    1 should be (1)
  }

  it should "compare 7 hands" in {
    1 should be (1)
  }

  it should "compare 8 hands" in {
    1 should be (1)
  }

  it should "compare 9 hands" in {
    1 should be (1)
  }

  it should "compare 10 hands" in {
    1 should be (1)
  }
}
