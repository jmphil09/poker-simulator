package handManager

import multipleHands._

import org.scalatest._

class multipleHandsSuite extends FlatSpec with Matchers {

   val handListTest2 = List(Straight("2H 3D 4S 5C 6D"), OnePair("3H 3S 4D 2S 6D"))
   val handListTest2Tie = List(Straight("2H 3D 4S 5C 6D"), Straight("2H 3D 4S 5C 6D"))
   val handListTest2SameRank = List(Straight("2H 3D 4S 5C 6D"), Straight("7H 3D 4S 5C 6D"))
   val handListTest2SameRankReverse = handListTest2SameRank.reverse

   val handListTest3 = handListTest2 ++ List(Straight("2H 3D 4S 5C AD"))
   val handListTest3Tie = handListTest2Tie ++ List(Straight("2H 3D 4S 5C 6D"))
   val handListTest3SameRank = handListTest2SameRank ++ List(Straight("5H 6S 7D 8H 9C"))
   val handListTest3SameRankReverse = handListTest3SameRank.reverse

   val handListTest4 = handListTest3 ++ List(FullHouse("2H 2D 2S 5C 5D"))
   val handListTest5 = handListTest4 ++ List(FullHouse("2H 2D 7S 7C 7D"))
   val handListTest6 = handListTest5 ++ List(FullHouse("2H 2D 2S 5C 5D"))
   val handListTest7 = handListTest6 ++ List(StraightFlush("TH JH QH KH AH"))

  "compareMultipleHands" should "compare 2 hands" in {
    compareMultHands(addPlayerIndex(handListTest2)) should be (List(Set("Player 1"), Set("Player 2")))
    compareMultHands(addPlayerIndex(handListTest2Tie)) should be (List(Set("Player 1", "Player 2")))
    compareMultHands(addPlayerIndex(handListTest2SameRank)) should be (List(Set("Player 2"), Set("Player 1")))
    compareMultHands(addPlayerIndex(handListTest2SameRankReverse)) should be (List(Set("Player 1"), Set("Player 2")))
  }

  it should "compare 3 hands" in {
    compareMultHands(addPlayerIndex(handListTest3)) should be (List(Set("Player 1"), Set("Player 3"), Set("Player 2")))
    compareMultHands(addPlayerIndex(handListTest3Tie)) should be (List(Set("Player 1", "Player 3", "Player 2")))
    compareMultHands(addPlayerIndex(handListTest3SameRank)) should be (List(Set("Player 3"), Set("Player 2"), Set("Player 1")))
    compareMultHands(addPlayerIndex(handListTest3SameRankReverse)) should be (List(Set("Player 1"), Set("Player 2"), Set("Player 3")))
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
