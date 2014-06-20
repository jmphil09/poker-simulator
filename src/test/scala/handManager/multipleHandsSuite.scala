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
   val handListTest4Tie = handListTest3Tie ++ List(Straight("2H 3D 4S 5C 6D"))
   val handListTest4SameRank = handListTest3SameRank ++ List(Straight("TH 6S 7D 8H 9C"))
   val handListTest4SameRankReverse = handListTest4SameRank.reverse

   val handListTest5 = handListTest4 ++ List(FullHouse("2H 2D 7S 7C 7D"))
   val handListTest5Tie = handListTest4Tie ++ List(Straight("2H 3D 4S 5C 6D"))
   val handListTest5SameRank = handListTest4SameRank ++ List(Straight("TH JS 7D 8H 9C"))
   val handListTest5SameRankReverse = handListTest5SameRank.reverse

   val handListTest6 = handListTest5 ++ List(FullHouse("3H 3D 2S 5C 5D"))
   val handListTest6Tie = handListTest5Tie ++ List(Straight("2H 3D 4S 5C 6D"))
   val handListTest6SameRank = handListTest5SameRank ++ List(Straight("TH JS QD 8H 9C"))
   val handListTest6SameRankReverse = handListTest6SameRank.reverse

   val handListTest7 = handListTest6 ++ List(StraightFlush("TH JH QH KH AH"))
   val handListTest7Tie = handListTest6Tie ++ List(Straight("2H 3D 4S 5C 6D"))
   val handListTest7SameRank = handListTest6SameRank ++ List(Straight("TH JS QD KH 9C"))
   val handListTest7SameRankReverse = handListTest7SameRank.reverse

   val handListTest8 = handListTest7 ++ List(StraightFlush("TH JH QH KH 9H"))
   val handListTest8Tie = handListTest7Tie ++ List(Straight("2H 3D 4S 5C 6D"))
   val handListTest8SameRank = handListTest7SameRank ++ List(Straight("TH JS QD KH AC"))
   val handListTest8SameRankReverse = handListTest8SameRank.reverse

   val handListTest9 = handListTest8 ++ List(HighCard("TH JH QH KH 4S"))
   val handListTest9Tie = handListTest8Tie ++ List(Straight("2H 3D 4S 5C 6D"))
   val handListTest9SameRank = handListTest8SameRank ++ List(Straight("4H 5S 6D 7H 8C"))
   val handListTest9SameRankReverse = handListTest9SameRank.reverse

   val handListTest10 = handListTest9 ++ List(HighCard("3D JH QH KH 2H"))
   val handListTest10Tie = handListTest9Tie ++ List(Straight("2H 3D 4S 5C 6D"))
   val handListTest10SameRank = handListTest9SameRank ++ List(Straight("4H 5S 6D 7H 8C"))
   val handListTest10SameRankReverse = handListTest10SameRank.reverse

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
    compareMultHands(addPlayerIndex(handListTest4)) should be (List(Set("Player 4"), Set("Player 1"), Set("Player 3"), Set("Player 2")))
    compareMultHands(addPlayerIndex(handListTest4Tie)) should be (List(Set("Player 1", "Player 3", "Player 2", "Player 4")))
    compareMultHands(addPlayerIndex(handListTest4SameRank)) should be (List(Set("Player 4"), Set("Player 3"), Set("Player 2"), 
      Set("Player 1")))
    compareMultHands(addPlayerIndex(handListTest4SameRankReverse)) should be (List(Set("Player 1"), Set("Player 2"), Set("Player 3"),
     Set("Player 4")))
  }

  it should "compare 5 hands" in {
    compareMultHands(addPlayerIndex(handListTest5)) should be (List(Set("Player 5"), Set("Player 4"), Set("Player 1"), Set("Player 3"),
     Set("Player 2")))
    compareMultHands(addPlayerIndex(handListTest5Tie)) should be (List(Set("Player 1", "Player 5", "Player 3", "Player 2", "Player 4")))
    compareMultHands(addPlayerIndex(handListTest5SameRank)) should be (List(Set("Player 5"), Set("Player 4"), Set("Player 3"), 
      Set("Player 2"), Set("Player 1")))
    compareMultHands(addPlayerIndex(handListTest5SameRankReverse)) should be (List(Set("Player 1"), Set("Player 2"), Set("Player 3"), 
      Set("Player 4"), Set("Player 5")))
  }

  it should "compare 6 hands" in {
    compareMultHands(addPlayerIndex(handListTest6)) should be (List(Set("Player 5"), Set("Player 6"), Set("Player 4"), Set("Player 1"), 
      Set("Player 3"), Set("Player 2")))
    compareMultHands(addPlayerIndex(handListTest6Tie)) should be (List(Set("Player 1", "Player 5", "Player 6", "Player 3", "Player 2", 
      "Player 4")))
    compareMultHands(addPlayerIndex(handListTest6SameRank)) should be (List(Set("Player 6"), Set("Player 5"), Set("Player 4"), 
      Set("Player 3"), Set("Player 2"), Set("Player 1")))
    compareMultHands(addPlayerIndex(handListTest6SameRankReverse)) should be (List(Set("Player 1"), Set("Player 2"), Set("Player 3"), 
      Set("Player 4"), Set("Player 5"), Set("Player 6")))
  }

  it should "compare 7 hands" in {
    compareMultHands(addPlayerIndex(handListTest7)) should be (List(Set("Player 7"), Set("Player 5"), Set("Player 6"), Set("Player 4"), 
      Set("Player 1"), Set("Player 3"), Set("Player 2")))
    compareMultHands(addPlayerIndex(handListTest7Tie)) should be (List(Set("Player 1", "Player 7", "Player 5", "Player 6", "Player 3", 
      "Player 2", "Player 4")))
    compareMultHands(addPlayerIndex(handListTest7SameRank)) should be (List(Set("Player 7"), Set("Player 6"), Set("Player 5"), 
      Set("Player 4"), Set("Player 3"), Set("Player 2"), Set("Player 1")))
    compareMultHands(addPlayerIndex(handListTest7SameRankReverse)) should be (List(Set("Player 1"), Set("Player 2"), Set("Player 3"), 
      Set("Player 4"), Set("Player 5"), Set("Player 6"), Set("Player 7")))
  }

  it should "compare 8 hands" in {
    compareMultHands(addPlayerIndex(handListTest8)) should be (List(Set("Player 7"), Set("Player 8"), Set("Player 5"), Set("Player 6"), 
      Set("Player 4"), Set("Player 1"), Set("Player 3"), Set("Player 2")))
    compareMultHands(addPlayerIndex(handListTest8Tie)) should be (List(Set("Player 1", "Player 7", "Player 8", "Player 5", "Player 6", 
      "Player 3", "Player 2", "Player 4")))
    compareMultHands(addPlayerIndex(handListTest8SameRank)) should be (List(Set("Player 8"), Set("Player 7"), Set("Player 6"), 
      Set("Player 5"), Set("Player 4"), Set("Player 3"), Set("Player 2"), Set("Player 1")))
    compareMultHands(addPlayerIndex(handListTest8SameRankReverse)) should be (List(Set("Player 1"), Set("Player 2"), Set("Player 3"), 
      Set("Player 4"), Set("Player 5"), Set("Player 6"), Set("Player 7"), Set("Player 8")))
  }

  it should "compare 9 hands" in {
    compareMultHands(addPlayerIndex(handListTest9)) should be (List(Set("Player 7"), Set("Player 8"), Set("Player 5"), Set("Player 6"), 
      Set("Player 4"), Set("Player 1"), Set("Player 3"), Set("Player 2"), Set("Player 9")))
    compareMultHands(addPlayerIndex(handListTest9Tie)) should be (List(Set("Player 1", "Player 7", "Player 8", "Player 5", "Player 6", 
      "Player 3", "Player 9", "Player 2", "Player 4")))
    compareMultHands(addPlayerIndex(handListTest9SameRank)) should be (List(Set("Player 8"), Set("Player 7"), Set("Player 6"), 
      Set("Player 5"), Set("Player 4"), Set("Player 3"), Set("Player 9"), Set("Player 2"), Set("Player 1")))
    compareMultHands(addPlayerIndex(handListTest9SameRankReverse)) should be (List(Set("Player 2"), Set("Player 3"), Set("Player 4"), 
        Set("Player 5"), Set("Player 6"), Set("Player 7"), Set("Player 1"), Set("Player 8"), Set("Player 9")))
  }

  it should "compare 10 hands" in {
    compareMultHands(addPlayerIndex(handListTest10)) should be (List(Set("Player 7"), Set("Player 8"), Set("Player 5"), Set("Player 6"), 
      Set("Player 4"), Set("Player 1"), Set("Player 3"), Set("Player 2"), Set("Player 9"), Set("Player 10")))
    compareMultHands(addPlayerIndex(handListTest10Tie)) should be (List(Set("Player 1", "Player 7", "Player 8", "Player 10", "Player 5", 
      "Player 6", "Player 3", "Player 9", "Player 2", "Player 4")))
    compareMultHands(addPlayerIndex(handListTest10SameRank)) should be (List(Set("Player 8"), Set("Player 7"), Set("Player 6"), 
      Set("Player 5"), Set("Player 4"), Set("Player 3"), Set("Player 9", "Player 10"), Set("Player 2"), Set("Player 1")))
    compareMultHands(addPlayerIndex(handListTest10SameRankReverse)) should be (List(Set("Player 3"), Set("Player 4"), 
        Set("Player 5"), Set("Player 6"), Set("Player 7"), Set("Player 8"), Set("Player 1", "Player 2"), Set("Player 9"), Set("Player 10")))
  }
}
