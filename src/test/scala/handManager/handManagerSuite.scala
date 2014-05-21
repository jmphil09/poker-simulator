package handManager

import org.scalatest._

class handManagerSuite extends FlatSpec with Matchers {

  "Sanity Check" should "prove 0==0" in {
    0 should be (0)
  }
  
  it should "prove 1==1" in {
    1 should be (1)
  }  
  
  "ConvertCard" should "convert every card properly" in {    
    convertCards('2') should be (2)
    convertCards('3') should be (3)
    convertCards('4') should be (4)
    convertCards('5') should be (5)
    convertCards('6') should be (6)
    convertCards('7') should be (7)
    convertCards('8') should be (8)
    convertCards('9') should be (9)
    convertCards('T') should be (10)
    convertCards('J') should be (11)
    convertCards('Q') should be (12)
    convertCards('K') should be (13)
    convertCards('A') should be (14)
  }
  
  "countCopies" should "work properly" in {
    val copiesList1 = List(2,2,3,4,5)
    val copiesList2 = List(10,10,10,4,4)
    val copiesList3 = List(10,10,4,4,4)
    maxCopies(copiesList1) should be (2)
    maxCopies(copiesList2) should be (3)
    maxCopies(copiesList3) should be (3)
    val testlist1 = List(6, 2, 3, 4, 5)
    val testlist2 = List(2, 2, 3, 4, 5)
    val testlist3 = List(6, 6, 6, 4, 4)
    val testlist4 = List(6, 6, 4, 4, 5)
    maxCopies(testlist1) should be (1)
    maxCopies(testlist2) should be (2)
    maxCopies(testlist3) should be (3)
    maxCopies(testlist4) should be (2)
  }
  
  "handAsOrderedPair" should "create a set and list for a hand" in {
    val hand1 = "3H 3H 4C 5S 8C"
      handAsOrderedPair(hand1) should be ((Set(3,4,5,8),List(3,3,4,5,8)))
  }
  
  "highCardTiebreaker" should "break ties between 2 high card hands" in {
    val highCardTest1 = "2H 3H 4C 5S 8C 2H 3D 4C 5S 7S"
    val highCardTest2 = "AH KH QC JS 3C AH KD QC JS 2S"
    val highCardTest3 = "AH KH QC JS 2C KH QD TC 5S 7S"
    handAsOrderedPair("AH KH QC JS 2C") should be ((Set(14, 13, 2, 12, 11), List(14, 13, 12, 11, 2)))

    findWinner(highCardTest1) should be ("Player 1")
    findWinner(highCardTest2) should be ("Player 1")
    findWinner(highCardTest3) should be ("Player 1")
  }

  "onePair hands" should "work properly" in {
    val onePairTest1 = "3H 3S 4D 2S 6D 9S TD 2C 3C 7S"
    findHand("3H 3S 4D 2S 6D") should be (OnePair("3H 3S 4D 2S 6D"))
    findWinner(onePairTest1) should be ("Player 1")
  }

  "onePairTiebreaker" should "break ties between 2 one pair hands" in {
    val onePairTest1 = "3H 3H 4C 5S 8C 2H 2D 4C 5S 7S"
    val onePairTest2 = "2H 2H 4C 5S AC 2H 2D 4C 5S 7S"
    val onePairTest3 = "3H 3H 8C 7S 6C 3H 3D 8C 5S 7S"
    findWinner(onePairTest1) should be ("Player 1")
    findWinner(onePairTest2) should be ("Player 1")
    findWinner(onePairTest3) should be ("Player 1")
    val onePairTest4 = "9H 4D JC KS JS TH 8H 5C QS TC"
    findHand("9H 4D JC KS JS") should be (OnePair("9H 4D JC KS JS"))
    findHand("TH 8H 5C QS TC") should be (OnePair("TH 8H 5C QS TC"))
    findWinner(onePairTest4) should be ("Player 1")
  }

  "determinePair" should "find a single pair in a list" in {
    val list1 = List(9, 4, 11, 13, 11)
    val list2 = List(10, 8, 5, 12, 10)
    determinePair(list1) should be (11)
    determinePair(list2) should be (10)
  }

  "twoPair hands" should "work properly" in {
    val twoPairTest1 = "3H 3S 2D 2S 6D 9S 9D 2C 3C 7S"
    findHand("3H 3S 2D 2S 6D") should be (TwoPair("3H 3S 2D 2S 6D"))
    findWinner(twoPairTest1) should be ("Player 1")
  }

  "twoPairTiebreaker" should "break ties between 2 two pair hands" in {
    val twoPairTest1 = "3H 3D 4C 4S 8C 3H 3D 4C 4S 7S"
    findWinner(twoPairTest1) should be ("Player 1")
  }

  "threeOfAKind hands" should "work properly" in {
    val threeOfAKindTest1 = "3H 3S 3D 5S 6D 9S 9D 2C 3C 7S"
    findHand("3H 3S 3D 5S 6D") should be (ThreeOfAKind("3H 3S 3D 5S 6D"))
    findWinner(threeOfAKindTest1) should be ("Player 1")
  }

  "threeOfAKindTiebreaker" should "break ties between 2 3 of a kind hands" in {
    val threeOfAKindTest1 = "9S 9D 9C 3C 7S 3H 3S 3D 5S 6D"
    findHand("9S 9D 9C 3C 7S") should be (ThreeOfAKind("9S 9D 9C 3C 7S"))
    findHand("3H 3S 3D 5S 6D") should be (ThreeOfAKind("3H 3S 3D 5S 6D"))
    findWinner(threeOfAKindTest1) should be ("Player 1")
  }

  "straight hands" should "work properly" in {
    val straightTest1 = "3H 2H 4H 5S 6D 9S 9D 2C 3S 7S"
    findHand("3H 2H 4H 5S 6D") should be (Straight("3H 2H 4H 5S 6D"))
    findWinner(straightTest1) should be ("Player 1")
    val straightTest2 = "3H 2H 4H 5S AD 9S 9D 2C 3S 7S" 
    findHand("3H 2H 4H 5S AD")should be (Straight("3H 2H 4H 5S AD"))
  }
  
  it should "also work with wheel straights" in {
    //test highest vs lowest vs wheel
    val wheelHand = "AC 2D 3H 4S 5S"
    val lowStraight = "2H 3S 4C 5H 6H"
    val highStraight = "AD KH QH JH TH"
    findHand(wheelHand) should be (Straight(wheelHand))
    findWinner(wheelHand + " " + lowStraight) should be ("Player 2")
    findWinner(wheelHand + " " + highStraight) should be ("Player 2")
    findWinner(highStraight + " " + lowStraight) should be ("Player 1")
    //do the same for straight flushes
    val wheelFlush = "AC 2C 3C 4C 5C"
    val lowStraightFlush = "2H 3H 4H 5H 6H"
    val highStraightFlush = "AH KH QH JH TH"
    findHand(wheelFlush)should be (StraightFlush(wheelFlush))
    findWinner(wheelFlush + " " + lowStraightFlush)should be ("Player 2")
    findWinner(wheelFlush + " " + highStraightFlush)should be ("Player 2")
    findWinner(highStraightFlush + " " + lowStraightFlush)should be ("Player 1")
  }

  "straightTiebreaker" should "break ties between 2 straight hands" in {
    val straightTest1 = "9S TD JC KS QS 3H 2H 4H 5S 6D"
    findHand("9S TD JC KS QS") should be (Straight("9S TD JC KS QS"))
    findWinner(straightTest1) should be ("Player 1")
  }

  "flush hands" should "work properly" in {
    val flushTest1 = "3H 2H 4H 9H 8H 9S 9D 2C 3S 7S"
    findHand("3H 2H 4H 9H 8H") should be (Flush("3H 2H 4H 9H 8H"))
    findWinner(flushTest1) should be ("Player 1")
  }

  "flushTiebreaker" should "break ties between 2 flush hands" in {
    val flushTest1 = "3H 2H 4H 9H 8H 2S 3S 4S 5S 7S"
    findHand("3H 2H 4H 9H 8H") should be (Flush("3H 2H 4H 9H 8H"))
    findHand("2S 3S 4S 5S 7S") should be (Flush("2S 3S 4S 5S 7S"))
    findWinner(flushTest1) should be ("Player 1")
  }

  "fullHouse hands" should "work properly" in {
    val fullHouseTest1 = "2H 2D 4D 4H 4S 9H 9D 2C 3S 7S"
    findHand("2H 2D 4D 4H 4S") should be (FullHouse("2H 2D 4D 4H 4S"))
    findWinner(fullHouseTest1) should be ("Player 1")
  }

  "fullHouseTiebreaker" should "break ties between 2 full house hands" in {
    val fullHouseTest1 = "JH JD 7D 7H 7S 2H 2D 4D 4H 4S"
    findHand("JH JD 7D 7H 7S") should be (FullHouse("JH JD 7D 7H 7S"))
    findWinner(fullHouseTest1) should be ("Player 1")
    val fullHouseTest2 = "JH JD 7D 7H 7S KH KD 4D 4H 4S"
    findWinner(fullHouseTest2) should be ("Player 1")
  }

  "fourOfAKind hands" should "work proplerly" in {
    val fourOfAKindTest1 = "2H 2D 2S 2C 4S 9H 9D 3C 3S 7S"
    findHand("2H 2D 2S 2C 4S") should be (FourOfAKind("2H 2D 2S 2C 4S"))
    findWinner(fourOfAKindTest1) should be ("Player 1")
  }

  "fourOfAKindTiebreaker" should "break ties between 2 four of a kind hands" in {
    val fourOfAKindTest1 = "9H 9D 9C 9S 2S 2H 2D 2S 2C 4S"
    findHand("9H 9D 9C 9S 2S") should be (FourOfAKind("9H 9D 9C 9S 2S"))
    findWinner(fourOfAKindTest1) should be ("Player 1")
  }

  "straightFlush hands" should "work properly" in {
    val straightFlushTest1 = "2H 3H 4H 5H 6H 9H 9D 9C 9S 7S"
    findHand("2H 3H 4H 5H 6H") should be (StraightFlush("2H 3H 4H 5H 6H"))
    findHand("9H 9D 9C 9S 7S") should be (FourOfAKind("9H 9D 9C 9S 7S"))
    findWinner(straightFlushTest1) should be ("Player 1")

  }

  "straightFlushTiebreaker" should "break ties between 2 straight flush hands" in {
    val straightFlushTest1 = "2H 3H 4H 5H 6H 9H TH JH QH KH"
    findHand("2H 3H 4H 5H 6H") should be (StraightFlush("2H 3H 4H 5H 6H"))
    findHand("9H TH JH QH KH") should be (StraightFlush("9H TH JH QH KH"))
    findWinner(straightFlushTest1) should be ("Player 2")
    val straightFlushTest2 = "9H TH JH QH KH 2H 3H 4H 5H 6H"
    findWinner(straightFlushTest2) should be ("Player 1")
  }

  "handTies" should "make sure equal hands can tie" in {
    val highCardTieHand = findHand("2H 3H 4S 8D 6H")
    val onePairTieHand = findHand("2H 2D 4H 5H 6H")
    val twoPairTieHand = findHand("2H 2D 4H 4D 6H")
    val threeOfAKindTiehand = findHand("2H 2D 2S 5H 6H")
    val straightTieHand = findHand("2H 3H 4H 5D 6H")
    val flushTieHand = findHand("2H 3H 4H 5H 9H")
    val fullHouseTieHand = findHand("2H 2D 2S 5H 5D")
    val fourOfAKindTieHand = findHand("2H 2S 2D 2C 6H")
    val straightFlushTieHand = findHand("2H 3H 4H 5H 6H")

    compareHands(highCardTieHand,highCardTieHand) should be ("Tie")
    compareHands(onePairTieHand,onePairTieHand) should be ("Tie")
    compareHands(twoPairTieHand,twoPairTieHand) should be ("Tie")
    compareHands(threeOfAKindTiehand,threeOfAKindTiehand) should be ("Tie")
    compareHands(straightTieHand,straightTieHand) should be ("Tie")
    compareHands(flushTieHand,flushTieHand) should be ("Tie")
    compareHands(fullHouseTieHand,fullHouseTieHand) should be ("Tie")
    compareHands(fourOfAKindTieHand,fourOfAKindTieHand) should be ("Tie")
    compareHands(straightFlushTieHand,straightFlushTieHand) should be ("Tie")
  }

  it should "make sure straights and flushes of different suites can tie" in {
    val straight1 = findHand("2D 3H 4S 5D 6H")
    val straight2 = findHand("2H 3D 4H 5S 6D")
    val flush1 = findHand("QH 4H TH JH 2H")
    val flush2 = findHand("QS 4S TS JS 2S")
    val straightFlush1 = findHand("2H 3H 4H 5H 6H")
    val straightFlush2 = findHand("2C 3C 4C 5C 6C")

    compareHands(straight1,straight2) should be ("Tie")
    compareHands(flush2,flush1) should be ("Tie")
    compareHands(straightFlush1,straightFlush2) should be ("Tie")
  }
}
