package handManagerSuite

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit._

import handManager.handManager._

class HandManagerTest extends AssertionsForJUnit {

  @Test
  def sanityCheck() {
    assert(0 === 0)
    assert(1 === 1)
  }

  //tests for convertCards
  @Test
  def convertCardTest() {
    assert(convertCards('2')===2)
    assert(convertCards('3')===3)
    assert(convertCards('4')===4)
    assert(convertCards('5')===5)
    assert(convertCards('6')===6)
    assert(convertCards('7')===7)
    assert(convertCards('8')===8)
    assert(convertCards('9')===9)
    assert(convertCards('T')===10)
    assert(convertCards('J')===11)
    assert(convertCards('Q')===12)
    assert(convertCards('K')===13)
    assert(convertCards('A')===14)
  }
  
  //tests for countCopies
  @Test
  def countCopiesTest() {
    val copiesList1 = List(2,2,3,4,5)
    val copiesList2 = List(10,10,10,4,4)
    val copiesList3 = List(10,10,4,4,4)
    assert(countCopies(copiesList1,copiesList1,0)===2)
    assert(countCopies(copiesList2,copiesList2,0)===3)
    assert(countCopies(copiesList3,copiesList3,0)===3)
    val testlist1 = List(6, 2, 3, 4, 5)
    val testlist2 = List(2, 2, 3, 4, 5)
    val testlist3 = List(6, 6, 6, 4, 4)
    val testlist4 = List(6, 6, 4, 4, 5)
    assert(countCopies(testlist1, testlist1, 0) === 1)
    assert(countCopies(testlist2, testlist2, 0) === 2)
    assert(countCopies(testlist3, testlist3, 0) === 3)
    assert(countCopies(testlist4, testlist4, 0) === 2)
  }
  
  //tests for convertHand
  @Test
  def convertHandTest() {
    val hand1 = "3H 3H 4C 5S 8C"
      assert(convertHand(hand1)===(Set(3,4,5,8),List(3,3,4,5,8)))
  }
  
  //tests for highcard
  @Test
  def highCardTiebreaker() = {
    val highCardTest1 = "2H 3H 4C 5S 8C 2H 3D 4C 5S 7S"
    val highCardTest2 = "AH KH QC JS 3C AH KD QC JS 2S"
    val highCardTest3 = "AH KH QC JS 2C KH QD TC 5S 7S"
    assert(convertHand("AH KH QC JS 2C") === (Set(14, 13, 2, 12, 11), List(14, 13, 12, 11, 2)))

    assert(findWinner(highCardTest1) === "Player 1")
    assert(findWinner(highCardTest2) === "Player 1")
    assert(findWinner(highCardTest3) === "Player 1")
  }

  //tests for onepair
  @Test
  def onePair() {
    val onePairTest1 = "3H 3S 4D 2S 6D 9S TD 2C 3C 7S"
    assert(findHand("3H 3S 4D 2S 6D") === OnePair("3H 3S 4D 2S 6D"))
    assert(findWinner(onePairTest1) === "Player 1")
  }

  @Test
  def onePairTiebreaker() {
    val onePairTest1 = "3H 3H 4C 5S 8C 2H 2D 4C 5S 7S"
    val onePairTest2 = "2H 2H 4C 5S AC 2H 2D 4C 5S 7S"
    val onePairTest3 = "3H 3H 8C 7S 6C 3H 3D 8C 5S 7S"
    assert(findWinner(onePairTest1) === "Player 1")
    assert(findWinner(onePairTest2) === "Player 1")
    assert(findWinner(onePairTest3) === "Player 1")
    val onePairTest4 = "9H 4D JC KS JS TH 8H 5C QS TC"
    assert(findHand("9H 4D JC KS JS") === OnePair("9H 4D JC KS JS"))
    assert(findHand("TH 8H 5C QS TC") === OnePair("TH 8H 5C QS TC"))
    assert(findWinner(onePairTest4) === "Player 1")
  }

  @Test
  def determinePairTest() {
    val list1 = List(9, 4, 11, 13, 11)
    val list2 = List(10, 8, 5, 12, 10)
    assert(determinePair(list1) === 11)
    assert(determinePair(list2) === 10)
  }

  //tests for twopair
  @Test
  def twoPair() {
    val twoPairTest1 = "3H 3S 2D 2S 6D 9S 9D 2C 3C 7S"
    assert(findHand("3H 3S 2D 2S 6D") === TwoPair("3H 3S 2D 2S 6D"))
    assert(findWinner(twoPairTest1) === "Player 1")
  }

  @Test
  def twoPairTiebreaker() {
    val twoPairTest1 = "3H 3D 4C 4S 8C 3H 3D 4C 4S 7S"
    assert(findWinner(twoPairTest1) === "Player 1")
  }

  //tests for 3 of a kind
  @Test
  def threeOfAKind() {
    val threeOfAKindTest1 = "3H 3S 3D 5S 6D 9S 9D 2C 3C 7S"
    assert(findHand("3H 3S 3D 5S 6D") === ThreeOfAKind("3H 3S 3D 5S 6D"))
    assert(findWinner(threeOfAKindTest1) === "Player 1")
  }

  @Test
  def threeOfAKindTiebreaker() {
    val threeOfAKindTest1 = "9S 9D 9C 3C 7S 3H 3S 3D 5S 6D"
    assert(findHand("9S 9D 9C 3C 7S") === ThreeOfAKind("9S 9D 9C 3C 7S"))
    assert(findHand("3H 3S 3D 5S 6D") === ThreeOfAKind("3H 3S 3D 5S 6D"))
    assert(findWinner(threeOfAKindTest1) === "Player 1")
  }

  //tests for a straight
  @Test
  def straight {
    val straightTest1 = "3H 2H 4H 5S 6D 9S 9D 2C 3S 7S"
    assert(findHand("3H 2H 4H 5S 6D") === Straight("3H 2H 4H 5S 6D"))
    assert(findWinner(straightTest1) === "Player 1")
    val straightTest2 = "3H 2H 4H 5S AD 9S 9D 2C 3S 7S"
    //TODO: Fix wheel straight  
    //assert(findHand("3H 2H 4H 5S AD")===Straight("3H 2H 4H 5S AD"))
  }

  @Test
  def straightTiebreaker {
    val straightTest1 = "9S TD JC KS QS 3H 2H 4H 5S 6D"
    assert(findHand("9S TD JC KS QS") === Straight("9S TD JC KS QS"))
    assert(findWinner(straightTest1) === "Player 1")
  }

  //tests for a flush
  @Test
  def flush() {
    val flushTest1 = "3H 2H 4H 9H 8H 9S 9D 2C 3S 7S"
    assert(findHand("3H 2H 4H 9H 8H") === Flush("3H 2H 4H 9H 8H"))
    assert(findWinner(flushTest1) === "Player 1")
  }

  @Test
  def flushTiebreaker() {
    val flushTest1 = "3H 2H 4H 9H 8H 2S 3S 4S 5S 7S"
    assert(findHand("3H 2H 4H 9H 8H") === Flush("3H 2H 4H 9H 8H"))
    assert(findHand("2S 3S 4S 5S 7S") === Flush("2S 3S 4S 5S 7S"))
    assert(findWinner(flushTest1) === "Player 1")
  }

  //test for full house
  @Test
  def fullHouse() {
    val fullHouseTest1 = "2H 2D 4D 4H 4S 9H 9D 2C 3S 7S"
    assert(findHand("2H 2D 4D 4H 4S") === FullHouse("2H 2D 4D 4H 4S"))
    assert(findWinner(fullHouseTest1) === "Player 1")
  }

  @Test
  def fullHouseTiebreaker() {
    val fullHouseTest1 = "JH JD 7D 7H 7S 2H 2D 4D 4H 4S"
    assert(findHand("JH JD 7D 7H 7S") === FullHouse("JH JD 7D 7H 7S"))
    assert(findWinner(fullHouseTest1) === "Player 1")
    val fullHouseTest2 = "JH JD 7D 7H 7S KH KD 4D 4H 4S"
    assert(findWinner(fullHouseTest2) === "Player 1")
  }

  //test for quads
  @Test
  def fourOfAKind() {
    val fourOfAKindTest1 = "2H 2D 2S 2C 4S 9H 9D 3C 3S 7S"
    assert(findHand("2H 2D 2S 2C 4S") === FourOfAKind("2H 2D 2S 2C 4S"))
    assert(findWinner(fourOfAKindTest1) === "Player 1")
  }

  @Test
  def fourOfAKindTiebreaker() {
    val fourOfAKindTest1 = "9H 9D 9C 9S 2S 2H 2D 2S 2C 4S"
    assert(findHand("9H 9D 9C 9S 2S") === FourOfAKind("9H 9D 9C 9S 2S"))
    assert(findWinner(fourOfAKindTest1) === "Player 1")
  }

  //test for straightflush
  @Test
  def straightFlush() {
    val straightFlushTest1 = "2H 3H 4H 5H 6H 9H 9D 9C 9S 7S"
    assert(findHand("2H 3H 4H 5H 6H") === StraightFlush("2H 3H 4H 5H 6H"))
    assert(findHand("9H 9D 9C 9S 7S") === FourOfAKind("9H 9D 9C 9S 7S"))
    assert(findWinner(straightFlushTest1) === "Player 1")

  }

  @Test
  def straightFlushTiebreaker() {
    val straightFlushTest1 = "2H 3H 4H 5H 6H 9H TH JH QH KH"
    assert(findHand("2H 3H 4H 5H 6H") === StraightFlush("2H 3H 4H 5H 6H"))
    assert(findHand("9H TH JH QH KH") === StraightFlush("9H TH JH QH KH"))
    assert(findWinner(straightFlushTest1) === "Player 2")
    val straightFlushTest2 = "9H TH JH QH KH 2H 3H 4H 5H 6H"
    assert(findWinner(straightFlushTest2) === "Player 1")
  }

}