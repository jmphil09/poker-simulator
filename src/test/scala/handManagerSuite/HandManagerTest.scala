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

  //tests without tiebreakers

  //tests for highcard
  @Test
  def highCard() {

  }

  //tests for onepair
  @Test
  def onePair() {

  }

  //tests for twopair
  @Test
  def twoPair() {

  }

  //tests for 3 of a kind
  @Test
  def threeOfAKind() {

  }

  //tests for a straight
  @Test
  def straight {

  }

  //tests for a flush
  @Test
  def flush() {
    val flushTest1 = "3H 2H 4H 9H 8H"
    assert(findHand(flushTest1) == Flush(flushTest1))
  }

  //test for full house
  @Test
  def fullHouse() {

  }

  //test for quads
  @Test
  def fourOfAKind() {

  }

  //test for straightflush
  @Test
  def straightFlush() {
    val straightFlushTest1 = "2H 3H 4H 5H 6H 9H 9D 9C 9S 7S"
    assert(findHand("2H 3H 4H 5H 6H") === StraightFlush("2H 3H 4H 5H 6H"))
    assert(findHand("9H 9D 9C 9S 7S") === FourOfAKind("9H 9D 9C 9S 7S"))
    assert(findWinner(straightFlushTest1) === "Player 1")

  }

  //tests with tiebreakers

  //tests for highcard
  @Test
  def highCardTiebreaker() = {
    val highCardTest1 = "2H 3H 4C 5S 8C 2H 3D 4C 5S 7S"
    val highCardTest2 = "AH KH QC JS 3C AH KD QC JS 2S"
    val highCardTest3 = "AH KH QC JS 2C KH QD TC 5S 7S"
    assert(convertHand("AH KH QC JS 2C") == (Set(14, 13, 2, 12, 11), List(14, 13, 12, 11, 2)))

    assert(findWinner(highCardTest1) == "Player 1")
    assert(findWinner(highCardTest2) == "Player 1")
    assert(findWinner(highCardTest3) == "Player 1")
  }

  //tests for onepair
  @Test
  def onePairTiebreaker() {
    val onePairTest1 = "3H 3H 4C 5S 8C 2H 2D 4C 5S 7S"
    val onePairTest2 = "2H 2H 4C 5S AC 2H 2D 4C 5S 7S"
    val onePairTest3 = "3H 3H 8C 7S 6C 3H 3D 8C 5S 7S"
    assert(findWinner(onePairTest1) == "Player 1")
    assert(findWinner(onePairTest2) == "Player 1")
    assert(findWinner(onePairTest3) == "Player 1")
  }

  //tests for twopair
  @Test
  def twoPairTiebreaker() {
    val twoPairTest1 = "3H 3D 4C 4S 8C 3H 3D 4C 4S 7S"
    assert(findWinner(twoPairTest1) == "Player 1")
  }

  //tests for 3 of a kind

  //tests for a straight

  //tests for a flush

  //test for full house

  //test for quads

  //test for straightflush
  @Test
  def straightFlushTiebreaker() {
    val straightFlushTest1 = "2H 3H 4H 5H 6H 9H TH JH QH KH"
    assert(findHand("2H 3H 4H 5H 6H") == StraightFlush("2H 3H 4H 5H 6H"))
    assert(findHand("9H TH JH QH KH") == StraightFlush("9H TH JH QH KH"))
    assert(findWinner(straightFlushTest1) == "Player 2")

  }
}