package deckManagerSuite

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit._

import deckManager.deckManager._
import deckManager._

class deckManager extends AssertionsForJUnit {

  @Test
  def sanityCheck() {
    assert(0 === 0)
    assert(1 === 1)
  }

  //tests for createDeckNums()
  @Test
  def createDeckNumsTest() {
    val numStr = createDeckNums
    assert(numStr.length === 175)
    //numStr should contain the following character counts
    //0: 4, 1: 24, 2: 8, 3: 8, 4: 8, 5: 4, 6: 4, 7: 4, 8: 4, 9: 4, _: 103
    assert(numStr.count(_ =='0')===4)
    assert(numStr.count(_ =='1')===24)
    assert(numStr.count(_ =='2')===8)
    assert(numStr.count(_ =='3')===8)
    assert(numStr.count(_ =='4')===8)
    assert(numStr.count(_ =='5')===4)
    assert(numStr.count(_ =='6')===4)
    assert(numStr.count(_ =='7')===4)
    assert(numStr.count(_ =='8')===4)
    assert(numStr.count(_ =='9')===4)
    assert(numStr.count(_ ==' ')===51)
    assert(numStr.count(_ =='D')===13)
    assert(numStr.count(_ =='C')===13)
    assert(numStr.count(_ =='H')===13)
    assert(numStr.count(_ =='S')===13)
    assert(numStr.count(_ =='T')===0)
    assert(numStr.count(_ =='J')===0)
    assert(numStr.count(_ =='Q')===0)
    assert(numStr.count(_ =='K')===0)
    assert(numStr.count(_ =='A')===0)
  }

  //tests for createDeck
  @Test
  def createDeckTest() {
    val deckStr = createDeck(createDeckNums)
    assert(createDeck(createDeckNums).length === 155)
    //deckStr should contain the following character counts
    //2: 4, 3: 4, 4: 4, 5: 4, 6: 4, 7: 4, 8: 4, 9: 4, T: 4, J: 4, Q: 4, K: 4, A: 4, _: 51
    assert(deckStr.count(_ =='0')===0)
    assert(deckStr.count(_ =='2')===4)
    assert(deckStr.count(_ =='3')===4)
    assert(deckStr.count(_ =='4')===4)
    assert(deckStr.count(_ =='5')===4)
    assert(deckStr.count(_ =='6')===4)
    assert(deckStr.count(_ =='7')===4)
    assert(deckStr.count(_ =='8')===4)
    assert(deckStr.count(_ =='9')===4)
    assert(deckStr.count(_ ==' ')===51)
    assert(deckStr.count(_ =='D')===13)
    assert(deckStr.count(_ =='C')===13)
    assert(deckStr.count(_ =='H')===13)
    assert(deckStr.count(_ =='S')===13)
    assert(deckStr.count(_ =='T')===4)
    assert(deckStr.count(_ =='J')===4)
    assert(deckStr.count(_ =='Q')===4)
    assert(deckStr.count(_ =='K')===4)
    assert(deckStr.count(_ =='A')===4)
  }

  //test to make sure deck is complete and contains no duplicates
  @Test
  def completeDeck() {
    
    def checkDeck(cardList: List[String], deck: String): Boolean = {
    if(cardList.isEmpty) true
    else if(deck contains cardList.head) checkDeck(cardList.tail, deck)
    else false
  }

    val deckStr = shuffleDeck
    val cardList = List("2H", "2C", "2S", "2D", "3H", "3C", "3S", "3D", "4H", "4C", "4S", "4D", "5H", "5C", "5S", "5D",
     "6H", "6C", "6S", "6D", "7H", "7C", "7S", "7D", "8H", "8C", "8S", "8D", "9H", "9C", "9S", "9D", "TH", "TC", "TS", "TD", 
     "JH", "JC", "JS", "JD", "QH", "QC", "QS", "QD", "KH", "KC", "KS", "KD", "AH", "AC", "AS", "AD")
    
    assert(checkDeck(cardList, deckStr))
  }
  
  @Test
  def shuffleDeckTest() {
    assert(shuffleDeck.size==155)
  }


}