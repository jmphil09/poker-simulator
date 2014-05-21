package deckManager

import org.scalatest._

class deckManagerSuite extends FlatSpec with Matchers {
  
  "A Deck" should "contain every card" in {
    def checkDeck(cardList: List[String], deck: String): Boolean = {
      if (cardList.isEmpty) true
      else if (deck contains cardList.head) checkDeck(cardList.tail, deck)
      else false
    }
    val deckStr = createDeck
    val cardList = List("2H", "2C", "2S", "2D", "3H", "3C", "3S", "3D", "4H", "4C", "4S", "4D", "5H", "5C", "5S", "5D",
      "6H", "6C", "6S", "6D", "7H", "7C", "7S", "7D", "8H", "8C", "8S", "8D", "9H", "9C", "9S", "9D", "TH", "TC", "TS", "TD",
      "JH", "JC", "JS", "JD", "QH", "QC", "QS", "QD", "KH", "KC", "KS", "KD", "AH", "AC", "AS", "AD")
    assert(checkDeck(cardList, deckStr))
  }

  it should "have length of 155 as a string" in {
    createDeck.size should be(155)
  }
}
