package deckManager

import scala.util.Random._
import scala.collection._

object deckManager {

  def createDeck(): String = {
    val cardList = List("2H", "2C", "2S", "2D", "3H", "3C", "3S", "3D", "4H", "4C", "4S", "4D", "5H", "5C", "5S", "5D",
      "6H", "6C", "6S", "6D", "7H", "7C", "7S", "7D", "8H", "8C", "8S", "8D", "9H", "9C", "9S", "9D", "TH", "TC", "TS", "TD",
      "JH", "JC", "JS", "JD", "QH", "QC", "QS", "QD", "KH", "KC", "KS", "KD", "AH", "AC", "AS", "AD")

    def shuffleDeck(cardList: List[String], deck: String): String = {
      if (cardList.isEmpty) deck
      else {
        val randInt = nextInt(cardList.length)
        val randCard = cardList(randInt)
        val newCardList = cardList.filterNot(_ == randCard)

        if (deck == "") shuffleDeck(newCardList, randCard)
        else shuffleDeck(newCardList, deck ++ " " ++ randCard)
      }
    }   
    shuffleDeck(cardList, "")
  }

}
