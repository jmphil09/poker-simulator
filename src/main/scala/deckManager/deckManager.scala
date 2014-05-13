package deckManager

import scala.util.Random._
import scala.collection._

object deckManager {

  def shuffleDeck(): String = {

    val cardList = List("2H", "2C", "2S", "2D", "3H", "3C", "3S", "3D", "4H", "4C", "4S", "4D", "5H", "5C", "5S", "5D",
      "6H", "6C", "6S", "6D", "7H", "7C", "7S", "7D", "8H", "8C", "8S", "8D", "9H", "9C", "9S", "9D", "TH", "TC", "TS", "TD",
      "JH", "JC", "JS", "JD", "QH", "QC", "QS", "QD", "KH", "KC", "KS", "KD", "AH", "AC", "AS", "AD")

    def randomizeDeck(cardList: List[String], deck: String): String = {
      if (cardList.isEmpty) deck
      else {
        val randInt = nextInt(cardList.length)
        val randCard = cardList(randInt)
        val newCardList = cardList.filterNot(_ == randCard)

        if (deck == "") randomizeDeck(newCardList, randCard)
        else randomizeDeck(newCardList, deck ++ " " ++ randCard)
      }
    }
    
    randomizeDeck(cardList, "")
  }

  def createDeckNums(): String = {

    def createDeckNumbers(): List[Int] = {
      def updateDeck(num: Int, deck: mutable.Map[Int, Int]): mutable.Map[Int, Int] = {
        if (deck.contains(num)) {
          val n = deck(num) + 1
          deck += { num -> n }
        } else deck += { num -> 1 }
      }

      def populateDeck(counter: Int, deckMap: mutable.Map[Int, Int], deck: List[Int]): List[Int] = {
        if (counter == 0) deck
        else {
          val card = nextCard

          if (deckMap.contains(card)) {
            if (deckMap(card) < 4) populateDeck(counter - 1, updateDeck(card, deckMap), deck ++ List(card))
            else populateDeck(counter, deckMap, deck)
          } else {
            deckMap += { card -> 1 }
            populateDeck(counter - 1, deckMap, deck ++ List(card))
          }

        }
      }

      def nextCard(): Int = {
        val card = nextInt(14)
        if (card == 0) nextCard
        else if (card == 1) 14
        else card
      }

      var deck = List(0)
      var cardMap = mutable.Map(0 -> 0)

      val cards = populateDeck(52, cardMap, List())

      cards
    }

    val nums = createDeckNumbers

    def createDeckSuits(): List[Char] = {

      def numToSuit(num: Int): Char = num match {
        case 0 => 'H'
        case 1 => 'D'
        case 2 => 'C'
        case 3 => 'S'
      }

      def updateDeck(suit: Char, deck: mutable.Map[Char, Int]): mutable.Map[Char, Int] = {
        if (deck.contains(suit)) {
          val n = deck(suit) + 1
          deck += { suit -> n }
        } else deck += { suit -> 1 }
      }

      def populateSuits(counter: Int, deckMap: mutable.Map[Char, Int], deck: List[Char]): List[Char] = {

        if (counter == 0) deck
        else {
          val card = nextInt(4)
          val suit = numToSuit(card)

          if (deckMap.contains(suit)) {
            if (deckMap(suit) < 13) populateSuits(counter - 1, updateDeck(suit, deckMap), deck ++ List(suit))
            else populateSuits(counter, deckMap, deck)
          } else {
            deckMap += { suit -> 1 }
            populateSuits(counter - 1, deckMap, deck ++ List(suit))
          }
        }

      }

      val suitMap = mutable.Map('H' -> 0)
      populateSuits(52, suitMap, List())

    }

    val suits = createDeckSuits
    def createDeckStr(nums: List[Int], suits: List[Char], str: String): String = {
      if (nums.isEmpty) str
      else createDeckStr(nums.tail, suits.tail, str ++ " " ++ nums.head.toString ++ suits.head.toString)
    }

    val deck = createDeckStr(nums, suits, "").drop(1)
    deck
  }

  def createDeck(str: String): String = {

    def replaceChars(chars: String): String = chars match {
      case "10" => "T"
      case "11" => "J"
      case "12" => "Q"
      case "13" => "K"
      case "14" => "A"
      case _ => chars
    }

    var result = ""
    var i = 0
    while (i < str.length - 1) {
      val chars = replaceChars(str(i).toString ++ str(i + 1).toString)
      if (chars == "T" || chars == "J" || chars == "Q" || chars == "K" || chars == "A") {
        result += chars
        i = i + 2
      } else {
        result += str(i)
        i = i + 1
      }
    }
    result ++ str(174).toString
  }

  //val deckNums = createDeckNums

  //val deck = createDeck(deckNums)

}