package handManager

import scala.util.Random._
import scala.collection._

import handManager._

object handManagerWorksheet {

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
      else card
    }

    var deck = List(0)
    var cardMap = mutable.Map(0 -> 0)

    val cards = populateDeck(52, cardMap, List())

    cards
  }                                               //> createDeckNumbers: ()List[Int]

  createDeckNumbers                               //> res0: List[Int] = List(1, 3, 12, 9, 12, 6, 11, 12, 7, 4, 3, 7, 3, 3, 2, 11,
                                                  //|  12, 10, 4, 13, 1, 1, 8, 1, 5, 6, 4, 9, 7, 7, 9, 6, 4, 5, 11, 2, 10, 5, 13,
                                                  //|  6, 11, 8, 5, 9, 10, 2, 8, 2, 8, 13, 10, 13)

  def createDeckSuits(): List[Char] = {

    def numToSuit(num: Int): Char = num match {
      case 0 => 'H'
      case 1 => 'D'
      case 2 => 'C'
      case 3 => 'S'
    }

    def populateSuits(counter: Int, deckMap: mutable.Map[Char, Int], deck: List[Char]): List[Char] = {
    
      def updateDeck(suit: Char, deck: mutable.Map[Char, Int]): mutable.Map[Char, Int] = {
        if (deck.contains(suit)) {
          val n = deck(suit) + 1
          deck += { suit -> n }
        } else deck += { suit -> 1 }
      }

      if (counter == 0) deck
      else {
        val card = nextInt(3)
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
    populateSuits(52, suitMap, List('P'))


  }                                               //> createDeckSuits: ()List[Char]| 

  createDeckSuits

}