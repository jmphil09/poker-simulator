package handManager

import scala.util.Random._
import scala.collection._

import handManager._

object handManagerWorksheet {

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
  }                                               //> createDeckNums: ()String

  createDeckNums.length                           //> res0: Int = 175
  
  def createDeck(str: String): String = {
  
  	def replaceChars(chars: String): String = chars match {
  		case "10" => "T"
  		case "11" => "J"
  		case "12" => "Q"
  		case "13" => "K"
  		case "14" => "A"
  		case _ => chars
  	}
  	
  	var result = str
  	for(i <- 0 to 10){
  	//println(replaceChars(result(i).toString++result(i+1).toString))
  		val chars = replaceChars(result(i).toString++result(i+1).toString)
  		if(chars=="T"||chars=="J"||chars=="Q"||chars=="K"||chars=="A") {
  		println("YIPE"++result.dropRight(i)++"YIPE")
  			//result = result.drop(i) ++ chars ++ result.drop(i).dropRight(result.length-(i+1+1))
  		}
  		//result
  	}
  result
 }                                                //> createDeck: (str: String)String
  
  createDeck(createDeckNums)                      //> YIPE6S 11H 4H 10S 11S 10S 8H 6H 11H 9C 3S 14D 12C 6S 14D 6H 4H 11D 5C 7H 9D
                                                  //|  5H 7C 8H 4D 14D 8D 3D 10C 13S 2D 5D 12C 12C 14D 2C 9D 13C 8C 3D 7S 5H 3C 7
                                                  //| C 10C 13H 13S 4S 9S 12S 2SYIPE
                                                  //| YIPE6S 11H 4H 10S 11S 10S 8H 6H 11H 9C 3S 14D 12C 6S 14D 6H 4H 11D 5C 7H 9D
                                                  //|  5H 7C 8H 4D 14D 8D 3D 10C 13S 2D 5D 12C 12C 14D 2C 9D 13C 8C 3D 7S 5H 3C 7
                                                  //| C 10C 13H 13S 4S 9SYIPE
                                                  //| res1: String = 6S 11H 4H 10S 11S 10S 8H 6H 11H 9C 3S 14D 12C 6S 14D 6H 4H 1
                                                  //| 1D 5C 7H 9D 5H 7C 8H 4D 14D 8D 3D 10C 13S 2D 5D 12C 12C 14D 2C 9D 13C 8C 3D
                                                  //|  7S 5H 3C 7C 10C 13H 13S 4S 9S 12S 2S 2H
}