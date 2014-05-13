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
  	while(i < str.length-1){
  		val chars = replaceChars(str(i).toString++str(i+1).toString)
  		if(chars=="T"||chars=="J"||chars=="Q"||chars=="K"||chars=="A") {
				result += chars
				i = i+2
  		}
  		else {
  		result += str(i)
  		i=i+1
  		}
  	}
  result++str(174).toString
 }                                                //> createDeck: (str: String)String
 
 val deckNums = createDeckNums                    //> deckNums  : String = 8H 14D 8S 13H 10H 5C 13D 6S 14S 8S 10H 4C 11D 7S 10C 1
                                                  //| 3C 7H 2S 14D 2C 11C 6D 12H 6H 3D 7C 11C 2S 11H 5D 14S 13C 4S 2H 12D 6S 12D 
                                                  //| 9C 3C 3S 8H 9H 9S 10C 12D 4H 9D 7D 5D 4S 5H 3C
  
 val deck = createDeck(deckNums)                  //> deck  : String = 8H AD 8S KH TH 5C KD 6S AS 8S TH 4C JD 7S TC KC 7H 2S AD 2
                                                  //| C JC 6D QH 6H 3D 7C JC 2S JH 5D AS KC 4S 2H QD 6S QD 9C 3C 3S 8H 9H 9S TC Q
                                                  //| D 4H 9D 7D 5D 4S 5H 3C
 
 def popCard(deck: String, hand: String, counter: Int): (String,String) = {
 	var cards = hand ++ " " ++ (deck.dropRight(deck.length-2))
 	var newDeck = deck.drop(3)
 	
 	if(counter==1) (cards.drop(1),newDeck)
 	else popCard(newDeck,cards,counter-1)
 }                                                //> popCard: (deck: String, hand: String, counter: Int)(String, String)
 
 val hand = popCard(deck,"",3)                    //> hand  : (String, String) = (8H AD 8S,KH TH 5C KD 6S AS 8S TH 4C JD 7S TC KC
                                                  //|  7H 2S AD 2C JC 6D QH 6H 3D 7C JC 2S JH 5D AS KC 4S 2H QD 6S QD 9C 3C 3S 8H
                                                  //|  9H 9S TC QD 4H 9D 7D 5D 4S 5H 3C)
 
 /*
 def getHands(numHands: Int, numCards: Int, deck: String, hands: List[String]): (List[List[String]],String) = {
 	if(numHands==0) (hands,deck)
 	else{
 		val cardPair = popCard(deck,List())
 		val newDeck = cardPair._2
 		val newHands = hands ++ List(cardPair._1)
 		getHands(numHands-1,numCards,newDeck,newHands)
 	}
 }*/
 
 //getHands(2,5,deck,List())
 
}