package handManager

import scala.util.Random._
import scala.collection._

import handManager._
import deckManager.deckManager._
import deckManager.dealer._

object handManagerWorksheet {
  val deckNums = createDeckNums                   //> deckNums  : String = 12D 9S 5D 10S 13S 11C 9D 12C 13C 7D 5H 7H 4S 4H 13D 4C 
                                                  //| 12S 11H 7S 7C 3S 9D 9D 14C 8D 5D 4D 8D 11C 13S 14C 8C 2S 11H 3C 5S 6H 3D 8D 
                                                  //| 10H 14H 3S 12S 10S 6H 6H 10C 14C 6C 2H 2H 2H
  deckNums.length                                 //> res0: Int = 175
  val deck = createDeck(deckNums)                 //> deck  : String = QD 9S 5D TS KS JC 9D QC KC 7D 5H 7H 4S 4H KD 4C QS JH 7S 7C
                                                  //|  3S 9D 9D AC 8D 5D 4D 8D JC KS AC 8C 2S JH 3C 5S 6H 3D 8D TH AH 3S QS TS 6H 
                                                  //| 6H TC AC 6C 2H 2H 2H
  deck.length                                     //> res1: Int = 155
  val handsDeck = getHands(3, 5, deck, List())    //> handsDeck  : (List[String], String) = (List(QD 9S 5D TS KS, JC 9D QC KC 7D, 
                                                  //| 5H 7H 4S 4H KD),4C QS JH 7S 7C 3S 9D 9D AC 8D 5D 4D 8D JC KS AC 8C 2S JH 3C 
                                                  //| 5S 6H 3D 8D TH AH 3S QS TS 6H 6H TC AC 6C 2H 2H 2H)
  val deck1 = handsDeck._1.tail.tail.head         //> deck1  : String = 5H 7H 4S 4H KD
	
  
  shuffleDeck                                     //> res2: String = AC 6S 6D AD TH 7C AS 4S 4D 7H 2S 2H QS JH AH JS 6C 8S 3C 5C Q
                                                  //| D 5H 9S TD TC 6H 7D 5D 3S 4H 2C 2D QH KD JC 7S 9H 3D QC TS 4C 8H KS KC 3H JD
                                                  //|  5S 9D 8D 8C KH 9C
  
  
  
  
     
}