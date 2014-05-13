package handManager

import scala.util.Random._
import scala.collection._

import handManager._
import deckManager.deckManager._
import deckManager.dealer._

object handManagerWorksheet {
  val deckNums = createDeckNums                   //> deckNums  : String = 5S 11C 13D 12C 5H 9C 8D 2C 9C 8C 8S 7S 11S 14D 2H 12D 1
                                                  //| 3C 13C 4C 12C 3S 8S 7C 9D 9D 7H 13H 7H 12C 5S 6H 5D 4D 4S 3H 2D 11C 6D 11H 1
                                                  //| 0S 10S 14S 3H 3H 4H 2H 10D 6D 6H 10D 14S 14S
  deckNums.length                                 //> res0: Int = 175
  val deck = createDeck(deckNums)                 //> deck  : String = 5S JC KD QC 5H 9C 8D 2C 9C 8C 8S 7S JS AD 2H QD KC KC 4C QC
                                                  //|  3S 8S 7C 9D 9D 7H KH 7H QC 5S 6H 5D 4D 4S 3H 2D JC 6D JH TS TS AS 3H 3H 4H 
                                                  //| 2H TD 6D 6H TD AS AS
  deck.length                                     //> res1: Int = 155
  val handsDeck = getHands(3, 5, deck, List())    //> handsDeck  : (List[String], String) = (List(5S JC KD QC 5H, 9C 8D 2C 9C 8C, 
                                                  //| 8S 7S JS AD 2H),QD KC KC 4C QC 3S 8S 7C 9D 9D 7H KH 7H QC 5S 6H 5D 4D 4S 3H 
                                                  //| 2D JC 6D JH TS TS AS 3H 3H 4H 2H TD 6D 6H TD AS AS)
  val deck1 = handsDeck._1.tail.tail.head         //> deck1  : String = 8S 7S JS AD 2H

}