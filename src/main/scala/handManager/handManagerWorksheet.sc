package handManager

import scala.util.Random._
import scala.collection._

import handManager._
import deckManager.deckManager._
import deckManager.dealer._

object handManagerWorksheet {
  
  val holdem = dealHoldem(6)                      //> holdem  : (List[String], String, String) = (List(9C 9D, 5S 2H, 8C 8H, TS 7H,
                                                  //|  AC 9S, 2S 3H),4S 7S QD 2C 5H,6H 5D 8D TC QC 3S 4D KC KS 6C 3C JC 5C 7D QH T
                                                  //| H 8S AD QS 4C 9H AS 6S JH 3D TD KD 7C JD 2D JS KH 4H 6D AH)
  val holdemHands = holdem._1.length              //> holdemHands  : Int = 6
  
  val comCards = holdem._2                        //> comCards  : String = 4S 7S QD 2C 5H
  
  holdem._3.length                                //> res0: Int = 104
  
  comCards.length                                 //> res1: Int = 14
  
  155- comCards.length - 5*6 - (6+1)              //> res2: Int = 104
  
  155-14                                          //> res3: Int(141) = 141
  
  val stud = deal5CardStud(6)                     //> stud  : (List[String], String) = (List(JH 3C JD 2D JC, 6D TD AS 5D 8C, 4H 9H
                                                  //|  KS 3D KC, TH 4D TS 5H 6C, 4C QH AD 6H 9D, 7C 7H 9S 8D 8S),3S KH 9C 8H KD 2C
                                                  //|  3H 2S QS 7D 2H JS AC 4S QD 6S AH QC 5C TC 7S 5S)
  val leftOverDeck = stud._2                      //> leftOverDeck  : String = 3S KH 9C 8H KD 2C 3H 2S QS 7D 2H JS AC 4S QD 6S AH 
                                                  //| QC 5C TC 7S 5S
  leftOverDeck.size                               //> res4: Int = 65
  155-6*14-6                                      //> res5: Int = 65
}