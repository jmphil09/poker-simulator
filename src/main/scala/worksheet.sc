import deckManager._
import handManager._
import multipleHands._

import scala.collection.mutable._

object worksheet {

  val twoHandsStr = "2H 3D 4S 5C 6D 7H 7D 7S 2D AC"
                                                  //> twoHandsStr  : String = 2H 3D 4S 5C 6D 7H 7D 7S 2D AC
  val hand1 = findHand("2H 3D 4S 5C 6D")          //> hand1  : handManager.Hand = Straight(2H 3D 4S 5C 6D)
  val hand2 = findHand("7H 7D 7S 2D AC")          //> hand2  : handManager.Hand = ThreeOfAKind(7H 7D 7S 2D AC)
  findWinner(twoHandsStr)                         //> res0: String = Player 1
  compareHands(hand1, hand2)                      //> res1: String = Player 1
  val handListTest = List(Straight("2H 3D 4S 5C 6D"), OnePair("3H 3S 4D 2S 6D"), OnePair("TH 8H 5C QS TC"), Straight("2H 3C 4S 5C 6D"))
                                                  //> handListTest  : List[Product with Serializable with handManager.Hand] = List
                                                  //| (Straight(2H 3D 4S 5C 6D), OnePair(3H 3S 4D 2S 6D), OnePair(TH 8H 5C QS TC),
                                                  //|  Straight(2H 3C 4S 5C 6D))


  compareMultHands(handListTest)                  //> res2: List[List[String]] = List(List(Player 4, Player 1), List(Player 3), Li
                                                  //| st(Player 2))
}