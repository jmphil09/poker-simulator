import scala.io.Source

import handManager.Hand._
import handManager.compareHands._

package handManager {

object handManager {

  //this function converts T,J,Q,K,A to 10,11,12,13,14 respectively
  def convertCards(card: Char): Int = card match {
    case 'T' => 10
    case 'J' => 11
    case 'Q' => 12
    case 'K' => 13
    case 'A' => 14
    case _ => card.asDigit
  }
  
  //this function will convert a hand to a list and set
  def convertHand(hand: String): (Set[Int],List[Int]) = {
    val card1 = convertCards(hand(0))
    val card2 = convertCards(hand(3))
    val card3 = convertCards(hand(6))
    val card4 = convertCards(hand(9))
    val card5 = convertCards(hand(12))
    val cardSet = Set(card1,card2,card3,card4,card5)
    val cardList = List(card1,card2,card3,card4,card5)
    
    (cardSet,cardList)
  }
  
  //this function will take a single players hand as a string and output the hand it represents
  def findHand(hand: String): Hand = {
    val cardPair = convertHand(hand)
    val cardSet = cardPair._1
    val cardList = cardPair._2  
    val suit1 = hand(1)
    val suit2 = hand(4)
    val suit3 = hand(7)
    val suit4 = hand(10)
    val suit5 = hand(13)   
    val suitSet = Set(suit1,suit2,suit3,suit4,suit5)
  
    if(suitSet.size==1) {
      if(cardSet.max-cardSet.min==4 || cardSet==Set(2,3,4,5,14)) StraightFlush(hand)
      else Flush(hand)
    }
    else if(cardSet.size==2) {
      if(countCopies(cardList, cardList, 0)==4) FourOfAKind(hand)
      else FullHouse(hand)
    }
    else if(cardSet.size==3) {
      if(countCopies(cardList,cardList,0)==3) ThreeOfAKind(hand)
      else TwoPair(hand)
    }
    else if(cardSet.size==4) OnePair(hand)
    else if(cardSet.max-cardSet.min==4 || cardSet==Set(2,3,4,5,14)) Straight(hand)
    else HighCard(hand)
  }
  
  //this function will be used to break the tie between hands
  def tieBreaker(p1Hand: Hand, p2Hand: Hand): String =  {
  
    val p1Cards = p1Hand.cardStr
    val p2Cards = p2Hand.cardStr
    val p1Pair = convertHand(p1Cards)
    val p2Pair = convertHand(p2Cards)
    val p1Set = p1Pair._1
    val p1List = p1Pair._2
    val p2Set = p2Pair._1
    val p2List = p2Pair._2
  
    (p1Hand,p2Hand) match {
      case (HighCard(cards1),HighCard(cards2)) => compareHighCard(p1Set,p2Set)
      case (OnePair(cards1),OnePair(cards2)) => compareOnePair(p1Pair,p2Pair)
      case (TwoPair(cards1),TwoPair(cards2)) => compareTwoPair(p1List,p2List)
      case (ThreeOfAKind(cards1),ThreeOfAKind(cards2)) => compareThreeOfAKind(p1List,p2List)
      case (Straight(cards1),Straight(cards2)) => compareStraight(p1Set,p2Set)
      case (Flush(cards1),Flush(cards2)) => compareHighCard(p1Set,p2Set)
      case (FullHouse(cards1),FullHouse(cards2)) => compareThreeOfAKind(p1List,p2List)
      case (FourOfAKind(cards1),FourOfAKind(cards)) => compareThreeOfAKind(p1List,p2List)
      case (StraightFlush(cards1),StraightFlush(cards2)) => compareStraight(p1Set,p2Set)
    }
  }
  
  //a function for hand comparison
  def handAsValue(hand: Hand): Int = hand match{
    case HighCard(cards) => 1
    case OnePair(cards) => 2
    case TwoPair(cards) => 3
    case ThreeOfAKind(cards) => 4
    case Straight(cards) => 5
    case Flush(cards) => 6
    case FullHouse(cards) => 7
    case FourOfAKind(cards) => 8
    case StraightFlush(cards) => 9
  }
  
  //this function will compare the two hands and output a winner
  def compareHands(p1Hand: Hand, p2Hand: Hand): String = {
    val p1Hval = handAsValue(p1Hand)
    val p2Hval = handAsValue(p2Hand)
    
    if(p1Hval > p2Hval) "Player 1"
    else if(p2Hval > p1Hval) "Player 2"
    else tieBreaker(p1Hand, p2Hand)
  }
  
  //this function will take the string of hands from the input file and output which player has won
  def findWinner(hands: String): String = {
    val p1Hand = hands.dropRight(29-14)
    val p2Hand = hands.drop(15)
    compareHands(findHand(p1Hand), findHand(p2Hand))
  }
  
}
}