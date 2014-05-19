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
  
  //this function will take a single players hand as a string and output the hand it represents
  def findHand(hand: String): Hand = {
    val cardPair = handAsOrderedPair(hand)
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

  //this function will compare the two hands and output a winner
  def compareHands(p1Hand: Hand, p2Hand: Hand): String = {
    val p1Hval = p1Hand.handValue
    val p2Hval = p2Hand.handValue
    
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
