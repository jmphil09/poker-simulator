import scala.io.Source

package handManager {

object handManager {
  
  class Hand(cards: String) {
    var cardStr = cards
  }
  case class HighCard(cards: String) extends Hand(cards: String)
  case class OnePair(cards: String) extends Hand(cards: String)
  case class TwoPair(cards: String) extends Hand(cards: String)
  case class ThreeOfAKind(cards: String) extends Hand(cards: String)
  case class Straight(cards: String) extends Hand(cards: String)
  case class Flush(cards: String) extends Hand(cards: String)
  case class FullHouse(cards: String) extends Hand(cards: String)
  case class FourOfAKind(cards: String) extends Hand(cards: String)
  case class StraightFlush(cards: String) extends Hand(cards: String)
	

	//this function converts T,J,Q,K,A to 10,11,12,13,14 respectively
  def convertCards(card: Char): Int = card match {
    case '2' => 2
    case '3' => 3
    case '4' => 4
    case '5' => 5
    case '6' => 6
    case '7' => 7
    case '8' => 8
    case '9' => 9
    case 'T' => 10
    case 'J' => 11
    case 'Q' => 12
    case 'K' => 13
    case 'A' => 14
    //case _ => card.toInt
  }
  
  //should probably refactor this
  //counts the copies in a list
  def countCopies(list: List[Int], oldlist: List[Int], maxInt: Int): Int = {
    var counter = 0
    
    if(list.isEmpty) maxInt
    else {
      for(i <- 0 to 4){
        if(oldlist(i)==list.head) counter = counter+1
      }
    if(counter > maxInt) countCopies(list.tail, oldlist, counter)
    else countCopies(list.tail, oldlist, maxInt)
    }
    
  }
  
  //this function will convert a hand to a list and set
  def convertHand(hand: String): (Set[Int],List[Int]) = {
    val card1 = convertCards(hand(0))
    val card2 = convertCards(hand(3))
    val card3 = convertCards(hand(6))
    val card4 = convertCards(hand(9))
    val card5 = convertCards(hand(12))
    val suit1 = hand(1)
    val suit2 = hand(4)
    val suit3 = hand(7)
    val suit4 = hand(10)
    val suit5 = hand(13)
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
  
  	//make this less sloppy
    if(suitSet.size==1) {
      if(cardSet.max-cardSet.min==4) StraightFlush(hand)
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
    else if(cardSet.max-cardSet.min==4) Straight(hand)
    else HighCard(hand)
  }
  
  //helper function for tiebreakers
  def compareHighCard(p1Set: Set[Int], p2Set: Set[Int]): String = {
    val p1Max = p1Set.max
    val p2Max = p2Set.max
    if(p1Max==p2Max) compareHighCard(p1Set-p1Max,p2Set-p2Max)
    else if(p1Max > p2Max) "Player 1"
    else "Player 2"
  }
  
  //helper function
  def determinePair(cardList: List[Int]): Int = {
    //helper function
    def checkMatch(num: Int, list: List[Int]): Boolean = {
      var result = false
      for(i <- 0 to list.length-1){
        if(num==list(i)) result = true
      }
      result
    }
    
    if(cardList.length==2 || checkMatch(cardList.head, cardList)) cardList.head
    else determinePair(cardList.tail)
  }
  
  def compareOnePair(p1Pair: (Set[Int],List[Int]), p2Pair: (Set[Int],List[Int])): String = {
    val p1Set = p1Pair._1
    val p1List = p1Pair._2
    val p2Set = p2Pair._1
    val p2List = p2Pair._2
    
    if(determinePair(p1List) > determinePair(p2List)) "Player 1"
    else if(determinePair(p1List) < determinePair(p2List)) "Player 2"
    else compareHighCard(p1Set,p2Set)
  }
  
  //TODO: combine this function with determinePair
  //helper function
  def determineTwoPair(cardList: List[Int]): (Int,Int) = {
    var pair1 = 0
    var pair2 = 0
    
    for(i <- 0 to 4){
      for(j <- i+1 to 4){
        if(cardList(i)==cardList(j)&&cardList(i)!=pair1) pair2=cardList(i)
      }
    }
    
    val highPair = List(pair1,pair2).max
    val lowPair = List(pair1,pair2).min
    
    (highPair, lowPair)
  }
  
  def compareTwoPair(p1List: List[Int], p2List: List[Int]): String = {
    val p1Pairs = determineTwoPair(p1List)
    val p1HighPair = p1Pairs._1
    val p1LowPair = p1Pairs._2
    val p2Pairs = determineTwoPair(p2List)
    val p2HighPair = p2Pairs._1
    val p2LowPair = p2Pairs._2
    
    if(p1HighPair > p2HighPair) "Player 1"
    else if(p1HighPair < p2HighPair) "Player 2"
    else if(p1LowPair > p2LowPair) "Player 1"
    else if(p1LowPair < p2LowPair) "Player 2"
    else compareHighCard(p1List.toSet,p2List.toSet)
  }
  
  //helper function
  def cardWithCopies(cardList: List[Int]): Int = {
  	
  	//outputs the number of times a number appears in a list
  	def helper(num: Int, cardList: List[Int], counter: Int): Int = {
  		if(cardList.isEmpty) counter
  		else if(num==cardList.head) helper(num, cardList.tail, counter+1)
  		else helper(num, cardList.tail, counter)
  	}
  	
  	var result = 0
  	var counter = 0
  	for(num <- cardList){
  		if(helper(num,cardList,0) > counter) {
  			result = num
  			counter = helper(num,cardList,0)
  		}
  	}
  	
  	result
  }
  
  def compareThreeOfAKind(p1List: List[Int], p2List: List[Int]): String = {
  	val p1Three = cardWithCopies(p1List)
  	val p2Three = cardWithCopies(p2List)
  	
  	if(p1Three > p2Three) "Player 1"
  	else "Player 2"
  	
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
      case (Straight(cards1),Straight(cards2)) => compareHighCard(p1Set,p2Set)
      case (Flush(cards1),Flush(cards2)) => compareHighCard(p1Set,p2Set)
      case (FullHouse(cards1),FullHouse(cards2)) => compareThreeOfAKind(p1List,p2List)
      case (FourOfAKind(cards1),FourOfAKind(cards)) => compareThreeOfAKind(p1List,p2List)
      case (StraightFlush(cards1),StraightFlush(cards2)) => compareHighCard(p1Set,p2Set)
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
  
  findWinner("8C TS KC 9H 4S 7D 2S 5D 3S AC")
  
  findHand("8C TS KC 9H 4S")
  findHand("5C AD 5D AC 9C")
  
  
  //TODO: write some tests for all of these functions!!!
  //tests for countCopies
  val testlist1 = List(6,2,3,4,5)
  val testlist2 = List(2,2,3,4,5)
  val testlist3 = List(6,6,6,4,4)
  val testlist4 = List(6,6,4,4,5)
  
  assert(countCopies(testlist1, testlist1, 0)==1)
  assert(countCopies(testlist2, testlist2, 0)==2)
  assert(countCopies(testlist3, testlist3, 0)==3)
  assert(countCopies(testlist4, testlist4, 0)==2)
  
  def findP1Wins(): Int = {
  	var p1Wins = 0
  	val hands = Source.fromFile("C:/Users/fligh_000/Desktop/git-workspace/poker-simulator/src/poker.txt").getLines
  	for(l <- hands){
  		if(findWinner(l)=="Player 2") p1Wins = p1Wins + 1
  	}
  	p1Wins
  }

}
}