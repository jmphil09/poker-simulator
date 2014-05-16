package handManager

object compareHands {
//should probably refactor this
  //finds the maximum times an element is repeated in a list
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
  
  //helper function for tiebreakers
  def compareHighCard(p1Set: Set[Int], p2Set: Set[Int]): String = {
    val p1Max = p1Set.max
    val p2Max = p2Set.max
    if(p1Max==p2Max) compareHighCard(p1Set-p1Max,p2Set-p2Max)
    else if(p1Max > p2Max) "Player 1"
    else "Player 2"
  }
  
  def compareStraight(p1Set: Set[Int], p2Set: Set[Int]): String = {
    var p1Wheel = false
    var p2Wheel = false
    if(p1Set==Set(2,3,4,5,14)) p1Wheel = true
    if(p2Set==Set(2,3,4,5,14)) p2Wheel = true
    
    if(p1Wheel && !p2Wheel) "Player 2"
    else if(p2Wheel && !p1Wheel) "Player 1"
    else(compareHighCard(p1Set,p2Set))
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
    if(cardList.length==2 || checkMatch(cardList.head, cardList.tail)) cardList.head
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
}