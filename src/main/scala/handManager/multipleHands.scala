package handManager

object multipleHands {
  //this function takes a list of hands and creates a list of 2ples of (hand,player number)
  def addPlayerIndex(list: List[Hand]): List[(Hand, Int)] = {
    def helper(ls: List[Hand], counter: Int, result: List[(Hand, Int)]): List[(Hand, Int)] = {
      if (ls.isEmpty) result
      else helper(ls.tail, counter + 1, result ++ List((ls.head, counter)))
    }
    helper(list, 1, List())
  }

  //input: list of hands
  //output: list of a list hands in decreasing order
  def compareMultHands(handList: List[(Hand, Int)]): List[Set[String]] = {
    //this function finds all of the hands which tie "hand" in a list of 3-tuples 
    def findTiesWithHand(hand: Hand, handList: List[(Int, Hand, Int)]): List[(Int, Hand, Int)] = {
      def helper(hand1: Hand, handList1: List[(Int, Hand, Int)], result: List[(Int, Hand, Int)]): List[(Int, Hand, Int)] = {
        if (handList1.isEmpty) result
        else if (compareHands(hand1, handList1.head._2) == "Tie") helper(hand1, handList1.tail, result ++ List(handList1.head))
        else helper(hand1, handList1.tail, result)
      }
      helper(hand, handList, List())
    }
    val orderedHands = handList.map(x => (x._1.handValue, x._1, x._2)).sortBy(x => x._1).reverse
    
    //this function takes the list of handValues,hands,player numbers and outputs of list of the players in descending order
    def createStrList(orderedHands: List[(Int, Hand, Int)], result: List[Set[String]]): List[Set[String]] = {
      //this function will subtract the best players from the orderedHands list
      def subtractPlayers(players: List[Int], orderedHands: List[(Int, Hand, Int)]): List[(Int, Hand, Int)] = {
        orderedHands.filterNot(x => players.contains(x._3))
      }
      //takes a list of ints and creates a list of players with those ints
      def genList(listNums: List[Int], players: Set[String]): Set[String] = {
        if (listNums.isEmpty) players
        else {
          val playerStr = "Player " ++ listNums.head.toString
          genList(listNums.tail, players ++ Set(playerStr))
        }
      }

      if (orderedHands.isEmpty) result
      else {
        //ints of the best players
        val bestPlayers = findTiesWithHand(orderedHands.head._2, orderedHands).map(x => x._3)
        createStrList(subtractPlayers(bestPlayers, orderedHands), result ++ List(genList(bestPlayers, Set())))
      }
    }
    createStrList(orderedHands, List())
  }
}
