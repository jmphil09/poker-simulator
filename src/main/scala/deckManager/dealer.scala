package deckManager

import deckManager._

object dealer {

  def getHands(numHands: Int, numCards: Int, deck: String, hands: List[String]): (List[String], String) = {
    def popCard(deck: String, hand: String, counter: Int): (String, String) = {
      val cards = hand ++ " " ++ (deck.dropRight(deck.length - 2))
      val newDeck = deck.drop(3)
      if (counter == 1) (cards.drop(1), newDeck)
      else popCard(newDeck, cards, counter - 1)
    }
    if (numHands == 0) (hands, deck)
    else {
      val cardPair = popCard(deck, "", numCards)
      val newDeck = cardPair._2
      val newHands = hands ++ List(cardPair._1)
      getHands(numHands - 1, numCards, newDeck, newHands)
    }
  }

  /**
   * This function will deal a standard texas holdem game
   *
   * @param players Number of players in game
   *
   * @return (List[String],String,String) List of players hands, followed by the community cards(turn flop river), followed by the remaining deck
   */
  def dealHoldem(players: Int): (List[String], String, String) = {
    val deck = createDeck
    val handPair = getHands(players, 2, deck, List())
    val playersHands = handPair._1
    val tempRemainingDeck = handPair._2
    val commPair = getHands(1, 5, tempRemainingDeck, List())
    val communityCards = commPair._1.head
    val remainingDeck = commPair._2
    (playersHands, communityCards, remainingDeck)
  }

  /**
   * This function will deal a standard 5 card stud game
   *
   * @param players Number of players in game
   *
   * @return (List[String],String) List of players hands, followed by the remaining deck
   */
  def deal5CardStud(players: Int): (List[String], String) = {
    val deck = createDeck
    val handPair = getHands(players, 5, deck, List())
    val playersHands = handPair._1
    val remainingDeck = handPair._2
    (playersHands, remainingDeck)
  }
}
