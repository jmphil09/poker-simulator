package deckManager

import deckManager._

object dealer {
  
  //pops "counter" cards off the top of a deck and returns both the cards and remaining deck
 def popCard(deck: String, hand: String, counter: Int): (String,String) = {
 	var cards = hand ++ " " ++ (deck.dropRight(deck.length-2))
 	var newDeck = deck.drop(3)
 	
 	if(counter==1) (cards.drop(1),newDeck)
 	else popCard(newDeck,cards,counter-1)
 }
 
 def getHands(numHands: Int, numCards: Int, deck: String, hands: List[String]): (List[String],String) = {
 	if(numHands==0) (hands,deck)
 	else{
 		val cardPair = popCard(deck,"",numCards)
 		val newDeck = cardPair._2
 		val newHands = hands ++ List(cardPair._1)
 		getHands(numHands-1,numCards,newDeck,newHands)
 	}
 }
 
 //This function will deal a standard texas holdem game
 //input: players: number of players
 //output: List of players hands, string representing the flop turn and river, string representing the remaining deck 
 def dealHoldem(players: Int): (List[String],String,String) = {
   val deck = createDeck(createDeckNums)
   val handPair = getHands(players+1,2,deck,List())
   val handList = handPair._1
   val communityCards = handList.head
   val playersHands = handList.tail
   val remainingDeck = handPair._2
   (playersHands,communityCards,remainingDeck)
 }
 
 //This function will deal a standard 5 card stud game to "players"
 //input: players: number of players
 //output: List of players hands, string representing the remaining deck
 def deal5CardStud(players: Int): (List[String],String) = {
   val deck = createDeck(createDeckNums)
   val handPair = getHands(players,5,deck,List())
   val playersHands = handPair._1
   val remainingDeck = handPair._2
   (playersHands,remainingDeck)
 }

}