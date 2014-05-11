package handManager

import scala.util.Random._
import scala.collection._

import handManager._

object handManagerWorksheet {
  
  nextInt(14)                                     //> res0: Int = 10
  
  def createDeck(): List[Int] = {
  
  def nextCard(): Int = {
  	val card = nextInt(14)
  	if (card==0) nextCard
  	else card
  }
  
  	var deck = List(0)
  	var cardMap = Map(0->0)
  for(n <- 1 to 52){
  	val card = nextCard
  	deck = deck ++ List(nextCard)
  }
  	deck.tail
  }                                               //> createDeck: ()List[Int]
  
  
  
  createDeck                                      //> res1: List[Int] = List(2, 7, 2, 1, 4, 9, 13, 11, 12, 8, 11, 7, 11, 5, 2, 1, 
                                                  //| 8, 11, 5, 3, 9, 6, 3, 10, 3, 6, 12, 6, 4, 6, 6, 12, 10, 10, 5, 1, 13, 4, 10,
                                                  //|  7, 12, 4, 8, 7, 11, 6, 5, 8, 12, 8, 12, 1)
  def updateDeck(num: Int, deck: mutable.Map[Int,Int]): mutable.Map[Int,Int] = {
  	if(deck.contains(num)) {
  		val n = deck(num)+1
  		deck+={num->n}
  	}
  	else deck+={num->1}
  }                                               //> updateDeck: (num: Int, deck: scala.collection.mutable.Map[Int,Int])scala.col
                                                  //| lection.mutable.Map[Int,Int]
  val deckMap = mutable.Map(0->0,3->1)            //> deckMap  : scala.collection.mutable.Map[Int,Int] = Map(3 -> 1, 0 -> 0)
  updateDeck(3,deckMap)                           //> res2: scala.collection.mutable.Map[Int,Int] = Map(3 -> 2, 0 -> 0)
     
  /*val deckMap = mutable.Map(0->0,3->1)
  if(deckMap.contains(2)) {
  	val n = deckMap(2)+1
  	deckMap+={2->n}
  }
  else{
  	deckMap+={2->1}
  }
  
  deckMap
  */
}