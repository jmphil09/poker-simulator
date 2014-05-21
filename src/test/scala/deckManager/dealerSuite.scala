package deckManager

import org.scalatest._
import scala.util.Random._

import dealer._

class dealerSuite extends FlatSpec with Matchers {
  
  val holdemHandNum = nextInt(8)+2
  val holdem = dealHoldem(holdemHandNum)
  val studHandNum = nextInt(4)+2
  val stud = deal5CardStud(studHandNum)
     
  "Dealer for holdem" should "deal the proper number of hands" in {
      val numHands = holdem._1
      numHands.length should be (holdemHandNum)
    } 
  
  it should "deal community cards" in {
      val commCards = holdem._2
      commCards.length should be (14)
    }
  
  it should "have a leftover deck" in {
      val leftOverDeck = holdem._3
      val leftOverSize = 155-(holdem._2.length)-5*holdemHandNum-(holdemHandNum+1)
      leftOverDeck.size should be (leftOverSize)
    } 
  
  "Dealer for 5 card stud" should "deal the proper number of hands" in {
	  val numHands = stud._1
      numHands.length should be (studHandNum)
    } 
  
  it should "have a leftover deck" in {
	  val leftOverDeck = stud._2
	  val leftOverSize = 155-studHandNum*14-studHandNum
      leftOverDeck.size should be (leftOverSize)
    }

}
