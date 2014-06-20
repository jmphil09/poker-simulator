//import deckManager._
//import handManager._
//import multipleHands._

//import scala.collection.mutable._

object worksheet {

val list = List(2, 3, 3, 4, 5)                    //> list  : List[Int] = List(2, 3, 3, 4, 5)
val list2 = List(2, 2, 3, 3, 8)                   //> list2  : List[Int] = List(2, 2, 3, 3, 8)
val list3 = List(2,2,2,3,4)                       //> list3  : List[Int] = List(2, 2, 2, 3, 4)
val list4 = List(5,5,5,5,6)                       //> list4  : List[Int] = List(5, 5, 5, 5, 6)

def cardWithCopies(cardList: List[Int]): Int = {
    //outputs the number of times a number appears in a list
    def helper(num: Int, cardList: List[Int], counter: Int): Int = {
      if (cardList.isEmpty) counter
      else if (num == cardList.head) helper(num, cardList.tail, counter + 1)
      else helper(num, cardList.tail, counter)
    }
    var result = 0
    var counter = 0
    for (num <- cardList) {
      if (helper(num, cardList, 0) > counter) {
        result = num
        counter = helper(num, cardList, 0)
      }
    }
    result
  }                                               //> cardWithCopies: (cardList: List[Int])Int
  
  def determinePair(cardList: List[Int]): Int = {
    val map = cardList.groupBy(x => x).map(x => (x._2.size, x._1))
    val key = map.max._1
    map(key)
    }                                             //> determinePair: (cardList: List[Int])Int
  
  cardWithCopies(list)                            //> res0: Int = 3
  cardWithCopies(list2)                           //> res1: Int = 2
  cardWithCopies(list3)                           //> res2: Int = 2
  cardWithCopies(list4)                           //> res3: Int = 5
  
	determinePair(list)                       //> res4: Int = 3
	determinePair(list2)                      //> res5: Int = 3
	determinePair(list3)                      //> res6: Int = 2
	determinePair(list4)                      //> res7: Int = 5

  list.groupBy(x => x)                            //> res8: scala.collection.immutable.Map[Int,List[Int]] = Map(2 -> List(2), 5 -
                                                  //| > List(5), 4 -> List(4), 3 -> List(3, 3))
  val map = list.groupBy(x => x).map(x => (x._2.size, x._1))
                                                  //> map  : scala.collection.immutable.Map[Int,Int] = Map(1 -> 4, 2 -> 3)

  val key = list.groupBy(x => x).map(x => (x._2.size, x._1)).max._1
                                                  //> key  : Int = 2
  map(key)                                        //> res9: Int = 3

  def determineTwoPair(cardList: List[Int]): (Int,Int) = {
    val pairSet = cardList.groupBy(x => x).filter(x => x._2.size==2).keys.toSet
    (pairSet.max,pairSet.min)
  }                                               //> determineTwoPair: (cardList: List[Int])(Int, Int)

  //determinePair(list)


  determineTwoPair(list2)                         //> res10: (Int, Int) = (3,2)
  
  
  
}