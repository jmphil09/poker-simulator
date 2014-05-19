package handManager

import scala.util.Random._
import scala.collection._


import handManager._
import compareHands._
import deckManager.deckManager._
import deckManager.dealer._

object handManagerWorksheet {
  
  val list1 = List(3,3,4,5,2)                     //> list1  : List[Int] = List(3, 3, 4, 5, 2)
  val set1 = list1.toSet                          //> set1  : scala.collection.immutable.Set[Int] = Set(3, 4, 5, 2)
  set1.size                                       //> res0: Int = 4
  countCopies(list1,list1,0)                      //> res1: Int = 2
  
  def findDuplicates[A,B](list:List[B])(crit:(B)=>A):Iterable[A] = {
     list.groupBy(crit) filter {case (_,l) => l.size > 1 } keys
}                                                 //> findDuplicates: [A, B](list: List[B])(crit: B => A)Iterable[A]

	findDuplicates(list1) {x=>x} toSet        //> res2: scala.collection.immutable.Set[Int] = Set(3)
  
}