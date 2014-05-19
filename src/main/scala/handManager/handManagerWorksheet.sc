package handManager

import scala.util.Random._
import scala.collection._

import handManager._
import compareHands._
import deckManager.deckManager._
import deckManager.dealer._

object handManagerWorksheet {

  val list1 = List(3, 3, 4, 5, 2)                 //> list1  : List[Int] = List(3, 3, 4, 5, 2)
  val set1 = list1.toSet                          //> set1  : scala.collection.immutable.Set[Int] = Set(3, 4, 5, 2)
  set1.size                                       //> res0: Int = 4
  countCopies(list1, list1, 0)                    //> res1: Int = 2

  def findDuplicates[A, B](list: List[B])(crit: (B) => A): Iterable[A] = {
    list.groupBy(crit) filter { case (_, l) => l.size > 1 } keys
  }                                               //> findDuplicates: [A, B](list: List[B])(crit: B => A)Iterable[A]

  findDuplicates(list1) { x => x } toSet          //> res2: scala.collection.immutable.Set[Int] = Set(3)

  val alist = List(1, 2, 3, 4, 3, 2, 5, 7, 5)     //> alist  : List[Int] = List(1, 2, 3, 4, 3, 2, 5, 7, 5)
  val duplicatesItem = alist groupBy { x => x } filter { case (_, lst) => lst.size > 1 } keys
                                                  //> duplicatesItem  : Iterable[Int] = Set(5, 2, 3)
	val mods = (1 to 20).toList groupBy ( _ % 4 )
                                                  //> mods  : scala.collection.immutable.Map[Int,List[Int]] = Map(2 -> List(2, 6, 
                                                  //| 10, 14, 18), 1 -> List(1, 5, 9, 13, 17), 3 -> List(3, 7, 11, 15, 19), 0 -> L
                                                  //| ist(4, 8, 12, 16, 20))
	
	val listMap = list1 groupBy (x => x)      //> listMap  : scala.collection.immutable.Map[Int,List[Int]] = Map(2 -> List(2),
                                                  //|  5 -> List(5), 4 -> List(4), 3 -> List(3, 3))
  listMap(3).size                                 //> res3: Int = 2
  
  for(key <- listMap){
  	println(key._1,key._2.size)               //> (2,1)
                                                  //| (5,1)
                                                  //| (4,1)
                                                  //| (3,2)
  }
}