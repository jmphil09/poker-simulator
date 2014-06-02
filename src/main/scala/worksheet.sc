import deckManager._
import handManager._
import multipleHands._

import scala.collection.mutable._

object worksheet {

  
  val list = List(2,3,3,4,5)                      //> list  : List[Int] = List(2, 3, 3, 4, 5)
  
  list.groupBy(x => x)                            //> res0: scala.collection.immutable.Map[Int,List[Int]] = Map(2 -> List(2), 5 ->
                                                  //|  List(5), 4 -> List(4), 3 -> List(3, 3))
  val map = list.groupBy(x => x).map(x => (x._2.size, x._1))
                                                  //> map  : scala.collection.immutable.Map[Int,Int] = Map(1 -> 4, 2 -> 3)
  
  val key = list.groupBy(x => x).map(x => (x._2.size, x._1)).max._1
                                                  //> key  : Int = 2
  map(key)                                        //> res1: Int = 3
}