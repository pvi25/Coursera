package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b,insert(a, empty))
    //println(a + " - "+ b + " - " + (a<b) + " - " + h)
    if (a < b) findMin(h) == a
    else findMin(h) == b
  }

  property("longerHeap") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c,insert(b,insert(a, empty)))
    findMin(h) == List(a, b, c).min
  }

  //println (arbitrary[Int])
  //println (genHeap.sample)
  //println (genMap)
  
  property("delempty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sorted") = forAll { (a: Int, b: Int, c: Int)  =>
    val h = insert(c,insert(b,insert(a, empty)))
    def sortedLoop (h: H): List[A] = 
      if (isEmpty(h)) Nil
      else findMin(h) :: sortedLoop(deleteMin(h))
    sortedLoop(h) == List(a, b, c).sorted 
  }
    
  lazy val genHeap: Gen[H] = for {
  k <- arbitrary[Int]
  m <- oneOf(value(empty), genHeap)
  } yield insert (k, m) 
/*
  lazy val genHeap2: Gen[Heap] = for {
  k <- arbitrary[Int]
  m <- oneOf(value(empty), genHeap)
  } yield insert (k, m) 
*/
lazy val genMap: Gen[Map[Int,Int]] = for {
  k <- arbitrary[Int]
  v <- arbitrary[Int]
  m <- oneOf(value(Map.empty[Int,Int]), genMap)
} yield m.updated(k, v)  
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
