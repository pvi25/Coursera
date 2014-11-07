package quickcheck

import quickcheck._
import common._

import org.scalacheck._
import Arbitrary._

object week1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val h = arbitrary[Int]                          //> h  : org.scalacheck.Gen[Int] = Gen()
  arbInt.arbitrary                                //> res0: org.scalacheck.Gen[Int] = Gen()
  for (i <- arbitrary[Int]) yield i               //> res1: org.scalacheck.Gen[Int] = Gen()
}