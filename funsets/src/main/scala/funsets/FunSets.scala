package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = x => (x == elem)
//  def singletonSet(elem: Int): Set = _ == elem

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = x => (contains(s, x) || contains(t, x))
//  def union(s: Set, t: Set): Set = s(elem) || t(elem)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = x => contains(s, x) && contains(t, x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = x => contains(s, x) && !contains(t, x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = x => contains(s, x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000
  val vals = -bound to bound

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains (s, a) && (!contains(filter(s, p), a))) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  def forall2(s: Set, p: Int => Boolean): Boolean = 
    vals.toList.filter(s).forall(p)
    //vals.filter(s).forall(p)
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, a => !p(a))
//  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))
/*
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (contains(filter(s, p), a)) true
      else if (a >= bound) false
      else iter(a + 1)
    }
    iter(-bound)
  }*/
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = a => exists (s, elem => f(elem) == a)
//  def map(s: Set, f: Int => Int): Set = a => exists (s, elem => f(elem) == a)
  /*{
    def iter(a: Int, accum: Set): Set = {
      if (a > bound) accum
      else if (contains(s, a)) { 
        //println(a)
        iter(a + 1, union(singletonSet(f(a)), accum))
      }
      else iter(a + 1, accum)
    }
    iter(-bound, x => x != x)
  }*/
  
  def empty(s:Set):Boolean =
//  	!vals.exists(s)
  	!exists(s, x => true)
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
