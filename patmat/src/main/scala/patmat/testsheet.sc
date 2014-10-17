package patmat

object testsheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  trait Function1[A, B] {
    def apply(x: A): B
  }

  class IntFun extends Function1[Int, Int] {
    def apply(x: Int) = x * x
  }
  class DoubleFun extends Function1[Double, Double] {
    def apply(x: Double) = x * x
  }

  val a = new IntFun                              //> a  : patmat.testsheet.IntFun = patmat.testsheet$$anonfun$main$1$IntFun$1@107
                                                  //| dfdb8
  val b = new DoubleFun                           //> b  : patmat.testsheet.DoubleFun = patmat.testsheet$$anonfun$main$1$DoubleFun
                                                  //| $1@2c12e42b

  (x: Int) => x * x                               //> res0: Int => Int = <function1>
  (y: Int) => y ^ y                               //> res1: Int => Int = <function1>
  (z: Int) => true                                //> res2: Int => Boolean = <function1>

  /*new Function1[Int, Int] {
  	def apply (x: Int) = x*x
  }*/

  a.apply(7)                                      //> res3: Int = 49
  a(8)                                            //> res4: Int = 64
  b(8.0)                                          //> res5: Double = 64.0

  trait List[A] {
    def isEmpty: Boolean
    def head: A
    def tail: List[A]
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
  }

  class Nil[T] extends List[T] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  }

  object List {
    def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
    def apply[T]() = new Nil
  }

  List(9, 19).head                                //> res6: Int = 9
  List(9, 19).tail.head                           //> res7: Int = 19
  List().isEmpty                                  //> res8: Boolean = true
  //List().head
  List(1.0, 2).head                               //> res9: Double = 1.0
  List(1.0, 2).tail.head                          //> res10: Double = 2.0
  List("A", "B").head                             //> res11: String = A
  List("A", 1).tail.head                          //> res12: Any = 1
  List(9, 19).isInstanceOf[Cons[Int]]             //> res13: Boolean = true
  List().isInstanceOf[Nil[Int]]                   //> res14: Boolean = true
  List(9, 19).isInstanceOf[Cons[_]]               //> res15: Boolean = true
  List().isInstanceOf[Cons[_]]                    //> res16: Boolean = false
  List().isInstanceOf[Nil[_]]                     //> res17: Boolean = true

  abstract class Boolean1 {
    def ifThenElse[T](t: => T, e: => T): T

    def &&(x: => Boolean1): Boolean1 = ifThenElse(x, false1)
    object false1 extends Boolean1 {
      def ifThenElse[T](t: => T, e: => T) = e
    }
    object true1 extends Boolean1 {
      def ifThenElse[T](t: => T, e: => T) = t
    }
  }
  /*
  class Boolean2 {
    def <(x: Boolean1) =
      ifThenElse(false, x)
  }*/
  /*class NewBool[Int] extends Boolean1 {
    def ifThenElse[T](t: => T, e: => T): T
  }*/

  //var f: Boolean1 = new NewBool

  //object NewBool extends Boolean1

  /*	class IntTest1 {
		def + (that: Double): Double
		def + (that: Float): Float
		def + (that: Long): Long
		def + (that: Int): Int
		
		def << (cnt: Int): Int
		def & (that: Long): Long
		def & (that: Int): Int
		
		}*/

  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)
    def size: String
    def +(that: Nat): Nat
    def -(that: Nat): Nat
  }

  object Zero extends Nat {
    def isZero = true
    def predecessor = throw new Error("0.predecessor")
    def +(that: Nat) = that
    def -(that: Nat) = if (that.isZero) this else throw new Error("negative number")
    def size = ""
    override def toString = "0"
  }

  class Succ(n: Nat) extends Nat {
    def isZero = false
    def predecessor = n
    def +(that: Nat) = new Succ(n + that)
    def -(that: Nat) = if (that.isZero) this else n - that.predecessor
    def size = "1" + this.predecessor.size
    //    override def toString = "1" + this.predecessor
    override def toString = size.length.toString
  }

  val i = new Succ(Zero)                          //> i  : patmat.testsheet.Succ = 1
  val j = new Succ(new Succ(Zero))                //> j  : patmat.testsheet.Succ = 2
  // i - j
  j - i                                           //> res18: patmat.testsheet.Nat = 1
  j - j                                           //> res19: patmat.testsheet.Nat = 0
  j + j + j + i                                   //> res20: patmat.testsheet.Succ = 7


}