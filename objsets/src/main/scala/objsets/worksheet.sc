package objsets

/*
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }

  override def toString = "{" + left + elem + right + "}"
}
*/
object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
/*
  object Empty extends IntSet {
    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    def contains(x: Int): Boolean = false
    def union(other: IntSet): IntSet = other
    override def toString = "."
  }

  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
  val t3 = t2 incl 2
  val t5 = new NonEmpty(70, Empty, Empty)
    val t6 = t5 incl 75
  val t7 = t6 incl 32
  val t4 = t3 union t7
*/

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
                                                  //> google  : List[String] = List(android, Android, galaxy, Galaxy, nexus, Nexu
                                                  //| s)

	"Dit is android".contains("android")      //> res0: Boolean = true
	
	google.exists("Dit is android".contains(_))
                                                  //> res1: Boolean = true
}