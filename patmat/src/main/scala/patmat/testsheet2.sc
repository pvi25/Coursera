package patmat

object testsheet2 {

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  val ls = List("Mary", "had", "a", "little", "lamb")
                                                  //> ls  : List[String] = List(Mary, had, a, little, lamb)
  ls.foreach(e => println(e.toUpperCase))         //> MARY
                                                  //| HAD
                                                  //| A
                                                  //| LITTLE
                                                  //| LAMB
  val l1 = List('A', 'B')                         //> l1  : List[Char] = List(A, B)
  val l2 = List('C', 'D')                         //> l2  : List[Char] = List(C, D)
  val l22 = List('B', 'C')                        //> l22  : List[Char] = List(B, C)
  val l3 = l1 ++ l2 ++ l1 ++ l22                  //> l3  : List[Char] = List(A, B, C, D, A, B, B, C)

	l1.contains('A')                          //> res0: Boolean = true
	l1.contains('C')                          //> res1: Boolean = false
	l1.length                                 //> res2: Int = 2
	l1 ++ {if (l1.contains('C')) l2 else l1}  //> res3: List[Char] = List(A, B, A, B)
	
  l3.tail                                         //> res4: List[Char] = List(B, C, D, A, B, B, C)
  l3.groupBy(l => l)                              //> res5: scala.collection.immutable.Map[Char,List[Char]] = Map(D -> List(D), A 
                                                  //| -> List(A, A), C -> List(C, C), B -> List(B, B, B))
  l3.groupBy(l => l).map(t => (t._1, t._2.length))//> res6: scala.collection.immutable.Map[Char,Int] = Map(D -> 1, A -> 2, C -> 2,
                                                  //|  B -> 3)

  val l4 = l3.groupBy(l => l).map(t => (t._1, t._2.length)).toList
                                                  //> l4  : List[(Char, Int)] = List((D,1), (A,2), (C,2), (B,3))

  l4.sorted                                       //> res7: List[(Char, Int)] = List((A,2), (B,3), (C,2), (D,1))
  l4(1)._2                                        //> res8: Int = 2
  l4.sortWith(_._2 < _._2)                        //> res9: List[(Char, Int)] = List((D,1), (A,2), (C,2), (B,3))
  val l5 = l4.sortWith(_._2 < _._2).map(x => new Leaf(x._1, x._2))
                                                  //> l5  : List[patmat.testsheet2.Leaf] = List(Leaf(D,1), Leaf(A,2), Leaf(C,2), L
                                                  //| eaf(B,3))
  l4.filter(t => t._2 > 2)                        //> res10: List[(Char, Int)] = List((B,3))
  l4.filter(t => t._2 > 3) ++ l4.filter(t => t._2 <= 3)
                                                  //> res11: List[(Char, Int)] = List((D,1), (A,2), (C,2), (B,3))

  def weight(tree: CodeTree): Int = tree match {
    case Fork(left, right, chars, weight2) => weight(left) + weight(right) // tree match ...
    case Leaf(char, weight) => weight
  }                                               //> weight: (tree: patmat.testsheet2.CodeTree)Int

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(left, right, chars2, weight) => chars(left) ::: chars(right) // tree match ...
    case Leaf(char: Char, weight: Int) => List(char)
  }                                               //> chars: (tree: patmat.testsheet2.CodeTree)List[Char]

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1
                                                  //> singleton: (trees: List[patmat.testsheet2.CodeTree])Boolean

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    if (singleton(trees)) trees
    else {
      val combinedweight = weight(trees(0)) + weight(trees(1))
      combine(
        trees.tail.tail.filter(t => weight(t) < combinedweight) ++
          List(new Fork(trees(0), trees(1), chars(trees(0)) ++ chars(trees(1)), combinedweight)) ++
          trees.tail.tail.filter(t => weight(t) >= combinedweight))
    }
  }                                               //> combine: (trees: List[patmat.testsheet2.CodeTree])List[patmat.testsheet2.Co
                                                  //| deTree]

  combine(l5)                                     //> res12: List[patmat.testsheet2.CodeTree] = List(Fork(Leaf(B,3),Fork(Leaf(C,2
                                                  //| ),Fork(Leaf(D,1),Leaf(A,2),List(D, A),3),List(C, D, A),5),List(B, C, D, A),
                                                  //| 8))
                                                  
	val q1 = List(('A', List(1,1,0)), ('B', List (0,0,1)))
                                                  //> q1  : List[(Char, List[Int])] = List((A,List(1, 1, 0)), (B,List(0, 0, 1)))
                                                  //| 
	val q2 = List(('B', List(1,1,0)), ('C', List (0,0,1)))
                                                  //> q2  : List[(Char, List[Int])] = List((B,List(1, 1, 0)), (C,List(0, 0, 1)))
                                                  //| 
 	q1 :: q2                                  //> res13: List[Product with java.io.Serializable] = List(List((A,List(1, 1, 0)
                                                  //| ), (B,List(0, 0, 1))), (B,List(1, 1, 0)), (C,List(0, 0, 1)))
	
 	(q1 ++ q2).groupBy(_._1)                  //> res14: scala.collection.immutable.Map[Char,List[(Char, List[Int])]] = Map(A
                                                  //|  -> List((A,List(1, 1, 0))), C -> List((C,List(0, 0, 1))), B -> List((B,Lis
                                                  //| t(0, 0, 1)), (B,List(1, 1, 0))))
 	(q1 ++ q2).groupBy(_._1).map(kv => (kv._2(0)))
                                                  //> res15: scala.collection.immutable.Map[Char,List[Int]] = Map(A -> List(1, 1,
                                                  //|  0), C -> List(0, 0, 1), B -> List(0, 0, 1))
 	(q1 ++ q2).groupBy(_._1).map(kv => (kv._2(0))).toList
                                                  //> res16: List[(Char, List[Int])] = List((A,List(1, 1, 0)), (C,List(0, 0, 1)),
                                                  //|  (B,List(0, 0, 1)))
	//( q1 ++ q2 ).groupBy( _._1 ).map( kv => (kv._1, kv._2.map( _._2).sum ) ).toList
	
	"abc".toList map (x => x.toUpper)         //> res17: List[Char] = List(A, B, C)
	def toUpper(x: Char) = x.toUpper          //> toUpper: (x: Char)Char
	"abc".toList map toUpper                  //> res18: List[Char] = List(A, B, C)
	
 l3.foldLeft(1)(_.toInt * _.toInt)                //> res19: Int = 398294816
}