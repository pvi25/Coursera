package patmat

object testsheet3 {
  val l1 = List('A', 'B', 'C', 'D')               //> l1  : List[Char] = List(A, B, C, D)
  val l2 = List(6, 3, 12, 9, 34, 2)               //> l2  : List[Int] = List(6, 3, 12, 9, 34, 2)

  l1.length                                       //> res0: Int = 4
  l1.last                                         //> res1: Char = D
  l1.init                                         //> res2: List[Char] = List(A, B, C)
  l1 take 2                                       //> res3: List[Char] = List(A, B)
  l1 drop 2                                       //> res4: List[Char] = List(C, D)
  l1(3)                                           //> res5: Char = D
  l1.reverse                                      //> res6: List[Char] = List(D, C, B, A)
  l1 updated (2, 'Z')                             //> res7: List[Char] = List(A, B, Z, D)
  l1 indexOf 'B'                                  //> res8: Int = 1
  l1 indexOf 'Z'                                  //> res9: Int = -1
  l1 contains 'B'                                 //> res10: Boolean = true
  l1 contains 'Z'                                 //> res11: Boolean = false

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }                                               //> reverse: [T](xs: List[T])List[T]
  reverse(l1)                                     //> res12: List[Char] = List(D, C, B, A)

  def removeAt[T](xs: List[T], n: Int): List[T] = xs match {
    case List() => List()
    case y :: ys => xs.take(n) ::: xs.drop(n + 1)
  }                                               //> removeAt: [T](xs: List[T], n: Int)List[T]

  removeAt(l1, 2)                                 //> res13: List[Char] = List(A, B, D)
  removeAt(l1, 12)                                //> res14: List[Char] = List(A, B, C, D)

  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) =>
            if (lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }
      val (fst, snd) = xs splitAt n
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }                                               //> msort: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]

  msort(l2.reverse)(_ < _)                        //> res15: List[Int] = List(2, 3, 6, 9, 12, 34)

  l2 map (_ * 2)                                  //> res16: List[Int] = List(12, 6, 24, 18, 68, 4)

l2                                                //> res17: List[Int] = List(6, 3, 12, 9, 34, 2)
  l2 filter (_ < 10)                              //> res18: List[Int] = List(6, 3, 9, 2)
  l2 filterNot (_ < 10)                           //> res19: List[Int] = List(12, 34)
  l2 partition (_ < 10)                           //> res20: (List[Int], List[Int]) = (List(6, 3, 9, 2),List(12, 34))
  l2 takeWhile (_ < 10)                           //> res21: List[Int] = List(6, 3)
  l2 dropWhile (_ < 10)                           //> res22: List[Int] = List(12, 9, 34, 2)
  l2 span (_ < 10)                                //> res23: (List[Int], List[Int]) = (List(6, 3),List(12, 9, 34, 2))

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]

  val data = List('a', 'a', 'a', 'b', 'c', 'c', 'a')
                                                  //> data  : List[Char] = List(a, a, a, b, c, c, a)
  pack(data)                                      //> res24: List[List[Char]] = List(List(a, a, a), List(b), List(c, c), List(a))
                                                  //| 
  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs) map (ys => (ys.head, ys.length))
    //    pack(xs) map ((_.head, _.length))
  }                                               //> encode: [T](xs: List[T])List[(T, Int)]

  encode(data)                                    //> res25: List[(Char, Int)] = List((a,3), (b,1), (c,2), (a,1))

  l2.reduceLeft(_ + _)                            //> res26: Int = 66
  l2.reduceLeft(_ * _)                            //> res27: Int = 132192
  ('a', 3)._2                                     //> res28: Int = 3
  //  encode(data).reduceLeft(ys => ys._2 + ys._2)
  encode(data).map(_._2).reduceLeft(_ + _)        //> res29: Int = 7

  val l3 = l2 filter (_ > 100)                    //> l3  : List[Int] = List()
  //l3.reduceLeft(_ + _)
  l3.foldLeft(0)(_ + _)                           //> res30: Int = 0

  data.foldLeft(0)(_.toInt + _.toInt)             //> res31: Int = 684
  
  data ++ List('z')                               //> res32: List[Char] = List(a, a, a, b, c, c, a, z)
  //data ++ 'z'
  //data ::: 'z'
  'z' :: data                                     //> res33: List[Char] = List(z, a, a, a, b, c, c, a)
  
	l2 map (x => x.toString mkString "-")     //> res34: List[String] = List(6, 3, 1-2, 9, 3-4, 2)
	l2 map (x => x.toString )                 //> res35: List[String] = List(6, 3, 12, 9, 34, 2)
	l2 map (_.toString)                       //> res36: List[String] = List(6, 3, 12, 9, 34, 2)
	l2 map (x => x.toString) mkString ("(","-",")")
                                                  //> res37: String = (6-3-12-9-34-2)
	msort(l2)(_ < _) map (x => x.toString mkString "-")
                                                  //> res38: List[String] = List(2, 3, 6, 9, 1-2, 3-4)
 
  "abc" mkString "-"                              //> res39: String = a-b-c
}