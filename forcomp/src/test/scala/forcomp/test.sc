package forcomp

object test {
	val word:String = "Woord"                 //> word  : String = Woord
	
	word.toList.sortWith(_ < _)               //> res0: List[Char] = List(W, d, o, o, r)

	word.toLowerCase.toList.sortWith(_ < _).groupBy(x=> x)
                                                  //> res1: scala.collection.immutable.Map[Char,List[Char]] = Map(w -> List(w), d 
                                                  //| -> List(d), r -> List(r), o -> List(o, o))
	word.toList.groupBy(x=> x).toList         //> res2: List[(Char, List[Char])] = List((d,List(d)), (r,List(r)), (W,List(W)),
                                                  //|  (o,List(o, o)))
	word.toLowerCase.toList.groupBy(x=> x).toList.map(x=> (x._1, x._2.length)).sortWith(_._1 < _._1)
                                                  //> res3: List[(Char, Int)] = List((d,1), (o,2), (r,1), (w,1))
                                                  
  type Occurrences = List[(Char, Int)]
  def wordOccurrences(w: String): Occurrences = w.toLowerCase.toList.groupBy(x=> x).toList.map(x=> (x._1, x._2.length)).sortWith(_._1 < _._1)
                                                  //> wordOccurrences: (w: String)forcomp.test.Occurrences
  
	val sentence = List("twee", "woorden", "weet")
                                                  //> sentence  : List[String] = List(twee, woorden, weet)
	
	sentence map (wordOccurrences)            //> res4: List[forcomp.test.Occurrences] = List(List((e,2), (t,1), (w,1)), List(
                                                  //| (d,1), (e,1), (n,1), (o,2), (r,1), (w,1)), List((e,2), (t,1), (w,1)))
	val x1:List[(Char, Int)] = sentence map (wordOccurrences) flatten
                                                  //> x1  : List[(Char, Int)] = List((e,2), (t,1), (w,1), (d,1), (e,1), (n,1), (o,
                                                  //| 2), (r,1), (w,1), (e,2), (t,1), (w,1))
	val x2 = x1.groupBy(x=> x._1)             //> x2  : scala.collection.immutable.Map[Char,List[(Char, Int)]] = Map(e -> List
                                                  //| ((e,2), (e,1), (e,2)), n -> List((n,1)), t -> List((t,1), (t,1)), r -> List(
                                                  //| (r,1)), w -> List((w,1), (w,1), (w,1)), o -> List((o,2)), d -> List((d,1)))
                                                  //| 
	val x3 = x1.groupBy(x=> x._1).toList      //> x3  : List[(Char, List[(Char, Int)])] = List((e,List((e,2), (e,1), (e,2))), 
                                                  //| (n,List((n,1))), (t,List((t,1), (t,1))), (r,List((r,1))), (w,List((w,1), (w,
                                                  //| 1), (w,1))), (o,List((o,2))), (d,List((d,1))))
	val x4 = x3 map(x=> (x._1, x._2.foldLeft(0)((y,z) => y + z._2)))
                                                  //> x4  : List[(Char, Int)] = List((e,5), (n,1), (t,2), (r,1), (w,3), (o,2), (d,
                                                  //| 1))
	x1.foldLeft(0)((r, c) => r + c._2)        //> res5: Int = 15
	
	x1.groupBy(x=> x._1).toList map(x=> (x._1, x._2.foldLeft(0)((y,z) => y + z._2))) sortWith(_._1 < _._1)
                                                  //> res6: List[(Char, Int)] = List((d,1), (e,5), (n,1), (o,2), (r,1), (t,2), (w,
                                                  //| 3))

	sentence map (x => (x, wordOccurrences(x)))
                                                  //> res7: List[(String, forcomp.test.Occurrences)] = List((twee,List((e,2), (t,1
                                                  //| ), (w,1))), (woorden,List((d,1), (e,1), (n,1), (o,2), (r,1), (w,1))), (weet,
                                                  //| List((e,2), (t,1), (w,1))))
	sentence map (x => (x, wordOccurrences(x))) groupBy (_._1) map {case (k,v) => (k,v.map(_._2))}
                                                  //> res8: scala.collection.immutable.Map[String,List[forcomp.test.Occurrences]]
                                                  //|  = Map(weet -> List(List((e,2), (t,1), (w,1))), woorden -> List(List((d,1),
                                                  //|  (e,1), (n,1), (o,2), (r,1), (w,1))), twee -> List(List((e,2), (t,1), (w,1)
                                                  //| )))
	sentence map (x => (x, wordOccurrences(x))) groupBy(_._2) map {case (k,v) => (k,v.map(_._1))}
                                                  //> res9: scala.collection.immutable.Map[forcomp.test.Occurrences,List[String]]
                                                  //|  = Map(List((e,2), (t,1), (w,1)) -> List(twee, weet), List((d,1), (e,1), (n
                                                  //| ,1), (o,2), (r,1), (w,1)) -> List(woorden))
                                                  
  x2('e')                                         //> res10: List[(Char, Int)] = List((e,2), (e,1), (e,2))

  /** Returns the list of all subsets of the occurrence list.
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   */

  def combinations(occurrences: Occurrences): List[Occurrences] = {
  
  	def combLoop (occurrences:Occurrences): Occurrences = occurrences match {
	  	case List() => List()
  		case start :: List() => List()// List(start)
  		case start :: rest => combLoop(rest) //++ (start :: rest) //++ List(List(start) :: combinations(rest))//++ combinations( //++
  	}
  	
  	//List(combLoop (occurrences))
  	
  	occurrences match {
  	case List() => List()
  	case start :: List() => List(List(start))
  	case start :: rest => List(List(start)) ++//:: combinations(rest) ++
  		List(start :: rest) ++
  		combinations (rest) ++
//  		combinations (List(start) ++ List(rest.head))
  		combinations (List(start) ++ rest.tail)
  		//List(start :: List(rest.head))
  		//List(start :: rest.tail)
  		//{List(List(start) ++ combLoop(rest))} //++ List(List(start) ++ combLoop(rest))//++ combinations( //++

//  	case start :: rest => List(start) :: combinations(rest) ++ List(occurrences) //++
//  			{ if (start._2 > 1) combinations (List((start._1, start._2 - 1)) ++ rest) else List() }//:: List(start :: rest)
  }
    }                                             //> combinations: (occurrences: forcomp.test.Occurrences)List[forcomp.test.Occu
                                                  //| rrences]
  wordOccurrences ("aabbcc")                      //> res11: forcomp.test.Occurrences = List((a,2), (b,2), (c,2))
  combinations(wordOccurrences ("aabbcc")).toSet  //> res12: scala.collection.immutable.Set[forcomp.test.Occurrences] = Set(List(
                                                  //| (b,2)), List((a,2), (c,2)), List((a,2), (b,2), (c,2)), List((b,2), (c,2)), 
                                                  //| List((c,2)), List((a,2)))
  def comb2(occurrences: Occurrences): List[Occurrences] = {
  
  	occurrences match {
  	case List() => List()
  	case start :: List() => List(List(start))
  	case start :: rest => List(List(start)) ++ {//:: combinations(rest) ++
  		for (xs <- rest) yield comb2 (List(start) ++ rest dropWhile { _ != xs })
  		}.flatten ++
  		comb2 (rest)
  }
    }                                             //> comb2: (occurrences: forcomp.test.Occurrences)List[forcomp.test.Occurrences
                                                  //| ]

  comb2(wordOccurrences ("aabbcc"))               //> res13: List[forcomp.test.Occurrences] = List(List((a,2)), List((b,2)), List
                                                  //| ((c,2)), List((c,2)), List((c,2)), List((b,2)), List((c,2)), List((c,2)))

	//wordOccurrences ("aabbcc").toSet[(Char, Int)]//.map(y => y.toList)

	wordOccurrences ("aabb").toSet[(Char, Int)].subsets.map(_.toList).toList
                                                  //> res14: List[List[(Char, Int)]] = List(List(), List((a,2)), List((b,2)), Lis
                                                  //| t((a,2), (b,2)))
	val wo = wordOccurrences ("ccaaabbb")     //> wo  : forcomp.test.Occurrences = List((a,3), (b,3), (c,2))
	
	wo filterNot (_ == ('a', 2))              //> res15: List[(Char, Int)] = List((a,3), (b,3), (c,2))
  for (x <- wo; y <- x._2 until 0 by -1) yield (x._1, y) :: {wo filterNot(_ == x)}
                                                  //> res16: List[List[(Char, Int)]] = List(List((a,3), (b,3), (c,2)), List((a,2)
                                                  //| , (b,3), (c,2)), List((a,1), (b,3), (c,2)), List((b,3), (a,3), (c,2)), List
                                                  //| ((b,2), (a,3), (c,2)), List((b,1), (a,3), (c,2)), List((c,2), (a,3), (b,3))
                                                  //| , List((c,1), (a,3), (b,3)))


	def combs (occurrences: Occurrences): List[Occurrences] = {
		def combLoop(firstOccurrences:Occurrences, restOccurrences:Occurrences): List[Occurrences] = restOccurrences match {
			case x :: List() => for (y <- 1 to x._2) yield List(x) :: firstOccurrences
			List(firstOccurrences)
		}
		for (x <- occurrences; y <- 1 to x._2) yield (x._1, y) :: {occurrences filterNot(_ == x)}
	  //occurrences filterNot(_ == List(('a', 2)))
	  //List(occurrences)
	}                                         //> combs: (occurrences: forcomp.test.Occurrences)List[forcomp.test.Occurrences
                                                  //| ]
  combs(wo)                                       //> res17: List[forcomp.test.Occurrences] = List(List((a,1), (b,3), (c,2)), Lis
                                                  //| t((a,2), (b,3), (c,2)), List((a,3), (b,3), (c,2)), List((b,1), (a,3), (c,2)
                                                  //| ), List((b,2), (a,3), (c,2)), List((b,3), (a,3), (c,2)), List((c,1), (a,3),
                                                  //|  (b,3)), List((c,2), (a,3), (b,3)))
 // for { comb <- wo; n <- 1 to comb._2 } yield (comb._1, n) :: comb
 
	(wo foldRight List[Occurrences](Nil)) { case ((ch,tm), acc) => {
		acc ++ ( for { comb <- acc; n <- 1 to tm } yield (ch, n) :: comb ) } }
                                                  //> res18: List[forcomp.test.Occurrences] = List(List(), List((c,1)), List((c,2
                                                  //| )), List((b,1)), List((b,2)), List((b,3)), List((b,1), (c,1)), List((b,2), 
                                                  //| (c,1)), List((b,3), (c,1)), List((b,1), (c,2)), List((b,2), (c,2)), List((b
                                                  //| ,3), (c,2)), List((a,1)), List((a,2)), List((a,3)), List((a,1), (c,1)), Lis
                                                  //| t((a,2), (c,1)), List((a,3), (c,1)), List((a,1), (c,2)), List((a,2), (c,2))
                                                  //| , List((a,3), (c,2)), List((a,1), (b,1)), List((a,2), (b,1)), List((a,3), (
                                                  //| b,1)), List((a,1), (b,2)), List((a,2), (b,2)), List((a,3), (b,2)), List((a,
                                                  //| 1), (b,3)), List((a,2), (b,3)), List((a,3), (b,3)), List((a,1), (b,1), (c,1
                                                  //| )), List((a,2), (b,1), (c,1)), List((a,3), (b,1), (c,1)), List((a,1), (b,2)
                                                  //| , (c,1)), List((a,2), (b,2), (c,1)), List((a,3), (b,2), (c,1)), List((a,1),
                                                  //|  (b,3), (c,1)), List((a,2), (b,3), (c,1)), List((a,3), (b,3), (c,1)), List(
                                                  //| (a,1), (b,1), (c,2)), List((a,2), (b,1), (c,2)), List((a,3), (b,1), (c,2)),
                                                  //|  List((a,1), (b,2), (c,
                                                  //| Output exceeds cutoff limit.

	val wo2 = wordOccurrences("aab")          //> wo2  : forcomp.test.Occurrences = List((a,2), (b,1))

/*val wo4: List[Char, Int] = for ((key, value) <- (wo ++ wo2).groupBy(_._1)) yield {
	if (value.length ==1) (key, value(1)._2)
	else (key, value(1)._2)
	}*/
	
val wo3 = (wo ++ wo2).groupBy(_._1).toList        //> wo3  : List[(Char, List[(Char, Int)])] = List((b,List((b,3), (b,1))), (a,Li
                                                  //| st((a,3), (a,2))), (c,List((c,2))))

val wo4 = wo3 map {
 		case (key, List((a,b:Int))) => ((a,b))
 		case (key, List((a,b:Int), (c,d:Int))) => (a,(b-d))
 		}                                 //> wo4  : List[(Char, Int)] = List((b,2), (a,1), (c,2))
 
 wo4 filter (x=> x._2 != 0)  sorted               //> res19: List[(Char, Int)] = List((a,1), (b,2), (c,2))
 	/*for (x <- wo3) yield {
 		case (key, value) => (key, value)
 		}*/
 		
 		"Mickey Mouse".filter(_ != ' ')   //> res20: String = MickeyMouse
 		val anagramtest = combinations(wordOccurrences("Mickey Mouse".filter(_ != ' ')))
                                                  //> anagramtest  : List[forcomp.test.Occurrences] = List(List((c,1)), List((c,1
                                                  //| ), (e,2), (i,1), (k,1), (m,2), (o,1), (s,1), (u,1), (y,1)), List((e,2)), Li
                                                  //| st((e,2), (i,1), (k,1), (m,2), (o,1), (s,1), (u,1), (y,1)), List((i,1)), Li
                                                  //| st((i,1), (k,1), (m,2), (o,1), (s,1), (u,1), (y,1)), List((k,1)), List((k,1
                                                  //| ), (m,2), (o,1), (s,1), (u,1), (y,1)), List((m,2)), List((m,2), (o,1), (s,1
                                                  //| ), (u,1), (y,1)), List((o,1)), List((o,1), (s,1), (u,1), (y,1)), List((s,1)
                                                  //| ), List((s,1), (u,1), (y,1)), List((u,1)), List((u,1), (y,1)), List((y,1)),
                                                  //|  List((u,1)), List((s,1)), List((s,1), (y,1)), List((y,1)), List((s,1)), Li
                                                  //| st((o,1)), List((o,1), (u,1), (y,1)), List((u,1)), List((u,1), (y,1)), List
                                                  //| ((y,1)), List((u,1)), List((o,1)), List((o,1), (y,1)), List((y,1)), List((o
                                                  //| ,1)), List((m,2)), List((m,2), (s,1), (u,1), (y,1)), List((s,1)), List((s,1
                                                  //| ), (u,1), (y,1)), List((u,1)), List((u,1), (y,1)), List((y,1)), List((u,1))
                                                  //| , List((s,1)), List((s,
                                                  //| Output exceeds cutoff limit.
   
   
    for (comb <- anagramtest) yield comb match {
     case found:Occurrences => found }            //> res21: List[List[(Char, Int)]] = List(List((c,1)), List((c,1), (e,2), (i,1)
                                                  //| , (k,1), (m,2), (o,1), (s,1), (u,1), (y,1)), List((e,2)), List((e,2), (i,1)
                                                  //| , (k,1), (m,2), (o,1), (s,1), (u,1), (y,1)), List((i,1)), List((i,1), (k,1)
                                                  //| , (m,2), (o,1), (s,1), (u,1), (y,1)), List((k,1)), List((k,1), (m,2), (o,1)
                                                  //| , (s,1), (u,1), (y,1)), List((m,2)), List((m,2), (o,1), (s,1), (u,1), (y,1)
                                                  //| ), List((o,1)), List((o,1), (s,1), (u,1), (y,1)), List((s,1)), List((s,1), 
                                                  //| (u,1), (y,1)), List((u,1)), List((u,1), (y,1)), List((y,1)), List((u,1)), L
                                                  //| ist((s,1)), List((s,1), (y,1)), List((y,1)), List((s,1)), List((o,1)), List
                                                  //| ((o,1), (u,1), (y,1)), List((u,1)), List((u,1), (y,1)), List((y,1)), List((
                                                  //| u,1)), List((o,1)), List((o,1), (y,1)), List((y,1)), List((o,1)), List((m,2
                                                  //| )), List((m,2), (s,1), (u,1), (y,1)), List((s,1)), List((s,1), (u,1), (y,1)
                                                  //| ), List((u,1)), List((u,1), (y,1)), List((y,1)), List((u,1)), List((s,1)), 
                                                  //| List((s,1), (y,1)), Lis
                                                  //| Output exceeds cutoff limit.
        //case found => found
  
                                                   
}