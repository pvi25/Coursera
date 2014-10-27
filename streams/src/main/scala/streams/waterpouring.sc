package streams

import streams._

object waterpouring {
 	val problem = new Pouring(Vector(4,9))    //> problem  : streams.Pouring = streams.Pouring@6093727d
 	 
 	problem.moves                             //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with s
                                                  //| treams.waterpouring.problem.Move] = Vector(Empty(0), Empty(1), Fill(0), Fill
                                                  //| (1), Pour(0,1), Pour(1,0))
  problem.pathSets.take(2).toList                 //> res1: List[Set[streams.waterpouring.problem.Path]] = List(Set(--> Vector(0, 
                                                  //| 0)), Set(Fill(0)--> Vector(4, 0), Fill(1)--> Vector(0, 9)))
  
  problem.solutions(6)                            //> res2: Stream[streams.waterpouring.problem.Path] = Stream(Fill(1) Pour(1,0) E
                                                  //| mpty(0) Pour(1,0) Empty(0) Pour(1,0) Fill(1) Pour(1,0)--> Vector(4, 6), ?)
                                                  
                                                  
val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin                    //> level  : String = ------
                                                  //| --ST--
                                                  //| --oo--
                                                  //| --oo--
                                                  //| ------
val vector = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
                                                  //> vector  : scala.collection.immutable.Vector[scala.collection.immutable.Vecto
                                                  //| r[Char]] = Vector(Vector(S, T), Vector(o, o), Vector(o, o))
vector(1)(1)                                      //> res3: Char = o

val vector2 = Vector('1', '2', '3')               //> vector2  : scala.collection.immutable.Vector[Char] = Vector(1, 2, 3)
vector2.indexWhere (_ == '4')                     //> res4: Int = -1

vector(0).indexWhere (_ == 'T')                   //> res5: Int = 1

vector.zipWithIndex                               //> res6: scala.collection.immutable.Vector[(scala.collection.immutable.Vector[C
                                                  //| har], Int)] = Vector((Vector(S, T),0), (Vector(o, o),1), (Vector(o, o),2))
val pos = (for ((v, i) <- vector.zipWithIndex if v.indexWhere(_ == 'T') != -1) yield (new Pos(i, v.indexWhere(_ == 'T'))))
                                                  //> pos  : scala.collection.immutable.Vector[streams.Pos] = Vector(Pos(0,1))
pos(0)                                            //> res7: streams.Pos = Pos(0,1)

val pos1 = new Pos(1,1)                           //> pos1  : streams.Pos = Pos(1,1)
val pos2 = new Pos(1,1)                           //> pos2  : streams.Pos = Pos(1,1)
pos1 == pos2                                      //> res8: Boolean = true

}

  case class Pos(x: Int, y: Int) {
    /** The position obtained by changing the `x` coordinate by `d` */
    def dx(d: Int) = copy(x = x + d)

    /** The position obtained by changing the `y` coordinate by `d` */
    def dy(d: Int) = copy(y = y + d)
  }