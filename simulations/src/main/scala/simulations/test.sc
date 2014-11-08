package simulations

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val l = List((0, (1,2)), (1, (2,3)))            //> l  : List[(Int, (Int, Int))] = List((0,(1,2)), (1,(2,3)))
  val l2 = l.filter (_._1 == 0)                   //> l2  : List[(Int, (Int, Int))] = List((0,(1,2)))
  l2(0)._2._1                                     //> res0: Int = 1
  val m = List((0, (1,2))) ++ List((1, (2,3)))    //> m  : List[(Int, (Int, Int))] = List((0,(1,2)), (1,(2,3)))
  val r = scala.util.Random                       //> r  : util.Random.type = scala.util.Random$@6ede1f90
  m(r.nextInt(m.length))                          //> res1: (Int, (Int, Int)) = (1,(2,3))
  r.nextInt(0)                                    //> java.lang.IllegalArgumentException: n must be positive
                                                  //| 	at java.util.Random.nextInt(Unknown Source)
                                                  //| 	at scala.util.Random.nextInt(Random.scala:65)
                                                  //| 	at simulations.test$$anonfun$main$1.apply$mcV$sp(simulations.test.scala:
                                                  //| 12)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at simulations.test$.main(simulations.test.scala:3)
                                                  //| 	at simulations.test.main(simulations.test.scala)
}