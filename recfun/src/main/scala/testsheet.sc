object testsheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val x = List(1,2,3)                             //> x  : List[Int] = List(1, 2, 3)
  
  println ((for (i <- x) yield i*2).sum)          //> 12
  
  }
  