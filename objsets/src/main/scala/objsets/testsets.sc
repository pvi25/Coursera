package objsets

//import objsets.TweetSet
//  import TweetSet._

object testsets {

  val t = 1                                       //> t  : Int = 1
  //  val t1 = new Tweet("User1", "Tweet1", 1)
  //val s1 = NonEmpty (t1, new Empty, new Empty)

  val capitals = Map("France" -> "Paris", "Japan" -> "Tokyo")
                                                  //> capitals  : scala.collection.immutable.Map[String,String] = Map(France -> Pa
                                                  //| ris, Japan -> Tokyo)

  println("capitals.get( \"France\" ) : " + capitals.get("France"))
                                                  //> capitals.get( "France" ) : Some(Paris)
  println("capitals.get( \"India\" ) : " + capitals.get("India"))
                                                  //> capitals.get( "India" ) : None

  //def findPerson(key: Int): Option[Boolean] = key == key
  def show(x: Option[String]) = x match {
    case Some(s) => s
    case None => "?"
  }                                               //> show: (x: Option[String])String

  capitals.get("France")                          //> res0: Option[String] = Some(Paris)
  capitals.get("India")                           //> res1: Option[String] = None
  show(capitals.get("India"))                     //> res2: String = ?
  show(capitals.get("France"))                    //> res3: String = Paris

     val a:Option[Int] = Some(5)                  //> a  : Option[Int] = Some(5)
      val b:Option[Int] = None                    //> b  : Option[Int] = None
      
      println("a.getOrElse(0): " + a.getOrElse(0) )
                                                  //> a.getOrElse(0): 5
      println("b.getOrElse(10): " + b.getOrElse(10) )
                                                  //> b.getOrElse(10): 10
                                                  
List(1, 2, 3) foreach { _ => println("Hi") }      //> Hi
                                                  //| Hi
                                                  //| Hi


//List("1", "2", "3") foreach { x => println("Hi"+ _) }

}