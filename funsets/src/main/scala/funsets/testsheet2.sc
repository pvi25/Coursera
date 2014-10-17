package funsets

import scala.util.Random

object testsheet2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: funsets.testsheet2.Set, elem: Int)Boolean

  def singletonSet(elem: Int): Set = elem => elem == elem
                                                  //> singletonSet: (elem: Int)funsets.testsheet2.Set
  
  val s1 = singletonSet(2)                        //> s1  : funsets.testsheet2.Set = <function1>
  val s2 = singletonSet(3)                        //> s2  : funsets.testsheet2.Set = <function1>

  2 % 2                                           //> res0: Int(0) = 0
  3 % 2                                           //> res1: Int(1) = 1
  
def handleGreetingRequest(request: String) =
	Map("greetings" -> request.split(",").toList.map(makeGreeting))
                                                  //> handleGreetingRequest: (request: String)scala.collection.immutable.Map[Strin
                                                  //| g,List[String]]
	
def random = new Random()                         //> random: => scala.util.Random

def greetings = Vector("Hello", "Greetings", "Salutations", "Hola")
                                                  //> greetings: => scala.collection.immutable.Vector[String]

def makeGreeting(name: String) =
"%s, %s".format(greetings(random.nextInt(greetings.size)), name)
                                                  //> makeGreeting: (name: String)String

val x = "1,2,3,4".split(",").toList.map(makeGreeting)
                                                  //> x  : List[String] = List(Salutations, 1, Hola, 2, Hola, 3, Hola, 4)
val y = "1,2,3,4".split(",")                      //> y  : Array[String] = Array(1, 2, 3, 4)
val z = "1,2,3,4".split(",").toList               //> z  : List[String] = List(1, 2, 3, 4)
val a = "1,2,a,4".split(",").toList.map(z => z.toUpperCase)
                                                  //> a  : List[String] = List(1, 2, A, 4)
val b = "1,2,a,4".split(",").toList.map(z => z.toUpperCase).length
                                                  //> b  : Int = 4
val c = "1,2,a,4".split(",").toList.map(z => z+","+z.toUpperCase).length
                                                  //> c  : Int = 4

println (handleGreetingRequest("1,2,3,4"))        //> Map(greetings -> List(Greetings, 1, Greetings, 2, Hello, 3, Greetings, 4))

println (handleGreetingRequest("1,2,3,4").size)   //> 1
println (handleGreetingRequest("1,2,3,4")("greetings").length)}
                                                  //> 4