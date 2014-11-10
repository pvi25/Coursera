package nodescala
import scala.language.postfixOps
//import scala.util.{Try, Success, Failure}
//import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
//import scala.concurrent.duration._
//import scala.async.Async.{async, await}
//import org.scalatest._
//import NodeScala._

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  //Thread.sleep(10 second)
  1==1                                            //> res0: Boolean(true) = true
  
  val tryDivideByZeroAgain = future {
      Thread.sleep(1000)
      1 / 0
    } recover {
      case e: ArithmeticException => "Infinity"
    }                                             //> tryDivideByZeroAgain  : scala.concurrent.Future[Any] = scala.concurrent.impl
                                                  //| .Promise$DefaultPromise@70edf123

    tryDivideByZeroAgain onSuccess {
      case e => Console.println("Succes: " + e)
    }

    tryDivideByZeroAgain onFailure {
      case e => Console.println("Failure: " + e)
    }

    Console.println("Try dividing by zero, recover from exception..")
                                                  //> Try dividing by zero, recover from exception..

    Thread.sleep(2000)                            //> Succes: Infinity|

  
}