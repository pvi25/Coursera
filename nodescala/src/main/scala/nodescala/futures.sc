package nodescala

object futures {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  import scala.concurrent.future
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.util.Random
  type CoffeeBeans = String
  type GroundCoffee = String
  case class Water(temperature: Int)
  type Milk = String
  type FrothedMilk = String
  type Espresso = String
  type Cappuccino = String
  case class GrindingException(msg: String) extends Exception(msg)
  case class FrothingException(msg: String) extends Exception(msg)
  case class WaterBoilingException(msg: String) extends Exception(msg)
  case class BrewingException(msg: String) extends Exception(msg)

  def grind(beans: CoffeeBeans): Future[GroundCoffee] = Future {
    println("start grinding...")
    Thread.sleep(Random.nextInt(2000))
    if (beans == "baked beans") throw GrindingException("are you joking?")
    println("finished grinding...")
    s"ground coffee of $beans"
  }                                               //> grind: (beans: nodescala.futures.CoffeeBeans)scala.concurrent.Future[nodesc
                                                  //| ala.futures.GroundCoffee]

  def heatWater(water: Water): Future[Water] = Future {
    println("heating the water now")
    Thread.sleep(Random.nextInt(2000))
    println("hot, it's hot!")
    water.copy(temperature = 75)
  }                                               //> heatWater: (water: nodescala.futures.Water)scala.concurrent.Future[nodescal
                                                  //| a.futures.Water]

  def frothMilk(milk: Milk): Future[FrothedMilk] = Future {
    println("milk frothing system engaged!")
    Thread.sleep(Random.nextInt(2000))
    println("shutting down milk frothing system")
    s"frothed $milk"
  }                                               //> frothMilk: (milk: nodescala.futures.Milk)scala.concurrent.Future[nodescala.
                                                  //| futures.FrothedMilk]

  def brew(coffee: GroundCoffee, heatedWater: Water): Future[Espresso] = Future {
    println("happy brewing :)")
    Thread.sleep(Random.nextInt(2000))
    println("it's brewed!")
    "espresso"
  }                                               //> brew: (coffee: nodescala.futures.GroundCoffee, heatedWater: nodescala.futur
                                                  //| es.Water)scala.concurrent.Future[nodescala.futures.Espresso]
  def prepareCappuccino(): Future[Cappuccino] = {
    val groundCoffee = grind("arabica beans")
    val heatedWater = heatWater(Water(20))
    val frothedMilk = frothMilk("milk")
    for {
      ground <- groundCoffee
      water <- heatedWater
      foam <- frothedMilk
      espresso <- brew(ground, water)
    } yield espresso + foam
  }                                               //> prepareCappuccino: ()scala.concurrent.Future[nodescala.futures.Cappuccino]
                                                  //| 
  def prepareCappuccinoSequentially(): Future[Cappuccino] = {
    for {
      ground <- grind("arabica beans")
      water <- heatWater(Water(20))
      foam <- frothMilk("milk")
      espresso <- brew(ground, water)
    } yield espresso + foam
  }                                               //> prepareCappuccinoSequentially: ()scala.concurrent.Future[nodescala.futures.
                                                  //| Cappuccino]
  /*
  grind("arabica beans").onSuccess {
    case ground =>
      println("okay, got my ground coffee")
  }*/

  val temperatureOkay: Future[Boolean] = heatWater(Water(25)).map { water =>
    println("we're in the future!")
    (80 to 85).contains(water.temperature)
  }                                               //> heating the water now
                                                  //| temperatureOkay  : scala.concurrent.Future[Boolean] = scala.concurrent.impl
                                                  //| .Promise$DefaultPromise@3af78ba5

  prepareCappuccino.onSuccess {
    //  prepareCappuccinoSequentially.onSuccess {
    case ground =>
      println(s"cappuccino: $ground")
  }                                               //> heating the water now
                                                  //| start grinding...
                                                  //| milk frothing system engaged!

  Thread.sleep(5000)                              //> shutting down milk frothing system
                                                  //| hot, it's hot!
                                                  //| hot, it's hot!
                                                  //| we're in the future!
                                                  //| finished grinding...
                                                  //| happy brewing :)
                                                  //| it's brewed!
                                                  //| cappuccino: espressofrothed milk
   temperatureOkay.map { temp =>
    println(s"temp ok: $temp")}                   //> res0: scala.concurrent.Future[Unit] = scala.concurrent.impl.Promise$Default
                                                  //| Promise@408d4b54
                                                  //| temp ok: false

}