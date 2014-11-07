package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevelanceFactor = 100
    val r = scala.util.Random  
    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = for (i <- List(1 to population).flatten) yield new Person(i) // to complete: construct list of persons
  //val persons: List[Person] = List(new Person(1), new Person(2))

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    infected = (id % prevelanceFactor) == 0
    
    def moveAction():Unit = {
    	afterDelay(r.nextInt(5)+1) (moveAction)
    }
    
   	afterDelay(r.nextInt(5)+1) (moveAction)
    //
    // to complete with simulation logic
    //
  }
}
