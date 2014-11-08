package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevelanceFactor = 100
    val sickDelay = 6
    val sickProbability = 40
    val deadDelay = 14
    val deadProbability = 40
    val immuneDelay = 16
    val healthyDelay = 18
    
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
    if ( (id % prevelanceFactor) == 0) {
      infected = true
      afterDelay(sickDelay) (sickAction)
    }
    
    def moveAction():Unit = {
      if (!dead) {
        val up = if (row == 0) roomRows - 1 else row - 1
        val down = if (row == roomRows - 1) 0 else row + 1
        val left = if (col == 0) roomColumns - 1 else col - 1
        val right = if (col == roomColumns - 1) 0 else col + 1
        
        val roomList = ( List ( (
        				  		  (persons.filter (x => x.col == col && x.row == up && x.sick)).length,
        						  (col, up)
        					    )
        					  )  ++
        			     List ( (
        						  (persons.filter (x => x.col == col && x.row == down && x.sick)).length,
        						  (col, down)
        					    )
        					  )  ++
        			     List ( (
        						  (persons.filter (x => x.col == left && x.row == row && x.sick)).length,
        						  (left, row)
        					    )
        					  )  ++
        			     List ( (
        						  (persons.filter (x => x.col == right && x.row == row && x.sick)).length,
        						  (right, row)
        					    )
        					  )
        				).filter(_._1 == 0)
        			   
            
/*        val upSick = (persons.filter (x => x.col == col && x.row == up && x.sick)).length
        val downSick = (persons.filter (x => x.col == col && x.row == down && x.sick)).length
        val leftSick = (persons.filter (x => x.col == left && x.row == row && x.sick)).length
        val rightSick = (persons.filter (x => x.col == right && x.row == row && x.sick)).length
        
        val newRoom = r.nextInt(4) match {
          case 0 => (left, row)
          case 1 => (right, row)
          case 2 => (col, up)
          case 3 => (col, down)
        }*/
        if (roomList.length > 0) {
          val newRoom = roomList(r.nextInt(roomList.length))._2
          if ( (persons.filter (x => x.col == newRoom._1 && x.row == newRoom._2 && x.infected)).length > 0 && !immune) {
            if (r.nextInt(100) < sickProbability)
              infected = true
              afterDelay(sickDelay) (sickAction)
          } 
        
          col = newRoom._1
          row = newRoom._2

        }
        
        
    	afterDelay(r.nextInt(5)+1) (moveAction)
      }
    }

    def sickAction():Unit = {
    	sick = true
        afterDelay(deadDelay - sickDelay) (deadAction)
     	
    }
        
    def deadAction():Unit = {
    	sick = true
        if (r.nextInt(100) < deadProbability)
          dead = true
        else {
          afterDelay(immuneDelay - deadDelay) (immuneAction)
          afterDelay(healthyDelay - deadDelay) (healthyAction)
        }
     	
    }

    def immuneAction():Unit = {
    	immune = true
    	sick = false
    }
        
    def healthyAction():Unit = {
    	infected = false
    	immune = false
    }
        
   	afterDelay(r.nextInt(5)+1) (moveAction)
    //
    // to complete with simulation logic
    //
  }
}
