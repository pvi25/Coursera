package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }  

  test("demux example") {
    val in1, c1, c2, o1, o2, o3, o4 = new Wire
    val demuxresult = List(o1, o2, o3, o4)
    demux(in1, List(c1, c2), demuxresult)
    in1.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(false)
    run
    
    assert(o1.getSignal === false, "and 1-1")
    assert(o2.getSignal === false, "and 1-2")
    assert(o3.getSignal === false, "and 1-3")
    assert(o4.getSignal === false, "and 1-4")

    in1.setSignal(true)
    run

    assert(o1.getSignal === false, "and 2-1")
    assert(o2.getSignal === false, "and 2-2")
    assert(o3.getSignal === false, "and 2-3")
    assert(o4.getSignal === true, "and 2-4")
    
    c2.setSignal(true)
    run

    assert(o1.getSignal === false, "and 3-1")
    assert(o2.getSignal === false, "and 3-2")
    assert(o3.getSignal === true, "and 3-3")
    assert(o4.getSignal === false, "and 3-4")
    
    c1.setSignal(true)
    run

    assert(o1.getSignal === true, "and 4-1")
    assert(o2.getSignal === false, "and 4-2")
    assert(o3.getSignal === false, "and 4-3")
    assert(o4.getSignal === false, "and 4-4")
    
    /*
    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")*/
  }  

  //
  // to complete with tests for orGate, demux, ...
  //

}
