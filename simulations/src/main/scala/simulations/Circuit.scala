package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
     def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
     def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
	  val a1Inv, a2Inv, andWire, inv = new Wire
	  inverter(a1, a1Inv)
	  inverter(a2, a2Inv)
	  andGate(a1Inv, a2Inv, andWire)
	  inverter(andWire, output)
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def demuxLoop (in: Wire, c: List[Wire], out: List[Wire]):Unit = {
      if (c.isEmpty) {
        def demuxAction() {
          val inSig = in.getSignal
          out.head.setSignal(inSig)
        }
        in.addAction(demuxAction)
      }
      else {
        def demuxAction() {
	        val and = new Wire
	        andGate (in, c.head, and)
            demuxLoop(and, c.tail, out.dropRight(out.length/2)) 
        }
        def demuxInvAction() {
	        val inv, and = new Wire
	        inverter(c.head, inv)
	        andGate (in, inv, and)
            demuxLoop(and, c.tail, out.takeRight(out.length/2)) 
        }
        in.addAction(demuxAction)
        c.head.addAction(demuxAction)
        in.addAction(demuxInvAction)
        c.head.addAction(demuxInvAction)
        //demuxLoop(and1,c.tail) ++ demuxLoop(and2,c.tail)
      }
    }
    demuxLoop (in, c, out)
    
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
