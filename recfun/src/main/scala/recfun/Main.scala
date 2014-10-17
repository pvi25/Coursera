package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0) 0
    else if (c > r) 0
    else if (r > 0) pascal(c - 1, r - 1) + pascal(c, r - 1)
    else 1
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def testBracket(chars: List[Char], level: Int): Int = {
      if (chars.isEmpty) level
      else if (level == -1) -1
      else if (chars.head == '(') testBracket(chars.tail, level + 1)
      else if (chars.head == ')')
        if (level == 0) -1
        else testBracket(chars.tail, level - 1)
      else testBracket(chars.tail, level)
    }

    testBracket(chars, 0) == 0

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def calcTotal(moneyLeft: Int, coins: List[Int]): Int = {
      //println ("moneyLeft: " + moneyLeft + " coins: " + coins + " counter: " + counter + " str:"+ str)

      if (moneyLeft == 0)
        1
      else if (moneyLeft < 0) {
        0
      } else if (coins.isEmpty && moneyLeft >= 1)
        0
      else if (coins.head == 0)
        0
      else {
/*        var sum = 0

        for (cs <- coins.tails if !cs.isEmpty && cs.head != 0) {
          sum += calcTotal(moneyLeft - cs.head, cs, 0) //, str + "+" + cs.head)
        }
        
        counter + sum
*/        
        //println (moneyLeft + " - " + coins)
        calcTotal(moneyLeft, coins.tail) + calcTotal(moneyLeft - coins.head, coins) //, str + "+" + cs.head)
/*        counter + (for (cs <- coins.tails if !cs.isEmpty && cs.head != 0) yield 
          calcTotal(moneyLeft - cs.head, cs, 0)).sum //, str + "+" + cs.head)*/
      }
    }

    //println ("totaal: " + calcTotal(money, coins, 0, ""))
    if (money == 0) 0
    else
      calcTotal(money, coins.sortWith(_.compareTo(_) < 0))
  }
}
