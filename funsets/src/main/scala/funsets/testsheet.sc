package funsets

object testsheet {
  println("Welcome to the Scala worksheet")

  def sumInts(a: Int, b: Int): Int =
    if (a > b) 0 else a + sumInts(a + 1, b)

  sumInts(2, 4)

  def cube(x: Int): Int = x * x * x

  def sumCube(a: Int, b: Int): Int =
    if (a > b) 0 else cube(a) + sumCube(a + 1, b)

  sumCube(2, 4)

  def sum2(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum2(f, a + 1, b)

  def sumCube2(a: Int, b: Int): Int = sum2(cube, a, b)

  sumCube2(2, 4)

  def id(a: Int): Int = a
  def sumInts2(a: Int, b: Int): Int = sum2(id, a, b)

  sumInts2(2, 4)

  def sumInts3(a: Int, b: Int): Int = sum2(x => x, a, b)

  sumInts3(2, 4)

  def sumCube3(a: Int, b: Int): Int = sum2(x => x * x * x, a, b)

  sumCube3(2, 4)

  def sum3(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    loop(a, 0)
  }

  sum3(x => x * x * x, 2, 4)

  def sumCube4(a: Int, b: Int) = sum3(x => x * x * x, a, b)

  sumCube4(2, 4)

  def sum4(f: Int => Int): (Int, Int) => Int = { // Function which returns a function (which takes two ints as param and returns an int)
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }

  sum4(x => x * x * x)(2, 4)

  def sumInts4 = sum4(x => x)

  sumInts4(2, 4)

  sum4(cube)(2, 4)

  def sum5(f: Int => Int)(a: Int, b: Int): Int = // Same as sum4
    if (a > b) 0
    else f(a) + sum5(f)(a + 1, b)

  sum5(x => x * x * x)(2, 4)

  sum5(cube)(2, 4)

  def product(f: Int => Int)(a: Int, b: Int): Int = // Same as sum4
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  product(x => x)(2, 4)

  def factorial(a: Int): Int = product(x => x)(1, a)

  factorial(4)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  mapReduce(x => x * x * x, (a, b) => a + b, 0)(2, 4)

  mapReduce(x => x, (a, b) => a * b, 1)(2, 4)

  def factorial2(y: Int): Int = mapReduce(x => x, (a, b) => a * b, 1)(1, y)

  factorial2(4)
  
  
  List(1, 2, 3) foreach { _ => println("Hi") }
  List(1, 2, 3) map (_ + 2)
  List(1, 2, 3) foreach println _ //+ "Hi"

}