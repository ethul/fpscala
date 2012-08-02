package fpscala
package chapter2

object section5 {
  object exercise1 {
    val abs: Int => Int =
      n => if (n < 0) -n else n

    private def formatAbs(x: Int) = {
      val s = "The absolute value of %d is %d."
      s.format(x, abs(x))
    }

    def main(args: Array[String]): Unit =
      println(formatAbs(-42))
  }

  object exercise2 {
    def isEven(n: Int): Boolean =
      n % 2 == 0

    val isOdd: Int => Boolean =
      not(isEven)

    def not(p: Int => Boolean): Int => Boolean =
      a => !p(a)

    def main(args: Array[String]): Unit =
      println("42 isEven? " + isEven(42) + " isOdd? " + isOdd(42))
  } 
}
