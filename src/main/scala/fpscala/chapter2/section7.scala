package fpscala
package chapter2

object section7 {
  import annotation.tailrec

  object exercise1 {
    def fib(i: Int): Int =
      if (i < 3) 1
      else fib(i - 1) + fib(i - 2)

    def fibTail(i: Int): Int = {
      @tailrec
      def helper(i: Int, acc1: Int, acc2: Int): Int =
        if (i < 3) acc1
        else helper(i - 1, acc1 + acc2, acc1)

      helper(i, 1, 1)
    }

    def main(args: Array[String]): Unit = {
      println("The 10th Fibonacci number is " + fib(10))
      println("The 10th Fibonacci number is " + fibTail(1000))
    }
  }

  object exercise2 {
    import section6.Pred

    def sqrt(n: Double): Double = {
      def f(x: Double) = (x * x) - n
      iterateWhile(2.0)(x => x - f(x) / (2 * x), x => f(x).abs > 1e-14)
    }

    @tailrec
    def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =
      if (!p(a)) a else iterateWhile(f(a))(f, p)

    def main(args: Array[String]): Unit =
      println("The approximate square root of 2 is " + sqrt(2.0))
  }
}
