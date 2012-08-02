package fpscala
package chapter2

object section6 {
  type Pred[A] = A => Boolean

  object exercise1 {
    def isDivisbleBy(k: Int): Pred[Int] =
      a => a % k == 0

    def isEven(n: Int): Boolean =
      isDivisbleBy(2)(n)

    def main(args: Array[String]): Unit =
      println("42 isEven? " + isEven(42))
  }

  object exercise2 {
    def lift[A](op: (A, A) => Boolean): Pred[A] =
      a => op(a, a)

    def isDivisibleBy3And5: Pred[Int] =
      lift(_ % 3 == 0 && _ % 5 == 0)

    def isDivisibleBy3Or5: Pred[Int] =
      lift(_ % 3 == 0 || _ % 5 == 0)

    def main(args: Array[String]): Unit = {
      println("15 isDivisbleBy3And5? " + isDivisibleBy3And5(15))
      println("10 isDivisbleBy3Or5? " + isDivisibleBy3Or5(10))
    }
  }

  object exercise3 {
    def applyIf[A](a: A, f: A => A, p: A => Boolean): A =
      bind(a, p, (b: Boolean, a: A) => if (b) f(a) else a)

    def bind[A, B, C](a: A, p: A => B, f: (B, A) => C): C =
      f(p(a), a)
  }

  object exercise4 {
    import exercise3.bind

    def lift[A](op: (A, A) => Boolean): Pred[A] =
      a => bind(a, (a: A) => a, op)

    def isDivisibleBy3And5: Pred[Int] =
      lift(_ % 3 == 0 && _ % 5 == 0)

    def isDivisibleBy3Or5: Pred[Int] =
      lift(_ % 3 == 0 || _ % 5 == 0)

    def main(args: Array[String]): Unit = {
      println("15 isDivisbleBy3And5? " + isDivisibleBy3And5(15))
      println("10 isDivisbleBy3Or5? " + isDivisibleBy3Or5(10))
    }
  }
}
