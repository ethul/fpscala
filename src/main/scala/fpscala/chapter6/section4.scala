package fpscala
package chapter6

object section4 {
  import section3.exercise11.State

  object exercise12 {
    def getState[S]: State[S,S] =
      State(s => (s, s))

    def setState[S](s: S): State[S,Unit] =
      State(s0 => (Unit, s))

    def main(args: Array[String]): Unit = {
      var res: State[Int,Int] =
        for {
          s <- getState
          _ <- setState(s + 1)
        } yield 10

      println("res = " + res.run(3))
    }
  }

  object exercise13 {
    import exercise12.{getState,setState}

    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input
    case class Machine(locked: Boolean, candies: Int, coins: Int)

    def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
      def transition(i: Input, m: Machine): Machine =
        (i, m) match {
          case (_, Machine(_,0,_)) => m
          case (Coin, Machine(true,_,a)) => m.copy(locked = false, coins = a + 1)
          case (Turn, Machine(true,_,a)) => m
          case (Coin, Machine(false,_,a)) => m.copy(coins = a + 1)
          case (Turn, Machine(false,a,_)) => m.copy(locked = true, candies = a - 1)
        }

      def run(is: List[Input], m: Machine): Machine =
        is match {
          case Nil => m
          case x :: xs => run(xs, transition(x, m))
        }

      for {
        s0 <- getState
        _  <- setState(run(inputs, s0))
        s1 <- getState
      } yield s1.coins
    }

    def main(args: Array[String]): Unit = {
      val inputs = List(Coin, Turn, Coin, Turn, Turn, Coin, Coin, Coin, Turn)
      println("simulation of machine: " + inputs + " is " +
        simulateMachine(inputs).run(Machine(true, 3, 0)))
    }
  }
}
