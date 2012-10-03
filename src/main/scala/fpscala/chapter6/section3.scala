package fpscala
package chapter6

import scala.annotation.tailrec

object section3 {
  import section2.RNG

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  object exercise5 {
    import section2.exercise1.positiveInt

    def positiveMax(n: Int): Rand[Int] =
      map(positiveInt _)(a => if (a > n) n else a)

    def main(args: Array[String]): Unit = {
      println("positive max of 1000000000 " +
        positiveMax(1000000000)(RNG.simple(System.currentTimeMillis)))
    }
  }

  object exercise6 {
    import section2.exercise1.positiveInt

    def nextDouble: Rand[Double] =
      map(positiveInt _) {a =>
        if (a == Int.MaxValue) 0.0
        else a.toDouble / Int.MaxValue.toDouble
      }

    def main(args: Array[String]): Unit = {
      println("Random [0,1) double " +
        nextDouble(RNG.simple(System.currentTimeMillis)))
    }
  }

  object exercise7 {
    import exercise5.positiveMax
    import exercise6.nextDouble

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rngA) = ra(rng)
        val (b, rngB) = rb(rngA)
        (f(a, b), rngB)
      }

    def intDouble: Rand[(Int,Double)] =
      map2(positiveMax(Int.MaxValue), nextDouble)((_, _))

    def doubleInt: Rand[(Double,Int)] =
      map2(nextDouble, positiveMax(Int.MaxValue))((_, _))

    def main(args: Array[String]): Unit = {
      println("int and double " +
        intDouble(RNG.simple(System.currentTimeMillis)))
      println("double and int " +
        doubleInt(RNG.simple(System.currentTimeMillis)))
    }
  }

  object exercise8 {
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      rng => {
        def run(fs: List[Rand[A]], acc: List[A], rngNth: RNG): (List[A], RNG) = {
          fs match {
            case Nil => (acc, rngNth)
            case x :: xs => {
              val (a,r) = x(rngNth)
              run(xs, a :: acc, r)
            }
          }
        }

        run(fs, Nil, rng)
      }

    def ints(count: Int): Rand[List[Int]] = 
      sequence(List.fill(count)((rng: RNG) => rng.nextInt))

    def main(args: Array[String]): Unit = {
      println("array of ints " +
        ints(10)(RNG.simple(System.currentTimeMillis)))
    }
  }

  object exercise9 {
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
      }

    def positiveInt: Rand[Int] =
      flatMap(rng => rng.nextInt) {a => 
        if (a != Int.MinValue) rng => (a.abs, rng)
        else positiveInt
      }

    def main(args: Array[String]): Unit = {
      println("positive int " +
        positiveInt(RNG.simple(System.currentTimeMillis)))
    }
  }

  object exercise10 {
    import exercise9.flatMap

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => rng => (f(a), rng))

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra) {a =>
        flatMap(rb) {b =>
          rng => (f(a, b), rng)
        }
      }
  }

  object exercise11 {
    case class State[S,+A](run: S => (A,S)) {
      def flatMap[B](f: A => State[S,B]): State[S,B] =
        State(s1 => {
          val (a, s2) = run(s1)
          f(a).run(s2)
        })

      def map[B](f: A => B): State[S,B] =
        flatMap(a => State(s => (f(a), s)))

    }

    trait States {
      def unit[S,A](a: A): State[S,A] =
        State(s => (a, s))

      def map2[S,A,B,C](sa: State[S,A], sb: State[S,B])(f: (A, B) => C): State[S,C] =
        sa.flatMap {a =>
          sb.flatMap {b =>
            State(s => (f(a, b), s))
          }
        }
    }

    object State extends States
  }
}
