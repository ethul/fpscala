package fpscala
package chapter6

import scala.annotation.tailrec

object section2 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
      }
    }
  }

  object exercise1 {
    def positiveInt(rng: RNG): (Int, RNG) = {
      val (x, rng2) = rng.nextInt
      if (x == Int.MinValue) (Int.MaxValue, rng2)
      else (x.abs, rng2)
    }

    def main(args: Array[String]): Unit = {
      println("Random positive int " +
        positiveInt(RNG.simple(System.currentTimeMillis)))
    }
  }

  object exercise2 {
    import exercise1.positiveInt

    def nextDouble(rng: RNG): (Double, RNG) = {
      val (x, rng2) = positiveInt(rng)
      if (x == Int.MaxValue) (0.0, rng2)
      else (x.toDouble / Int.MaxValue.toDouble, rng2)
    }

    def main(args: Array[String]): Unit = {
      println("Random [0,1) double " +
        nextDouble(RNG.simple(System.currentTimeMillis)))
    }
  }

  object exercise3 {
    import exercise1.positiveInt
    import exercise2.nextDouble

    def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (x, rng2) = positiveInt(rng)
      val (y, rng3) = nextDouble(rng2)
      ((x, y), rng3)
    }

    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
      val (x, rng2) = nextDouble(rng)
      val (y, rng3) = positiveInt(rng2)
      ((x, y), rng3)
    }

    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
      val (x, rng2) = nextDouble(rng)
      val (y, rng3) = nextDouble(rng2)
      val (z, rng4) = nextDouble(rng3)
      ((x, y, z), rng4)
    }

    def main(args: Array[String]): Unit = {
      println("int and double " +
        intDouble(RNG.simple(System.currentTimeMillis)))
      println("double and int " +
        doubleInt(RNG.simple(System.currentTimeMillis)))
      println("double three " +
        double3(RNG.simple(System.currentTimeMillis)))
    }
  }

  object exercise4 {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def run(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
        if (count == 0) (acc, rng)
        else {
          val (x,r) = rng.nextInt
          run(count - 1, r, x :: acc)
        }
      }
      run(count, rng, Nil)
    }

    def main(args: Array[String]): Unit = {
      println("array of ints " +
        ints(10)(RNG.simple(System.currentTimeMillis)))
    }
  }
}
