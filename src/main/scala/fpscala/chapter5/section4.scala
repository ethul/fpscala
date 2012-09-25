package fpscala
package chapter5

object section4 {
  import section3.Stream
  import section3.exercise2.take
  import section3.exercise1.toList

  object exercise7 {
    def constant[A](a: A): Stream[A] =
      Stream.cons(a, constant(a))

    def main(args: Array[String]): Unit = {
      println("Infinite stream of 1s " + toList(take(constant(1), 10)))
    }
  }

  object exercise8 {
    def from(n: Int): Stream[Int] =
      Stream.cons(n, from(n + 1))

    def main(args: Array[String]): Unit = {
      println("Incremental stream 1 " + toList(take(from(1), 10)))
    }
  }

  object exercise9 {
    def fibs: Stream[Int] = {
      def helper(a: Int, b: Int): Stream[Int] =
        Stream.cons(a, helper(b, a + b))
      helper(0, 1)
    }

    def main(args: Array[String]): Unit = {
      println("Fibs stream " + toList(take(fibs, 10)))
    }
  }

  object exercise10 {
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
      f(z).map {
        case (a,s) => Stream.cons(a, unfold(s)(f))
      }.getOrElse(Stream.empty[A])
  }

  object exercise11 {
    import exercise10.unfold

    def constant[A](a: A): Stream[A] =
      unfold(a)(s => Some((s, s)))

    def from(n: Int): Stream[Int] =
      unfold(n)(s => Some((s, s + 1)))

    def fibs: Stream[Int] =
      unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

    def ones: Stream[Int] =
      constant(1)

    def main(args: Array[String]): Unit = {
      println("Infinite stream of 1s " + toList(take(constant(1), 10)))
      println("Incremental stream 1 " + toList(take(from(1), 10)))
      println("Fibs stream " + toList(take(fibs, 10)))
      println("Ones stream " + toList(take(ones, 10)))
    }
  }

  object exercise12 {
    import exercise10.unfold
    import exercise11.{constant,from}

    def map[A, B](s: Stream[A])(f: A => B): Stream[B] =
      unfold(s) {s => 
        s.uncons.map {
          case (h,ts) => (f(h), ts)
        }
      }

    def take[A](s: Stream[A], n: Int): Stream[A] =
      unfold((s,n)) {s =>
        s match {
          case (_,0) => None
          case (s,n) => s.uncons.map {
            case (h,ts) => (h, (ts, n - 1))
          }
        }
      }

    def takeWhile[A](s: Stream[A])(f: A => Boolean): Stream[A] =
      unfold(s) {s =>
        s.uncons.flatMap {
          case (h,ts) =>
            if (f(h)) Some((h, ts))
            else None
        }
      }

    def zip[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((a,b)) {s =>
        for {
          a <- s._1.uncons
          b <- s._2.uncons
        } yield {
          (f(a._1, b._1), (a._2, b._2))
        }
      }

    def zipAll[A, B, C](as: Stream[A], bs: Stream[B], a: A, b: B)(f: (A, B) => C): Stream[C] =
      unfold((as,bs)) {s =>
        (s._1.uncons, s._2.uncons) match {
          case (None,None) => None
          case (Some((a,as)),None) => Some((f(a, b), (as, Stream.empty[B])))
          case (None,Some((b,bs))) => Some((f(a, b), (Stream.empty[A], bs)))
          case (Some((a,as)),Some((b,bs))) => Some((f(a, b), (as, bs)))
        }
      }

    def main(args: Array[String]): Unit = {
      println("Map constant stream " +
        toList(take(map(constant(1))(_ + 1), 10)))

      println("Take while a < 5 " +
        toList(takeWhile(from(1))(_ < 5)))

      println("Zip as and bs " +
        toList(zip(take(constant(3), 10), take(from(1), 5))(_ * _)))

      println("ZipAll as and bs " +
        toList(zipAll(take(constant(3), 10), take(from(1), 5), 3, 10)(_ * _)))
    }
  }

  object exercise13 {
    import section3.exercise4.forall
    import exercise12.zip

    // What happens if the stream t is longer than s? E.g., Stream(1,2)
    // startsWith Stream(1,2,3).
    def startsWith[A](s: Stream[A], t: Stream[A]): Boolean =
      forall(zip(s, t)(_ == _))(_ == true)

    def main(args: Array[String]): Unit = {
      println("(1,2,3) starts with (1,2) " +
        startsWith(Stream(1,2,3), Stream(1,2)))

      println("(1,2,3) starts with (1,4) " +
        startsWith(Stream(1,2,3), Stream(1,4)))
    }
  }

  object exercise14 {
    import exercise10.unfold
    import exercise13.startsWith

    def tails[A](s: Stream[A]): Stream[Stream[A]] =
      unfold(s) {s =>
        s.uncons.map {
          case (h,ts) => (s, ts)
        }
      }

    def exists[A](s: Stream[A])(p: A => Boolean): Boolean =
      s.foldRight(false)((a, b) => p(a) || b)

    def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
      exists(tails(s1))(startsWith(_, s2))

    def main(args: Array[String]): Unit = {
      println("tails of (1,2,3) " +
        toList(tails(Stream(1,2,3))).map(toList(_)))

      println("(1,2,3,4,5,6,7) has subsequence (3,4,5) " +
        hasSubsequence(Stream(1,2,3,4,5,6,7), Stream(3,4,5)))
    }
  }

  object exercise15 {
    // With some help from
    // http://hackage.haskell.org/packages/archive/base/latest/doc/html/src/GHC-List.html#scanr
    def scanRight[A, B](s: Stream[A], z: => B)(f: (A, => B) => B): Stream[B] =
      s.uncons.flatMap {
        case (x, xs) => {
          val t = scanRight(xs, z)(f)
          t.uncons.map {
            case (y, ys) => Stream.cons(f(x, y), t)
          }
        }
      }.getOrElse(Stream(z))

    def main(args: Array[String]): Unit = {
      println("scan right of (1,2,3) " +
        toList(scanRight(Stream(1,2,3), 0)(_ + _)))
    }
  }
}
