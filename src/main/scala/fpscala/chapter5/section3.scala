package fpscala
package chapter5

object section3 {
  trait Stream[A] {
    def uncons: Option[(A, Stream[A])]

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons.
      map {case (h,t) => f(h, t.foldRight(z)(f))}.
      getOrElse(z)
  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None
      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }

  object exercise1 {
    def toList[A](s: Stream[A]): List[A] = {
      def helper(s: Stream[A], acc: List[A]): List[A] =
        s.uncons.map {
          case (h,t) => helper(t, acc :+ h)
        }.getOrElse(acc)

      helper(s, Nil)
    }

    def main(args: Array[String]): Unit = {
      val s = Stream(1,2,3,4,5)
      println("To a list " + toList(s))
    }
  }

  object exercise2 {
    import exercise1.toList

    def reverse[A](s: Stream[A]): Stream[A] = {
      def helper(s: Stream[A], acc: Stream[A]): Stream[A] =
        s.uncons.map {
          case (h,t) => helper(t, Stream.cons(h, acc))
        }.getOrElse(acc)

      helper(s, Stream.empty)
    }

    def take[A](s: Stream[A], n: Int): Stream[A] = {
      def helper(s: Stream[A], acc: Stream[A], n: Int): Stream[A] =
        if (n == 0) acc
        else s.uncons.map {
          case (h,t) => helper(t, Stream.cons(h, acc), n - 1)
        }.getOrElse(acc)

      reverse(helper(s, Stream.empty, n))
    }

    def main(args: Array[String]): Unit = {
      val s = Stream(1,2,3,4,5)
      println("Take 3 is " + toList(take(s, 3)))
    }
  }

  object exercise3 {
    import exercise1.toList
    import exercise2.reverse

    def takeWhile[A](s: Stream[A])(f: A => Boolean): Stream[A] = {
      def helper(s: Stream[A], acc: Stream[A]): Stream[A] =
        s.uncons.map { case(h,t) =>
          if (!f(h)) acc
          else helper(t, Stream.cons(h, acc))
        }.getOrElse(acc)

      reverse(helper(s, Stream.empty))
    }

    def main(args: Array[String]): Unit = {
      val s = Stream(1,2,3,4,5)
      println("Take while a < 5 is " + toList(takeWhile(s)(_ < 5)))
    }
  }

  object exercise4 {
    def forall[A](s: Stream[A])(f: A => Boolean): Boolean =
      s.foldRight(true)((h,t) => if (f(h)) t else false)

    def main(args: Array[String]): Unit = {
      val s = Stream(1,2,3,4,5)
      println("Forall a < 3 is " + forall(s)(_ < 3))
    }
  }

  object exercise5 {
    import exercise1.toList

    def takeWhile[A](s: Stream[A])(f: A => Boolean): Stream[A] = {
      val as: Stream[A] = Stream.empty
      s.foldRight(as) { (h,t) =>
        if (f(h)) Stream.cons(h, t)
        else as
      }
    }

    def main(args: Array[String]): Unit = {
      val s = Stream(1,2,3,4,5)
      println("Take while a < 5 is " + toList(takeWhile(s)(_ < 5)))
    }
  }

  object exercise6 {
    import exercise1.toList

    def map[A,B](s: Stream[A])(f: A => B): Stream[B] = {
      val empty: Stream[B] = Stream.empty
      s.foldRight(empty) { (h,t) =>
        Stream.cons(f(h), t)
      }
    }

    def filter[A](s: Stream[A])(f: A => Boolean): Stream[A] = {
      val empty: Stream[A] = Stream.empty
      s.foldRight(empty) { (h,t) =>
        if (f(h)) Stream.cons(h, t)
        else t
      }
    }

    def append[A](s: Stream[A])(t: Stream[A]): Stream[A] =
      s.foldRight(t) { (h,t) =>
        Stream.cons(h, t)
      }

    def flatMap[A,B](s: Stream[A])(f: A => Stream[B]): Stream[B] = {
      val empty: Stream[B] = Stream.empty
      s.foldRight(empty) { (h,t) =>
        append(f(h))(t)
      }
    }

    def main(args: Array[String]): Unit = {
      val s = Stream(1,2,3,4,5)
      val t = Stream(6,7,8,9,10)
      println("Map is " + toList(map(s)(_ + 1)))
      println("Filter is " + toList(filter(s)(_ % 2 == 0)))
      println("Map, filter is " + toList(filter(map(s)(_ + 1))(_ % 2 == 0)))
      println("Append s and t is " + toList(append(s)(t)))
      println("Flatmap is " + toList(flatMap(s)(x => Stream(x.toString + " hello"))))
    }
  }
}
