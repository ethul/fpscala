package fpscala
package chapter3

object section4 {
  import section3.{List,Nil,Cons}

  def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B =
    list match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  object exercise1 {
    // foldRight (Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
    // f(1, f(2, f(3, 0)))
    // f(1, f(2, 3))
    // f(1, 5)
    // 6
  }

  object exercise2 {
    // foldRight (Cons(1.0, Cons(0.0, Cons(3.0, Nil))), 1.0) { (a,b) =>
    //   if (a == 0.0 || b == 0.0) 0.0
    //   else a * b
    // }
    // f(1.0, f(0.0, f(3.0, 1.0)))
    // f(1.0, f(0.0, 3.0))
    // f(1.0, 0.0)
    // 0.0
    // I don't believe we can escape the recursion, just avoid the
    // multiplication operation in the case that a or b are 0.0,
    // because we don't control the recursion, this occurs outside of
    // our function.
  }

  object exercise3 {
    // Maybe that the data constructors can be used as an identity for
    // folding right over a list.
    def main(args: Array[String]): Unit = {
      val res = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
      println("Folding right passing nil and cons " + res)
    }
  }

  object exercise4 {
    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_,x) => x + 1)

    def main(args: Array[String]): Unit =
      println("The length of List(2,3,1,4) is " + length(List(2,3,1,4)))
  }

  object exercise5 {
    import annotation.tailrec

    def foldLeft[A,B](list: List[A], z: B)(f: (B, A) => B): B = {
      @tailrec
      def helper(as: List[A], acc: B): B =
        as match {
          case Nil => acc
          case Cons(x,xs) => helper(xs, f(acc, x))
        }

      helper(list, z)
    }
    // foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
    // helper(Cons(1, Cons(2, Cons(3, Nil))), 0)
    // helper(Cons(2, Cons(3, Nil)), 1)
    // helper(Cons(3, Nil), 3)
    // helper(Nil, 6)
    // 6

    def main(args: Array[String]): Unit = {
      val large = List((1 to 5000): _*)
      val double = section3.exercise4.append(large, large)
      try {exercise4.length(double)}
      catch {case e => println(e)}
    }
  }

  object exercise6 {
    import exercise5.foldLeft

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((x,_) => x + 1)

    def main(args: Array[String]): Unit = {
      val large = List((1 to 5000): _*)
      val double = section3.exercise4.append(large, large)
      try {println("Length of the list is " + length(double))}
      catch {case e => println(e)}
    }
  }

  object exercise7 {
    import section3.exercise4.append
    import exercise5.foldLeft

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A]) { (b, x) =>
        Cons(x, b) 
      }

    def main(args: Array[String]): Unit = {
      val list = List(1,2,3,4)
      println("The reverse of " + list + " is " + reverse(list))
    }
  }

  object exercise8 {
    import exercise7.reverse

    def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B =
      exercise5.foldLeft(reverse(list), z) { (b, a) =>
        f(a, b)
      }

    def foldLeft[A,B](list: List[A], z: B)(f: (B, A) => B): B =
      section4.foldRight(reverse(list), z) { (a, b) =>
        f(b, a)
      }

    def main(args: Array[String]): Unit = {
      val list = List(1,2,3)
      println("The foldRight of " + list + " is " + foldRight(list, 0)(_ + _))
      println("The foldLeft of " + list + " is " + foldLeft(list, 0)(_ + _))
    }
  }

  object exercise9 {
    import exercise5.foldLeft
    import exercise7.reverse

    def append[A](a1: List[A], a2: List[A]): List[A] =
      foldLeft(reverse(a1), a2) { (b, x) =>
        Cons(x, b)
      }

    def main(args: Array[String]): Unit = {
      val list1 = List(1,2,3,4)
      val list2 = List(5,6,7,8)
      println("Appending lists is " + append(list1, list2))
    }
  }

  object exercise10 {
    import exercise5.foldLeft
    import exercise7.reverse
    import exercise9.append

    def flatten[A](aas: List[List[A]]): List[A] =
      foldLeft(reverse(aas), Nil: List[A]) { (b, x) =>
        append(x, b)
      }

    def main(args: Array[String]): Unit = {
      val lls = List(List(1,2,3), List(4,5,6), List(7,8,9))
      println("Flattening a list of lists is " + flatten(lls))
    }
  }

  object exercise11 {
    import chapter2.section6.Pred
    import section3.exercise2.dropWhile
    import exercise5.foldLeft
    import exercise6.length
    import exercise7.reverse
    import exercise10.flatten

    def addOne(as: List[Int]): List[Int] =
      map(as)(_ + 1)

    def doublesToStrings(as: List[Double]): List[String] =
      map(as)(_.toString())

    def map[A,B](as: List[A])(f: A => B): List[B] =
      foldLeft(reverse(as), Nil: List[B]) { (b, a) =>
        Cons(f(a), b)
      }

    def filter[A](as: List[A])(p: Pred[A]): List[A] =
      flatMap(as) { a =>
        if (!p(a)) Nil
        else Cons(a, Nil)
      }

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
      flatten(foldLeft(reverse(as), Nil: List[List[B]]) { (b, a) =>
        Cons(f(a), b)
      })

    def zipAdd(as: List[Int], bs: List[Int]): List[Int] =
      zip(as, bs)(_ + _)

    def zip[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
      def helper(as: List[A], bs: List[B], acc: List[C]): List[C] =
        as match {
          case Nil => acc
          case Cons(x,xs) => bs match {
            case Nil => acc
            case Cons(y,ys) => helper(xs, ys, Cons(f(x, y), acc))
          }
        }

      reverse(helper(as, bs, Nil))
    }

    def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
      val res =
        sub match {
          case Nil => Nil
          case Cons(x,xs) => reverse(sub) match {
            case Nil => Nil
            case Cons(y,ys) =>
              reverse(dropWhile(reverse(dropWhile(as)(_ != x)))(_ != y))
          }
        }

      length(filter(zip(res, sub)(_ == _))(_ == true)) == length(sub)
    }

    def main(args: Array[String]): Unit = {
      val ints = List(1,2,3,4,5,6,7)
      val doubles = List(1.0,2.0,3.0)
      println("Adding one to a list is " + addOne(ints))
      println("Doubles to strings is " + doublesToStrings(doubles))
      println("Filtering odds from a list is " + filter(ints)(_ % 2 == 0))
      println("Zip add of lists is " + zipAdd(ints, ints))
      println("Has subsequence (1,2) is " + hasSubsequence(ints, List(1,2)))
      println("Has subsequence (2,3) is " + hasSubsequence(ints, List(2,3)))
      println("Has subsequence (4) is " + hasSubsequence(ints, List(4)))
      println("Has subsequence (4,7,6) is " + hasSubsequence(ints, List(4,7,6)))
    }
  }
}
