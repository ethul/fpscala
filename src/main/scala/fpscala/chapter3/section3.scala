package fpscala
package chapter3

object section3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  object exercise1 {
    // The tail function could return Option[List[A]] and in the case of
    // Nil return None, otherwise return Some(xs)
    def tail[A](as: List[A]): List[A] =
      as match {
        case Nil => Nil
        case Cons(x,xs) => xs
      }

    def main(args: Array[String]): Unit = {
      val list = Cons(1, Cons(2, Cons(3, Nil)))
      println("The tail of List(1,2,3) is " + tail(list))
    }
  }

  object exercise2 {
    def drop[A](l: List[A])(n: Int): List[A] =
      l match {
        case Nil => Nil
        case Cons(x,xs) =>
          if (n == 0) l
          else drop(xs)(n - 1)
      }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
      l match {
        case Nil => Nil
        case Cons(x,xs) =>
          if (!f(x)) l
          else dropWhile(xs)(f)
      }

    def main(args: Array[String]): Unit = {
      val list = Cons(1, Cons(2, Cons(3, Nil)))
      println("Drop 2 of " + list + " is " + drop(list)(2))
      println("Drop of " + list + " while < 3 is " + dropWhile(list)(_ < 3))
    }
  }

  object exercise3 {
    def setHead[A](as: List[A])(a: A): List[A] =
      as match {
        case Nil => Nil
        case Cons(_,xs) => Cons(a, xs)
      }

    def main(args: Array[String]): Unit = {
      val list = Cons(1, Cons(2, Cons(3, Nil)))
      println("Setting the head of " + list + " with 20 is " + setHead(list)(20))
    }
  }

  object exercise4 {
    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    def init[A](l: List[A]): List[A] = {
      def helper(l: List[A], acc: List[A]): List[A] =
        l match {
          case Nil => acc
          case Cons(x,Nil) => acc
          case Cons(x,xs) => helper(xs, append(acc, Cons(x, Nil)))
        }
      helper(l, Nil)
    }

    def main(args: Array[String]): Unit = {
      val list = Cons(1, Cons(2, Cons(3, Nil)))
      println("Init of " + list + " is " + init(list))
    }
  }
}
