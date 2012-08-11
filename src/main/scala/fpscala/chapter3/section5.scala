package fpscala
package chapter3

object section5 {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object exercise1 {
    def size[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 1
        case Branch(l,r) => size(l) + size(r)
      }

    def main(args: Array[String]): Unit = {
      val tree = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
      println("The size is " + size(tree))
    }
  }

  object exercise2 {
    def maximum(t: Tree[Int]): Int = {
      def helper(t: Tree[Int], acc: Int): Int =
        t match {
          case Leaf(v) =>
            if (v > acc) v
            else acc
          case Branch(l,r) => {
            val u = helper(l, acc)
            val v = helper(r, acc)
            if (u > v) u
            else v
          }
        }
      helper(t, 0)
    }

    def main(args: Array[String]): Unit = {
      val tree = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
      println("The maximum is " + maximum(tree))
    }
  }

  object exercise3 {
    def depth[A](t: Tree[A]): Int = {
      def helper(t: Tree[A], acc: Int): Int =
        t match {
          case Leaf(_) => acc + 1
          case Branch(l,r) => {
            val u = helper(l, acc + 1)
            val v = helper(r, acc + 1)
            if (u > v) u
            else v
          }
        }
      helper(t, 0)
    }

    def main(args: Array[String]): Unit = {
      val tree = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
      println("The depth is " + depth(tree))
    }
  }

  object exercise4 {
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
      t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }

    def main(args: Array[String]): Unit = {
      val tree = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
      println("Mapping tree " + map(tree)(_ + 20))
    }
  }

  object exercise5 {
    // When a leaf is passed the left function is invoked, and when a
    // branch is passed the right function is invoked.
    def fold[A,B](t: Tree[A])(f: Leaf[A] => B)(g: Branch[A] => B): B =
      t match {
        case x @ Leaf(_) => f(x)
        case x @ Branch(_,_) => g(x)
      }

    def size[A](t: Tree[A]): Int =
      fold(t)(x => 1)(x => size(x.left) + size(x.right))

    def maximum(t: Tree[Int]): Int =
      fold(t)(_.value){ x =>
        val u = maximum(x.left)
        val v = maximum(x.right)
        if (u > v) u
        else v
      }

    def depth[A](t: Tree[A]): Int = {
      def helper(t: Tree[A], acc: Int): Int =
        fold(t)(x => acc + 1) { x =>
          val u = helper(x.left, acc + 1)
          val v = helper(x.right, acc + 1)
          if (u > v) u
          else v
        }

      helper(t, 0)
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
      fold[A, Tree[B]](t)(x => Leaf(f(x.value))) { x =>
        Branch(map(x.left)(f), map(x.right)(f))
      }

    def main(args: Array[String]): Unit = {
      val tree = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
      println("The size is " + size(tree))
      println("The maximum is " + maximum(tree))
      println("The depth is " + depth(tree))
      println("Mapping tree " + map(tree)(_ + 20))
    }
  }
}
