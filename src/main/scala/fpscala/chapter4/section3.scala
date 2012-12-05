package fpscala
package chapter4

object section3 {
  object exercise1 {
    trait Option[+A] {
      def map[B](f: A => B): Option[B] =
        this match {
          case Some(a) => Some(f(a))
          case None => None
        }

      def getOrElse[B >: A](default: => B): B =
        this match {
          case Some(a) => a
          case None => default
        }

      def flatMap[B](f: A => Option[B]): Option[B] =
        this.map(f).getOrElse(None)

      def orElse[B >: A](ob: Option[B]): Option[B] =
        this.map(a => Some(a)).getOrElse(ob)

      def filter(f: A => Boolean): Option[A] =
        this.flatMap(a => if (f(a)) Some(a) else None)
    }

    case class Some[+A](get: A) extends Option[A]
    case object None extends Option[Nothing]
  }

  object exercise2 {
    import exercise1.{Option, Some, None}

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      for {
        m <- mean(xs)
        v <- mean(xs.map(x => math.pow(x - m, 2)))
      } yield v
  }

  object exercise3 {
    import exercise1.{Option, Some, None}
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for {
        x <- a
        y <- b
      } yield f(x, y)
  }

  object exercise4 {
    import java.util.regex._
    import exercise1.{Option, Some, None}
    import exercise3.map2

    def pattern(s: String): Option[Pattern] =
      try {
        Some(Pattern.compile(s))
      } catch {
        case e: PatternSyntaxException => None
      }

    def mkMatcher(pat: String): Option[String => Boolean] =
        pattern(pat) map (p => (s: String) => p.matcher(s).matches)

    def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
      map2(mkMatcher(pat1), mkMatcher(pat2))((a, b) => a(s) && b(s))
  }

  object exercise5 {
    import exercise1.{Option, Some, None}

    def sequence[A](as: List[Option[A]]): Option[List[A]] = {
      def go(as: List[Option[A]], acc: List[A]): Option[List[A]] =
        as match {
          case Nil => Some(acc.reverse)
          case x :: xs =>
            x match {
              case None => None
              case Some(a) => go(xs, a :: acc)
            }
        }
      go(as, Nil)
    }
  }

  object exercise6 {
    import exercise1.{Option, Some, None}

    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
      as match {
        case Nil => Some(Nil)
        case a :: as =>
          for {
            b  <- f(a)
            bs <- traverse(as)(f)
          } yield b :: bs
      }

    def sequence[A](as: List[Option[A]]): Option[List[A]] =
      traverse(as)(a => a)
  }

  object exercise7 {
    trait Either[+E, +A] {
      def map[B](f: A => B): Either[E, B] =
        this match {
          case Left(e) => Left(e)
          case Right(a) => Right(f(a))
        }

      def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
        this match {
          case Left(e) => Left(e)
          case Right(a) => f(a)
        }

      def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] =
        this match {
          case Left(_) => b
          case Right(a) => Right(a)
        }

      def map2[EE >: E, B, C](fb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
        for {
          a <- this
          b <- fb
        } yield f(a, b)
    }
    case class Left[+E](value: E) extends Either[E, Nothing]
    case class Right[+A](value: A) extends Either[Nothing, A]
  }

  object exercise8 {
    import exercise7.{Either, Left, Right}

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as match {
        case Nil => Right(Nil)
        case a :: as =>
          for {
            b <- f(a)
            bs <- traverse(as)(f)
          } yield b :: bs
      }

    def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
      traverse(as)(a => a)
  }

  object exercise9 {
    type Monoid[+A] = List[A]

    def append[A](as: Monoid[A], bs: Monoid[A]): Monoid[A] =
      as ++ bs

    trait EitherAcc[+E, +A] {
      def map[B](f: A => B): EitherAcc[E, B] =
        this match {
          case LeftAcc(e) => LeftAcc(e)
          case RightAcc(a) => RightAcc(f(a))
        }

      def map2[EE >: E, B, C](fb: EitherAcc[EE, B])(f: (A, B) => C): EitherAcc[EE, C] =
        this match {
          case LeftAcc(e1) =>
            fb match {
              case LeftAcc(e2) => LeftAcc(append(e1, e2))
              case RightAcc(_) => LeftAcc(e1)
            }
          case RightAcc(a) =>
            fb match {
              case LeftAcc(e2) => LeftAcc(e2)
              case RightAcc(b) => RightAcc(f(a, b))
            }
        }
    }
    case class LeftAcc[+E](value: Monoid[E]) extends EitherAcc[E, Nothing]
    case class RightAcc[+A](value: A) extends EitherAcc[Nothing, A]

    // traverse and sequence would take all of the LeftAcc values and
    // append them to become a single LeftAcc containing the resulting value.

    // orElse should concatenate the two LeftAcc values when both are
    // LeftAccs and result in a LeftAcc containing the concatenated value.
  }
}
