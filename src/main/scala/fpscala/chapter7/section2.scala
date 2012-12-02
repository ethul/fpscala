package fpscala
package chapter7

object section2 {
  object exercise1 {
    // def map2[A,B,C](ap: Par[A], bp: Par[B])(f: (A,B) => C): Par[C]
  }

  object exercise2 {
    import java.util.concurrent.{ExecutorService,Future}

    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  }

  object exercise3 {
    import exercise2.{Par,run}
    import java.util.concurrent.{Callable,ExecutorService,TimeUnit}
    import java.util.concurrent.{Future => JFuture}

    def unit[A](a: A): Par[A] =
      s => new JFuture[A] {
        def cancel(a: Boolean) = false
        def get = a
        def get(t: Long, u: TimeUnit) = get
        def isCancelled = false
        def isDone = true
      }

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
      s => unit(f(run(s)(a).get, run(s)(b).get))(s)
      
    def fork[A](a: => Par[A]): Par[A] =
      s => s.submit(new Callable[A] {
        def call: A = run(s)(a).get
      })

    def async[A](a: => A): Par[A] =
      fork(unit(a))

    object timeout {
      import TimeUnit.NANOSECONDS

      def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
        s => new JFuture[C] {
          val af = run(s)(a)
          val bf = run(s)(b)
          def cancel(a: Boolean) = af.cancel(a) || bf.cancel(a)
          def get = f(af.get, bf.get)
          def get(t: Long, u: TimeUnit) = {
            val start = System.nanoTime
            val a = af.get(t, u)
            val end = System.nanoTime
            val b = bf.get(t - u.convert(start - end, NANOSECONDS), u)
            f(a, b)
          }
          def isCancelled = af.isCancelled && bf.isCancelled
          def isDone = af.isDone && bf.isDone
        }
    }
  }

  object exercise4 {
    import exercise2.{Par,run}
    import exercise3.{fork,unit}

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => fork(unit(f(a)))
  }

  object exercise5 {
    import exercise2.{Par,run}
    import exercise3.unit

    def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)] =
      s => unit((run(s)(fa).get, run(s)(fb).get))(s)

    def map[A,B](fa: Par[A])(f: A => B): Par[B] =
      s => unit(f(run(s)(fa).get))(s)

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
      map(product(a, b))(ab => f(ab._1, ab._2))
  }

  object exercise6 {
    import java.util.concurrent.{Future => JFuture, TimeUnit}
    import exercise2.{Par,run}

    def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = {
      s => {
        val fs = l.map { a =>
          new JFuture[B] {
            def cancel(a: Boolean) = false
            def get = f(a)
            def get(t: Long, u: TimeUnit) = get
            def isCancelled = false
            def isDone = true
          }
        }
        new JFuture[List[B]] {
          def cancel(a: Boolean) = false
          def get = fs.map(a => a.get)
          def get(t: Long, u: TimeUnit) = get
          def isCancelled = false
          def isDone = true
        }
      }
    }
  }

  object exercise7 {
    import exercise2.{Par, run}
    import exercise3.{fork, unit}
    import exercise4.asyncF
    import exercise5.map2

    def sequence[A](l: List[Par[A]]): Par[List[A]] =
      l.foldLeft(unit[List[A]](Nil)) { (fas, fa) =>
        map2(fas, fa)((as, a) => a :: as)
      }

    def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = l.map(asyncF(f))
      sequence(fbs)
    }
  }

  object exercise8 {
    import exercise2.{Par, run}
    import exercise3.{fork, unit}
    import exercise4.asyncF
    import exercise5.map
    import exercise7.sequence

    /*
    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] =
      l.foldLeft(unit[List[A]](Nil)) { (fas, a) =>
        map(fas)(as => if (f(a)) a :: as else as)
      }
    */

    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
      val fbs: List[Par[List[A]]] = l.map(asyncF(a => if (f(a)) List(a) else Nil))
      val res: Par[List[List[A]]] = sequence(fbs)
      map(res)(_.flatten)
    }
  }

  object ideas {
    import exercise2.Par
    import exercise3.unit
    import exercise4.asyncF
    import exercise5.{map,map2}
    import exercise7.sequence

    def wordCount(ps: List[String]): Par[Int] =
      generalWordCount(ps)(_.split(' ').size)(_.sum)

    def generalWordCount[A,B](as: List[A])(f: A => B)(g: List[B] => B): Par[B] =
      map(sequence(as.map(asyncF(f))))(g)

    def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] =
      map2(a, map2(b, c)((b,c) => (b,c))) { (a,bc) =>
        val (b,c) = bc
        f(a,b,c)
      }

    def map4[A,B,C,D,E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A,B,C,D) => E): Par[E] =
      map2(a, map2(b, map2(c, d)((c,d) => (c,d)))((b,cd) => (b,cd))) { (a, bcd) =>
        val (b,(c,d)) = bcd
        f(a,b,c,d)
      }

    def map5[A,B,C,D,E,F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A,B,C,D,E) => F): Par[F] =
      map2(a, map2(b, map2(c, map2(d, e)((d,e) => (d,e)))((c,de) => (c,de)))((b,cde) => (b,cde))) { (a, bcde) =>
        val (b,(c,(d,e))) = bcde
        f(a,b,c,d,e)
      }
  }

  object exercise9 {
    // TODO
  }

  object exercise10 {
    // Would the single thread executor work?
  }

  object exercise11 {
    // Show that given a fixed thread pool size of N, the fork
    // implementaton will deadlock.
    //
    // We know that a single call to fork given a Par[A] requires 2
    // threads for computation (as seen in the text). Assume that we
    // nest calls to for N times where N is the fixed thread pool size;
    // e.g., if N = 2 then we have fork(fork(parA)).
    //
    // Generally, we would have fork_1(fork_2(...fork_n(parA)...)),
    // which we can rewrite as:
    //
    //  def fork[A](a: => Par[A]): Par[A] =
    //    s => s.submit(new Callable[A] {
    //      def call: A = run(s) {
    //        s => s.submit(new Callable[A] {
    //          def call: A = run(s) {
    //            ...
    //            s => s.submit(new Callable[A] {
    //              def call: A = run(s) {
    //                run(s) {
    //                  a
    //                }.get
    //              })
    //            }.get
    //            ...
    //          })
    //        }.get
    //      })
    //    }.get
    //  })
    //
    // Each time we nest the fork call, we require one thread of
    // computation since we submit a new Callable to the same thread
    // pool before the outer callable completes since each call is
    // blocked by the get invocation. At the innermost fork call we
    // require 2 threads for computation.
    //
    // Therefore, if we nest fork calls N times we require N + 1 threads
    // to complete the computation and thus will result in deadlock for
    // any given fixed threadpool size N.

    import exercise2.Par

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)
  }

  // TODO
  object exercise12 {
    import java.util.concurrent.{Callable, Executors, Future => JFuture}
    import java.util.concurrent.TimeUnit
    import exercise2.{Par, run}
    import exercise3.{async, fork => deadLockFork}

    def fork[A](a: => Par[A]): Par[A] =
      s => {
        val x = new JFuture[A] {
          def cancel(a: Boolean) = false
          def get = a(s).get
          def get(t: Long, u: TimeUnit) = get
          def isCancelled = false
          def isDone = true
        }
        s.submit(new Callable[A] {
          def call: A = run(s)(a).get
        })
      }

    def main(args: Array[String]): Unit = {
      val a = async(42 + 1)
      val s = Executors.newFixedThreadPool(1)
      val t = Executors.newFixedThreadPool(2)
      println(deadLockFork(a)(t).get)
    }
  }

  // TODO
  object exercise13 {
  }

  object exercise14 {
    import exercise2.Par

    // We need something like flatMap
    def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      s => if (a(s).get) ifTrue(s) else ifFalse(s)
  }

  object exercise15 {
    import exercise2.Par
    import exercise5.map

    def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =
      s => choices(a(s).get)(s)

    def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(x => if (x) 0 else 1))(List(ifTrue, ifFalse))
  }

  object exercise16 {
    import exercise2.Par
    
    def choiceMap[A,B](a: Par[A])(choices: Map[A,Par[B]]): Par[B] =
      s => choices(a(s).get)(s)

    // Attempt to generalize
    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      s => f(a(s).get)(s)
  }

  object exercise17 {
    import exercise2.Par

    def chooser[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      s => f(a(s).get)(s)

    def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(a)(choices(_))

    def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      chooser(a)(if (_) ifTrue else ifFalse)
  }

  object exercise18 {
    import exercise2.Par
    import exercise5.map

    def join[A](a: Par[Par[A]]): Par[A] =
      s => a(s).get()(s)

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      join(map(a)(f))

    // TODO
    object withoutDeadlock {
    }

    object alt {
      def join[A](a: Par[Par[A]]): Par[A] =
        exercise16.flatMap(a)((x: Par[A]) => x)
    }
  }
}
