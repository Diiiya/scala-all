// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: ADPR
//
// AUTHOR1: Diyana Boyadzhieva
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: Fadi Dasus
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)

package adpro

import java.util.concurrent.{Executors,ExecutorService,CountDownLatch,TimeUnit,Callable}
import scala.language.implicitConversions
import scala.io.Source

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  trait Future[+A] {
    private[adpro] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new java.util.concurrent.atomic.AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  /** A non-strict version of `unit` */
  def delay[A](a: => A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
    def apply(k: A => Unit) = f(k)
  }

  def eval (es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  def map2[A,B,C] (p: Par[A], p2: Par[B]) (f: (A,B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(es)(cb(f(a,br.get)))
            else ar = Some(a)
          case Right(b) =>
            if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
            else br = Some(b)
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  // map is shown in the blocking part of the book (should still work but
  // probably uses one thread more than the version  below

  // def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
  //   map2 (pa,unit (())) ((a,_) => f(a))

  // This is the version of map2 specialized for nonblocking (Section 7.4.4
  // version)

  def map[A,B] (p: Par[A]) (f: A => B): Par[B] =
    es => new Future[B] {
      def apply (cb: B => Unit): Unit =
        p (es) ( a => eval (es) { cb (f (a)) } )
    }

  // Exercise 1
  //
  /* The argument a is passed by name (lazily) because we want dynamically and in parallel to compute the sum of the left and the right
     side of the list. If we pass a by value, that means the construction of the tree on the left side (let's say) will be 
     constructed and maybe even start executing before the tree of the right side has started and thus the two halves won't be
     executed in parallel which is the goal.
  */


  // Exercise 2 (CB7.4)
  def asyncF[A,B] (f: A => B) : A => Par[B] = a => lazyUnit(f(a))


  // Exercise 3
  //
  /* The goal of the function Par.Map is to combine the result of 2 parallel computations. Therefore, first, we should 
     have a test case that checks if the 2 computations are constructed/executed in parallel. As described in the book, 
     if both arguments to the function are passed strictly, that means that the first argument's tree will be constructed 
     fully (with recursion) before moving to the second argument which breaks our goal for parallelism. Therefore our test 
     case should check if the first argument (left half) is fully constructed before the second one (right half). We don't 
     want to prioritize neither the left or the right side.
     We should also have a test case that checks if the function combines the correct (final) counts of the left and the right
     halves. So that it doesn't return intermediate results.
     Another test case should check if we can choose to execute the task asynchronously or not. That is for cases when the
     input is so small that there's no need to start another thread. 
  */


  // Exercise 4 (CB7.5)
  def sequence[A] (ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))
  }

  // this is shown in the book:

  def parMap[A,B] (as: List[A]) (f: A => B): Par[List[B]] =
    sequence (as map (asyncF (f)))


  // Exercise 5
  /* First we define exec and how many threads we want to use -- in our case we have 3. Then For each uri in the uris Array,
    we should create a thread and call the function: scala.io.Source.fromURL(url).mkString that will download the HTML, encode
    it and return a string. For this to happen simultaneously, we use parMap which also returns a List of Strings. */
  def wget (uris: String*): List[String] = {
    val exec = Executors.newFixedThreadPool(3)
    run(exec)(parMap(uris.toList)(uri => scala.io.Source.fromURL(uri)("ISO-8859-1").mkString))
  }

  // Exercise 6 (CB7.6)
  def parFilter[A] (as: List[A]) (f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // works
    // map(parMap(as) (aVal => Some(aVal).filter(f)))(aVal => aVal.flatten) // also works
  }

  // shown in the book (adjusted for the non-blocking version)

   def equal[A] (e: ExecutorService) (p: Par[A], p2: Par[A]): Boolean =
     p(e) == p2(e)

  // Exercise 7 (CB7.11)
  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] = map2(n, sequence(choices))((x,l) => l(x))
    // es => {
    //   val ind = run(es)(n).get 
    //   run(es)(choices(ind))
    // } // github repo but doesnt work ??

  def choice[A] (n: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(n)(b => if (b) 0 else 1))(List(t, f))
  }


  // Exercise 8 (CB7.13)
  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] = es => choices(run(es)(pa))(es)
    // (es: ExecutorService) => {
    //   val k = run(es)(pa).get
    //   run(es)(choices(k))
    // } // github repo but doesnt work ??

  def choice2[A] (n: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] = chooser(n)(x => if (x) t else f) // not in tests

  def choiceN2[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] = chooser(n)(choices) // not in tests


  // Exercise 9 (CB7.14)
  def join[A] (a : Par[Par[A]]) :Par[A] = chooser (a) (a => a)


  // Exercise 10
  implicit class parExtensions[A](val pa: Par[A]) extends AnyVal {
    def map[B] (f: A => B): Par[B] = Par.map (pa) (f)
    def map2[B,C] (pb: Par[B]) (f: (A,B) => C): Par[C] = Par.map2 (pa, pb) (f)
    def chooser[B] (choices: A => Par[B]): Par[B] = Par.chooser (pa) (choices)
  }

}
