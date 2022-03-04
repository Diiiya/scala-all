// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen

package adpro
import java.util.stream
sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons (h, t) => Some(h ())
  }

  def tail: Stream[A] = this match {
    case Empty => Empty
    case Cons (h, t) => t ()
  }

  def foldRight[B] (z : =>B) (f: (A, =>B) => B): B = this match {
    case Empty => z
    case Cons (h, t) => f (h (), t ().foldRight (z) (f))
    // Note 1. f can return without forcing the tail
    // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
    // if f requires to go deeply into the stream. So folds sometimes may be
    // less useful than in the strict case
  }

  // Note. foldLeft is eager; cannot be used to work with infinite streams. So
  // foldRight is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z: =>B) (f: (A, =>B) =>B): B = this match {
    case Empty => z
    case Cons (h,t) => t().foldLeft (f (h (), z)) (f)
    // Note 2. even if f does not force z, foldLeft will continue to recurse
  }

  def exists (p : A => Boolean): Boolean = this match {
    case Empty => false
    case Cons (h, t) => p (h ()) || t ().exists (p)
    // Note 1. lazy; tail is never forced if satisfying element found this is
    // because || is non-strict
    // Note 2. this is also tail recursive (because of the special semantics of ||)
  }

  // Exercise 2
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => List()
  }


  // Exercise 3
  def take (n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop (n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }


  // Exercise 4
  def takeWhile (p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  //Exercise 5
  def forAll (p: A => Boolean): Boolean = this.foldRight(false)((h, t) => p(h) && t)


  //Exercise 6
  def takeWhile2 (p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty)


  //Exercise 7
  def headOption2: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))


  //Exercise 8 The types of these functions are omitted as they are a part of the exercises
  def map[B](f: A=>B): Stream[B] = {
    foldRight(empty[B])((h,t) => cons(f(h), t))
  }

  def filter(f: A=>Boolean):Stream[A] =
    foldRight(empty[A])((h,t) => if (f(h)) cons(h,t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  //Exercise 09
  //Put your answer here:
  /*
    Transformations on Streams are done in such manner that only the memory needed to check the current element
    is allocated and then the memory is reclaimed back.
  */

  // Exercise 13
  def map_[B] (f: A=>B): Stream[B] = unfold(this){
    case Cons(h, t) => Some(f(h()),t())
    case _ => None
  }

  def take_  (n: Int): Stream[A] = unfold((this,n)) {
    case (Cons(h,t), 1) => Some((h(), (empty, 0)))
    case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
    case _ => None
  }

  def takeWhile_ (p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith_ [B,C](f: (A,B)=> C)(s: Stream[B]): Stream[C] = unfold((this,s)){
    case (Cons(h1, t1),Cons(h2,t2)) => Some(f(h1(), h2()), (t1(),t2()))
    case _ => None
  }

}


case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons (() => head, () => tail)
  }

  def apply[A] (as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons (as.head, apply (as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  // Exercise 1

  def from (n: Int): Stream[Int] = cons(n, from(n+1))

  def to (n: Int): Stream[Int] = cons(n, to(n-1))

  val naturals: Stream[Int] = from(1) /// if not implemented the whole test suit fails!!

  //Exercise 10
  //Put your answer here:
  lazy val fibs = {
    def go(f: BigInt, s: BigInt): Stream[BigInt] = {
      cons(f,go(s, f+s))
    }
    go(0,1)
  }

  //Exercise 11
  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] = f(z).fold(empty[A])((p: (A,S)) => cons(p._1,unfold(p._2)(f)))

  // Exercise 12

  def fibs1 = unfold((0,1)) { case (f1,f2) => Some((f1,(f2,f1+f2))) }
  def from1(n: Int) = unfold(n)(n => Some((n,n+1)))

}
