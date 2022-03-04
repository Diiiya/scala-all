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
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.  The tests
// will fail for unfnished parts.  Comment such out.

package adpro

// Exercise  1

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

  this: java.awt.Point =>

  override def compare (that: java.awt.Point): Int =  
    if (getX == that.getX) getY compare that.getY
    else getX compare that.getX

}

// Try the following (and similar) tests in the repl (sbt console):
// val p = new java.awt.Point(0,1) with OrderedPoint
// val q = new java.awt.Point(0,2) with OrderedPoint
// assert(p < q)

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2
  def size[A] (t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r) + 1
  }


  // Exercise 3
  def maximum (t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l).max(maximum(r))
  }


  // Exercise 4
  def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
  

  // Exercise 5  
  // Swap the signature of the method so that first it handles the Leaf and then the Branch logic
  def fold[A,B](t: Tree[A])(g: A => B)(f: (B,B) => B): B = t match {
    case Leaf(a) => g(a)
    case Branch(l,r) => f(fold(l)(g)(f), fold(r)(g)(f))
  }

  def size1[A] (t: Tree[A]): Int = fold(t)(a => 1)(_ + _ + 1)

  def maximum1 (t: Tree[Int]): Int = fold(t)(a => a)(_.max(_))

  def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

}

sealed trait Option[+A] {

  // Exercise 6
  def map[B] (f: A=>B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }


  /**
   * Ignore the arrow (=>) in default's type below for now.
   * It prevents the argument "default" from being evaluated until it is needed.
   * So it is not evaluated if we process a Some object (this is 'call-by-name'
   * and we should talk about this soon).
   */

  def getOrElse[B >: A] (default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // Flatmap creates a new List based on specific function
  // List(1, 2, 3).flatMap(duplicate) => List(1, 1, 2, 2, 3, 3)
  // => flatMap implementation uses 1st map based on the function
  // map internally is applied for each element
  // the getOrElse takes only the valid ones -- of type Some
  // else returns None ??
  def flatMap[B] (f: A => Option[B]): Option[B] = map(f) getOrElse None

  def filter (p: A => Boolean): Option[A] = this match {
    case Some(a) if p(a) => this
    case _ => None
  }

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // mean is implemented in Chapter 4 of the text book
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some (xs.sum / xs.length)

  // Exercise 7
  def headOption[A] (lst: List[A]): Option[A] = lst match {
    case Nil => None
    case h::t => Some(h)
  }


  def headGrade (lst: List[(String,Int)]): Option[Int] = headOption (lst map (x => x._2))


  // Exercise 8
  def variance (xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
      .flatMap(a => Some(xs.map(e => Math.pow(e - a, 2))))
      .flatMap(as => Some(as.sum / as.length))
  }


  // Exercise 9
  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = ao flatMap (a2 => bo map (b2 => f(a2, b2)))


  // Exercise 10
  def sequence[A] (aos: List[Option[A]]): Option[List[A]] = 
    aos.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)((head, tail) => head::tail))
    // ((head, tail) => head::tail)) like (_ :: _)


  // Exercise 11
  def traverse[A,B] (as: List[A]) (f: A => Option[B]): Option[List[B]] = 
    as.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))

}
