// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// Work on this file by following the associated exercise sheet
// (available in PDF in the same directory).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain'.
// To load the file int the REPL use the 'console' command.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests (for the solved exercises),
// after you are done with each exercise (if you do them in order).
// Compile and test frequently. Best continously.


// >> https://github.com/fpinscala/fpinscala/blob/first-edition/answers/src/main/scala/fpinscala/datastructures/List.scala
package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 1 requires no programming
  /* 
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    } 
  */
  // -- Will output 3 because it is the only matching > Cons(1, Cons(2, Cons(3, Cons(4, _)))) => 1 + 2 == 3 !


  // Exercise 2
  def tail[A] (as: List[A]) :List[A] = as match {
      case Nil => sys.error("tail of empty list")
      // case Cons(_,Nil) => Nil // optional
      case Cons(_,t) => t
    }


  // Exercise 3
  @annotation.tailrec
  // Uncomment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def drop[A] (l: List[A], n: Int) : List[A] = 
    if (n <= 0) l
    else l match {
      case Nil => sys.error("tail of empty list")
      // case Cons(_,Nil) => sys.error("tail of empty list") // optional
      case Cons(_,t) => drop(t, n-1)
    }


  // Exercise 4
  def dropWhile[A] (l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }


  // Exercise 5
  def init[A] (l: List[A]): List[A] = 
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

    // Helper
  // def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
  //   case Nil => z
  //   case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  // }
  // Exercise 6
  def length[A] (as: List[A]): Int = 
    foldRight(as, 0)((_,acc) => acc + 1)

    // val nums = List(1, 2, 3, 4, 5)
    // println("Length if list: " + length(nums))


  // Exercise 7
  @annotation.tailrec
  // Uncomment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }


  // Exercise 8
  def product (as: List[Int]): Int = 
    foldRight(as, 1)(_ * _)

  def length1[A] (as: List[A]): Int = 
    foldLeft(as, 0)((acc,_) => acc + 1)


  // Exercise 9
  def reverse[A] (as: List[A]): List[A] = 
    foldLeft(as, List[A]())((acc,h) => Cons(h,acc))
    // foldLeft(as, Nil:List[A])((acc,h) => Cons(h,acc)) // same as above ??


  // Exercise 10
  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B = 
    foldLeft(reverse(as), z)((b,a) => f(a,b))


  // Exercise 11
  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B = 
    foldRight(as, (b:B) => b)((a,g) => (b: B) => g(f(b,a)))(z)


  // Exercise 12
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }
  // def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(_++_)

  def concat[A] (as: List[List[A]]): List[A] = 
    foldRight(as, Nil:List[A])(append)
    // foldRight(as, List[A]())(append) // same as above
    // foldLeft(as, Nil:List[A])(append) // works as well


  // Exercise 13
  // def filter[A] (as: List[A]) (p: A => Boolean): List[A] = 
  //   foldRight(as, Nil:List[A])((h,t) => if (p(h)) Cons(h,t) else t)
    def filter[A] (as: List[A]) (p: A => Boolean): List[A] = as match {
      case Nil => Nil
      case Cons(h, t) => if (p(h) != true) filter(t)(p) else Cons(h, filter(t)(p))
    }
    


  // Exercise 14
  def map[A,B](l: List[A])(f: A => B): List[B] = // added as helper to flatMap
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    concat(map(as)(f))


  // Exercise 15
  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = 
    flatMap(l)(a => if (p(a)) List(a) else Nil)


  // Exercise 16
  def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, add(t1)(t2))
  }


  // Exercise 17
  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C] = (l,r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(f)(t1,t2))
  }


  // Exercise 18

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match { // helper func
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }

}
