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

// The extension of App allows writing the main method statements at the top
// level (the so called default constructor). For App objects they will be
// executed as if they were placed in the main method in Java.

package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 3
  def fib (n: Int): Int = n match {
    case 1 => 0
    case 2 => 1
    case _ => fib(n-1) + fib(n-2)
  }

  // Exercise 4
  def isSorted[A] (as: Array[A], comparison: (A,A) =>  Boolean): Boolean = {
    // def go(n: Int): Boolean =
    //   if (n >= as.length-1) true
    //   else if (comparison(as(n), as(n+1))) true
    //   else if (comparison(as(n), as(n))) false
    //   else if (comparison(as(n), as(n-1))) false
    //   else go(n+1)

    // go(0)

    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true // if there's 1 element in the array
      else if (! comparison(as(n), as(n+1))) false // if comparison return false
      else go(n+1)

    go(0)

    // @annotation.tailrec
    // def go(n: Int): Boolean =
    //   if (n == as.length - 1) comparison(as(n-1), as(n))
    //   else if (! comparison(as(n-1), as(n))) false
    //   else go(n + 1)

    // if (as.size == 0 || as.size == 1) true
    // else go(1)
  }

  // Exercise 5
  def curry[A,B,C] (f: (A,B)=>C): A => (B => C) = 
    (a: A) => (b: B) => f(a, b)
    // (a) => (b) => f(a, b) // also works

  // Exercise 6
  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = 
    (a, b) => f(a)(b)
    // (x, y) => f(x)(y) // also works -- the param names doesnt matter
    // (x: A, y: B) => f(x)(y) also

  // Exercise 7
  def compose[A,B,C] (f: B => C, g: A => B) : A => C = 
    a => f(g(a))

}
