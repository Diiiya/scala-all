// wasowski, Advanced Programming, IT University of Copenhagen

// ADPR group
// Diyana Boyadzhieva
// Fadi Dasus

package fpinscala.laziness
import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import stream00._    // uncomment to test the book solution (should pass your tests)
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  import Stream._

  // A simple converter of lists to streams

  def list2stream[A] (la: List[A]): Stream[A] =
    la.foldRight (Stream.empty[A]) (cons[A](_,_))

  // There  is  a name  clash  between  Stream.empty and  the  testing
  // library, so we need to qualify Stream.empty

  // An example generator  of random finite non-empty  streams (we use
  // the  built in  generator of  lists and  convert them  to streams,
  // using the above converter)
  //
  // 'suchThat'  filters  out  the  generated instances  that  do  not
  // satisfy the predicate given in the right argument.

  def genNonEmptyStream[A] (implicit arbA: Arbitrary[A]): Gen[Stream[A]] =
    for {
      la <- arbitrary[List[A]] suchThat { _.nonEmpty }
    } yield list2stream (la)

  "headOption" - {

    // Exercise 1 (no coding, understand)

    // A scenario test:

    "returns None on an empty Stream (01)" in {

      Stream.empty.headOption shouldBe (None)
    }


    // Two property tests:

    "returns the head of a singleton stream packaged in Some (02)" in {

      forAll { (n: Int) =>
        cons (n, Stream.empty).headOption should be (Some (n))
      }
    }

    "returns the head of random stream packaged in Some (02)" in {

      // Make the generator available in the context
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // Uses our generator of non empty streams
      // thanks to the implicit declaration above
      forAll { (s: Stream[Int]) =>
        s.headOption shouldNot be (None)
      }
    }

    // Exercise 2 (add here)
    "doesn't force the tail of the stream" in {
      forAll {(n: Int) =>
        cons (n, cons(sys.error("forces the tail of the stream"), Stream.empty)).headOption should be (Some(n))
      }
    }

  }

  "take" - {

    // Exercise 3
    "doesn't force any heads, nor tails" in {
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll {
        (s: Stream[Int], n: Int) => s.map(x => sys.error("forces heads or tails")).take(n)
      }

    }

    // Exercise 4
    "take(n) doesn't force the (n+1)st head ever" in {
      implicit def arbIntStream =
         Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll {(stream :Stream[Int]) => {
        val x = stream.toList.size
        val stream2 = stream.append(cons(???, ???))
        stream2.take(x).toList
      }}
    }

    // Exercise 5
    "s.take(n).take(n) == s.take(n) for any n and stream s" in {

      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (s: Stream[Int], n: Int) =>
        (s.take(n)).take(n).toList should equal((s.take(n)).toList)
      }

    }

  }

  "drop" - {

    // Exercise 6
    "s.drop(n).drop(m) ==s.drop(n+m) for any n and m" in {

      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (s: Stream[Int], n: Int, m: Int) =>
        if (n > 0 && m > 0 && Int.MaxValue-n > m) (s.drop(n)).drop(m).toList should equal((s.drop(n+m)).toList) else true
      }

    }

    // Exercise 7

  }


  "map" - {

     // Exercise 8
     "x.map(id) == x for any stream" in {
        implicit val arbIntStream =
          Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

        forAll { (x: Stream[Int], n: Int) =>
          x.map(identity).toList should equal(x.toList)
        }
     }

     // Exercise 9
     "map terminates on infinite streams" in {

       implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

        forAll {
          (n: Int) => from(n).map(identity)
        }

     }

  }

  "append" - {

    // Exercise 10

  }

}
