package fpcourse

import org.scalacheck.{Arbitrary, Gen}

trait Generators:
  /**
   * DONE 10
   * Instance of Arbitrary for functions A => A.
   * No constraints on what the function does.
   * Simple solutions should be enough.
   */
  implicit def arbFun[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary(arbA.arbitrary.map(a => _ => a))

  /**
   * DONE 11
   * Instance of Arbitrary for the Get monad.
   * Any solution should suffice.
   * Some optional constraints if you want to spice things up:
   * - allow for both successful and failed return values to be produced
   * - when running the Get successfully, the remaining bytes should be
   *   a suffix of the bytes passed as argument to the run function; that is,
   *   you should simulate actual 'consumption' of the bytes.
   */
  implicit def arbGet[A](implicit arbA: Arbitrary[A]): Arbitrary[Get[A]] =
    val successful: Gen[Get[A]] =
      for
        percentToDrop <- Gen.choose(0, 100)
        a <- arbA.arbitrary
      yield
        Get { bytes => Right((bytes.drop(bytes.length * percentToDrop / 100), a))}  
    
    val failed: Gen[Get[A]] =
      Gen.alphaNumStr map { errorMessage => 
        Get { bytes => Left(errorMessage) }
      }
      
    Arbitrary(Gen.oneOf(successful, failed))
