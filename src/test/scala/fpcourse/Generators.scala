package fpcourse

import org.scalacheck.{Arbitrary, Gen}

trait Generators {
  implicit def arbFun[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary(arbA.arbitrary.map(a => ((_: A) => a)))

  implicit def arbGet[A](implicit arbA: Arbitrary[A]): Arbitrary[Get[A]] = {
    val successful: Gen[Get[A]] =
      for {
        pctToDrop <- Gen.choose(0, 100)
        a         <- arbA.arbitrary
      } yield Get(bytes => Right((bytes.drop(bytes.length * pctToDrop / 100), a)))

    val failed: Gen[Get[A]] = Gen.alphaNumStr.map(s => Get(_ => Left(s)))

    Arbitrary(Gen.oneOf(successful, failed))
  }
}
