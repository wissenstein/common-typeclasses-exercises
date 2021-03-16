package fpcourse

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GetSpec extends AnyFunSuite with Matchers {
  test("getIntBE fails on insufficient input") {
    val bytes = List[Byte](3, 1, 2)
    Get.getIntBE.run(bytes) shouldBe(Left("Insufficient input"))
  }

  test("getIntBE should read an int in big endian order") {
    val bytes = List[Byte](3, 1, 2, 1, 5, 3, 2)
    Get.getIntBE.run(bytes) shouldBe(Right(List[Byte](5, 3, 2), 50397697))
  }


}
