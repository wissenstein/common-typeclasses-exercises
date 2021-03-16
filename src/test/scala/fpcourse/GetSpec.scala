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

  test("getIntLE fails on insufficient input") {
    val bytes = List[Byte](3, 1, 2)
    Get.getIntBE.run(bytes) shouldBe(Left("Insufficient input"))
  }

  test("getIntLE should read an int in little endian order") {
    val bytes = List[Byte](3, 1, 2, 1, 5, 3, 2)
    Get.getIntLE.run(bytes) shouldBe(Right(List[Byte](5, 3, 2), 16908547))
  }

  test("getByte fails on insufficient input") {
    val bytes = List[Byte]()
    Get.getByte.run(bytes) shouldBe(Left("Insufficient input"))
  }

  test("getByte should read a byte") {
    val bytes = List[Byte](4, 3, 2)
    Get.getByte.run(bytes) shouldBe(Right(List[Byte](3, 2), 4))
  }

  test("isEmpty should be true when input is fully consumed") {
    val bytes = List[Byte]()
    Get.isEmpty.run(bytes) shouldBe(Right((bytes, true)))
  }

  test("isEmpty should be false when there is input remaining") {
    val bytes = List[Byte](5)
    Get.isEmpty.run(bytes) shouldBe(Right((bytes, false)))
  }

  test("skip fails on insufficient input") {
    val bytes = List[Byte](5, 3)
    Get.skip(3).run(bytes) shouldBe(Left("Insufficient input"))
  }

  test("skip consumes bytes and returns no result") {
    val bytes = List[Byte](5, 3, 8, 1)
    Get.skip(3).run(bytes) shouldBe(Right((List[Byte](1), ())))
  }
}
