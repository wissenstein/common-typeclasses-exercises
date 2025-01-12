package fpcourse

import cats.syntax.monoid.given
import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import cats.laws.discipline.MonadErrorTests
import com.wissenstein.test.discipline.FreeSpecDiscipline
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration

class GetSpec extends AnyFreeSpec, Matchers, Configuration, FreeSpecDiscipline, Generators:
  "getIntBe" - {
    "fails on insufficient input" in {
      val bytes = List[Byte](3, 1, 2)
      Get.getIntBe.run(bytes) shouldBe (Left("Insufficient input"))
    }
    "should read an int in big endian order" in {
      val bytes = List[Byte](3, 1, 2, 1, 5, 3, 2)
      Get.getIntBe.run(bytes) shouldBe (Right(List[Byte](5, 3, 2), 50397697))
    }
  }

  "getIntLe" - {
    "fails on insufficient input" in {
      val bytes = List[Byte](3, 1, 2)
      Get.getIntLE.run(bytes) shouldBe (Left("Insufficient input"))
    }
    "getIntLE should read an int in little endian order" in {
      val bytes = List[Byte](3, 1, 2, 1, 5, 3, 2)
      Get.getIntLE.run(bytes) shouldBe (Right(List[Byte](5, 3, 2), 16908547))
    }
  }

  "getByte" - {
    "fails on insufficient input" in {
      val bytes = List[Byte]()
      Get.getByte.run(bytes) shouldBe (Left("Insufficient input"))
    }
    "should read a byte" in {
      val bytes = List[Byte](4, 3, 2)
      Get.getByte.run(bytes) shouldBe (Right(List[Byte](3, 2), 4))
    }
  }

  "isEmpty" - {
    "should be true when input is fully consumed" in {
      val bytes = List[Byte]()
      Get.isEmpty.run(bytes) shouldBe (Right((bytes, true)))
    }
    "should be false when there is input remaining" in {
      val bytes = List[Byte](5)
      Get.isEmpty.run(bytes) shouldBe (Right((bytes, false)))
    }
  }

  "skip" - {
    "fails on insufficient input" in {
      val bytes = List[Byte](5, 3)
      Get.skip(3).run(bytes) shouldBe (Left("Insufficient input"))
    }
    "consumes bytes and returns no result" in {
      val bytes = List[Byte](5, 3, 8, 1)
      Get.skip(3).run(bytes) shouldBe (Right((List[Byte](1), ())))
    }
  }

  "getString" - {
    "fails on insufficient input" in {
      val bytes = List[Byte](1, 6, 2, 1)
      Get.getString(5).run(bytes) shouldBe (Left("Insufficient input"))
    }
    "should read a string of N characters" in {
      val bytes = List[Byte](104, 97, 112, 112, 121, 2, 1)
      Get.getString(5).run(bytes) shouldBe (Right((List[Byte](2, 1), "happy")))
    }
  }

  "combining" - {
    "two Gets yields a Get which runs both Gets in sequence and combines their results" in {
      val bytes = List[Byte](30, 35, 97, 86)
      val firstGet = Get.getByte
      val secondGet = Get.getByte
      val combinedGet = firstGet.combine(secondGet)
      combinedGet.run(bytes) shouldBe (Right((List[Byte](97, 86), 30 + 35)))
    }
    "a failed Get with a successful Get should preserve the error" in {
      val bytes = List[Byte](30, 35, 97, 86)
      val firstGet = Get[Byte](_ => Left("made up error"))
      val secondGet = Get.getByte
      val combinedGet = firstGet.combine(secondGet)
      combinedGet.run(bytes) shouldBe (Left("made up error"))
    }
    "a successful Get with a failed Get should preserve the error" in {
      val bytes = List[Byte](30, 35, 97, 86)
      val firstGet = Get.getByte
      val secondGet = Get[Byte](_ => Left("made up error"))
      val combinedGet = firstGet.combine(secondGet)
      combinedGet.run(bytes) shouldBe (Left("made up error"))
    }
  }

/**
 * TODO 12
 * Write discipline tests for instances of Monoid, Eq and MonadError.
 */
