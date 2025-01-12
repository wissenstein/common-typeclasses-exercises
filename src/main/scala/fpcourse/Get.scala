package fpcourse

import cats.*
import cats.given
import cats.syntax.either.given
import org.scalacheck.Gen

import java.nio.{ByteBuffer, ByteOrder}
import scala.annotation.tailrec

/**
 * The Get monad parses values from a list of bytes, keeping track of the
 * remaining input after each operation.
 *
 * The run function reads and consumes the bytes needed to construct a value of A.
 * If there is any issue (i.e: insufficient input) it should signal it via a Left result.
 * If everything goes ok, it should return the remaining input along with the parsed value.
 *
 * For more information of a real implementation for Haskell, check out:
 * https://wiki.haskell.org/Dealing_with_binary_data
 */
case class Get[A](run: List[Byte] => Either[String, (List[Byte], A)])

object Get:
  private val InsufficientInput = Left("Insufficient input")
  private val BytesInInt = 4

  /**
   * DONE 1
   * Consumes n bytes of input parsing no value.
   */
  def skip(n: Int): Get[Unit] = Get { bytes =>
    if bytes.length < n then
      InsufficientInput
    else
      Right((bytes.drop(n), ()))
  }

  /**
   * DONE 2
   * True if the input is fully consumed
   */
  def isEmpty: Get[Boolean] = Get { bytes =>
    Right((bytes, bytes.isEmpty))
  }

  /**
   * DONE 3
   * Reads one byte from input
   */
  def getByte: Get[Byte] = Get {
    case Nil => InsufficientInput
    case head :: tail => Right((tail, head))
  }

  /**
   * DONE 4
   * Reads an Int from input using Big Endian order.
   *
   * Hint: Consider using the method replicateA in Applicative.
   */
  def getIntBe: Get[Int] = Get { bytes =>
    getBytes(BytesInInt, bytes)(fourBytes => fourBytes.toInt(ByteOrder.BIG_ENDIAN))
  }

  /**
   * DONE 5
   * Reads an Int from input using Little Endian order.
   *
   * Hint: Consider using the method replicateA in Applicative.
   */
  def getIntLe: Get[Int] = Get { bytes =>
    getBytes(BytesInInt, bytes)(fourBytes => fourBytes.toInt(ByteOrder.LITTLE_ENDIAN))
  }

  /**
   * DONE 6
   * Reads a String of n characters from input.
   */
  def getString(n: Int): Get[String] = Get { bytes =>
    getBytes(n, bytes)(pickedBytes => String(pickedBytes))
  }

  /**
   * Helper function that turns four bytes into an Int. It doesn't check the
   * length of the array, so please make sure to provide 4 bytes.
   */
  private def bytesToIntUnsafe(fourBytes: Array[Byte], order: ByteOrder): Int =
    val bb = ByteBuffer.allocate(4).order(order)
    bb.put(fourBytes)
    bb.flip()
    bb.getInt()

  private def getBytes[A](n: Int, bytes: List[Byte])(f: Array[Byte] => A): Either[String, (List[Byte], A)] =
    @tailrec
    def go(
      timesRemained: Int,
      acc: Either[String, (List[Byte], Array[Byte])]
    ): Either[String, (List[Byte], Array[Byte])] =
      timesRemained match
        case n if n < 1 => acc
        case _ => go(timesRemained - 1, acc.flatMap { (remained, picked) =>
          getByte.run(remained).map { (newRemained, newPicked) =>
            (newRemained, picked :+ newPicked)
          }
        })

    go(n, Right(bytes, Array())) map { (remainedBytes, pickedBytes) =>
      (remainedBytes, f(pickedBytes))
    }

  /**
   * DONE 7
   * Instance of monad error for Get.
   */
  given monadGet: MonadError[Get, String] = new MonadError[Get, String]:
    override def flatMap[A, B](fa: Get[A])(f: A => Get[B]): Get[B] = Get { bytes =>
      fa.run(bytes) match
        case Left(s) => Left(s)
        case Right((remainedBytes, a)) => f(a).run(remainedBytes)
    }

    override def pure[A](x: A): Get[A] = Get { bytes =>
      Right((bytes, x))
    }

    override def tailRecM[A, B](a: A)(f: A => Get[Either[A, B]]): Get[B] = {
      Get { bytes =>
        Monad[[X] =>> Either[String, X]].tailRecM((bytes, a)) { case (bytes, a) =>
          f(a).run(bytes).map { case (bytes, eab) =>
            eab match {
              case Right(b) => Right((bytes, b))
              case Left(a) => Left((bytes, a))
            }
          }
        }
      }
    }

    override def raiseError[A](e: String): Get[A] = Get { _ =>
      Left(e)
    }

    override def handleErrorWith[A](fa: Get[A])(f: String => Get[A]): Get[A] = Get { bytes =>
      fa.run(bytes) match
        case Left(s) => f(s).run(bytes)
        case r @ Right(_) => r
    }

  /**
   * DONE 8
   * Instance of Eq for Get. A full comparison is impossible, so we just
   * compare on a given number of List[Byte] samples and assume that
   * if both Get compute the same result, they are equal.
   *
   * Hint: One possible way of doing this is to use scalacheck to build
   * a generator of List[Byte], then sample it several times (e.g. 32)
   * and check that running both Gets yields the same result every time.
   */
  implicit def eqGet[A: Eq]: Eq[Get[A]] = (x: Get[A], y: Get[A]) =>
    val sampleCount = 32
    val byteGenerator: Gen[List[Byte]] = Gen.listOf(Gen.choose(0, 255).map(_.toByte))
    Iterator
      .continually(byteGenerator.sample)
      .take(sampleCount)
      .flatten
      .forall(bytes => x.run(bytes) === y.run(bytes))

  /**
   * DONE 9
   * Monoid instance for Get.
   */
  implicit def monoid[A: Monoid]: Monoid[Get[A]] = new Monoid[Get[A]]:
    /**
     * Read the docs for combine and come up with an instance that does not
     * alter the behaviour of any Get it is combined with.
     *
     * Think about what should happen to the input bytes, and what would be a
     * suitable result.
     */
    override def empty: Get[A] = Get { bytes =>
      Right(bytes, Monoid[A].empty)
    }

    /**
     * Combining two Get[A] instances should yield a new Get[A] instance which
     * runs both Gets in sequence and yields the combined result.
     *
     * If any of the Gets fails, the combined Get should fail with that same error.
     *
     * Check the tests for details.
     */
    override def combine(x: Get[A], y: Get[A]): Get[A] = Get { bytes =>
      x.run(bytes) match
        case l @ Left(_) => l
        case Right((remainedBytes1, a1)) => y.run(remainedBytes1) match
          case l @ Left(_) => l
          case Right((remainedBytes2, a2)) => Right(remainedBytes2, Monoid[A].combine(a1, a2))
    }

  implicit class ByteArrayOps(val fourBytes: Array[Byte]):
    def toInt(byteOrder: ByteOrder): Int = bytesToIntUnsafe(fourBytes, byteOrder)
