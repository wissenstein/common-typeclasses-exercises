package fpcourse

import cats._
import cats.implicits._
import org.scalacheck.Gen

import java.nio.{ByteBuffer, ByteOrder}

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

object Get {
  /**
   * TODO 1
   * Consumes n bytes of input parsing no value.
   */
  def skip(n: Int): Get[Unit] = Get { bytes =>
    if(bytes.length < n) Left("Insufficient input")
    else Right((bytes.drop(n), ()))
  }

  /**
   * TODO 2
   * True if the input is fully consumed
   */
  def isEmpty: Get[Boolean] = Get { bytes =>
    Right((bytes, bytes.isEmpty))
  }

  /**
   * TODO 3
   * Reads one byte from input
   */
  def getByte: Get[Byte] = Get { bytes =>
    if(bytes.isEmpty) Left("Insufficient input")
    else Right((bytes.tail, bytes.head))
  }

  /**
   * TODO 4
   * Reads an Int from input using Big Endian order.
   *
   * Hint: Consider using the method replicateA in Applicative.
   */
  def getIntBE: Get[Int] = getByte.replicateA(4).map { bytes =>
    bytesToIntUnsafe(bytes.toArray, ByteOrder.BIG_ENDIAN)
  }

  /**
   * TODO 5
   * Reads an Int from input using Little Endian order.
   *
   * Hint: Consider using the method replicateA in Applicative.
   */
  def getIntLE: Get[Int] = getByte.replicateA(4).map { bytes =>
    bytesToIntUnsafe(bytes.toArray, ByteOrder.LITTLE_ENDIAN)
  }

  /**
   * TODO 6
   * Reads a String of n characters from input.
   */
  def getString(n: Int): Get[String] = getByte.replicateA(n).map { bytes =>
    new String(bytes.toArray)
  }

  /**
   * Helper function that turns four bytes into an Int. It doesn't check the
   * length of the array, so please make sure to provide 4 bytes.
   */
  private def bytesToIntUnsafe(fourBytes: Array[Byte], order: ByteOrder): Int = {
    val bb = ByteBuffer.allocate(4).order(order)
    bb.put(fourBytes)
    bb.flip()
    bb.getInt()
  }

  /**
   * TODO 7
   * Instance of monad error for Get.
   */
  implicit val monadGet: MonadError[Get, String] = new MonadError[Get, String] {
    override def flatMap[A, B](fa: Get[A])(f: A => Get[B]): Get[B] = Get { bytes =>
      fa.run(bytes) match {
        case Right((remainingBytes, a)) => f(a).run(remainingBytes)
        case Left(e) => Left(e)
      }
    }

    override def pure[A](x: A): Get[A] = Get { bytes =>
      Right((bytes, x))
    }

    override def tailRecM[A, B](a: A)(f: A => Get[Either[A, B]]): Get[B] = {
      Get { bytes =>
        Monad[Either[String, *]].tailRecM((bytes, a)) { case (bytes, a) =>
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
      fa.run(bytes) match {
        case r @ Right(_) => r
        case Left(e) => f(e).run(bytes)
      }
    }
  }

  /**
   * TODO 8
   * Instance of Eq for Get. A full comparison is impossible, so we just
   * compare on a given number of List[Byte] samples and assume that
   * if both Get compute the same result, they are equal.
   */
  implicit def eqGet[A: Eq]: Eq[Get[A]] = Eq.instance { (g1, g2) =>
    val nSamples = 32
    val genBytes = Gen.listOf(Gen.choose(0, 255).map(_.toByte))
    Iterator
      .continually(genBytes.sample)
      .take(nSamples)
      .flatten
      .forall(bs => g1.run(bs) === g2.run(bs))
  }

  /**
   * TODO 9
   * Monoid instance for Get.
   */
  implicit def monoid[A: Monoid]: Monoid[Get[A]] = new Monoid[Get[A]] {
    /**
     * Read the docs for combine and come up with an instance that does not
     * alter the behaviour of any Get it is combined with.
     *
     * Think about what should happen to the input bytes, and what would be a
     * suitable result.
     */
    override def empty: Get[A] = Monad[Get].pure(Monoid[A].empty)

    /**
     * Combining two Get[A] instances should yield a new Get[A] instance which
     * runs both Gets in sequence and yields the combined result.
     *
     * If any of the Gets fails, the combined Get should fail with that same error.
     *
     * Check the tests for details.
     */
    override def combine(x: Get[A], y: Get[A]): Get[A] =
      (x, y).mapN((a1, a2) => a1 |+| a2)
  }
}