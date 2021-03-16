package fpcourse

import cats._
import cats.implicits._
import org.scalacheck.Gen

import java.nio.{ByteBuffer, ByteOrder}
import scala.util.Try

case class Get[A](run: List[Byte] => Either[String, (List[Byte], A)])

object Get {
  def skip(n: Int): Get[Unit] = Get { bytes =>
    if(bytes.length < n) Left("Insufficient input")
    else Right((bytes.drop(n), ()))
  }

  def isEmpty: Get[Boolean] = Get { bytes =>
    Right((bytes, bytes.isEmpty))
  }

  def getByte: Get[Byte] = Get { bytes =>
    if(bytes.isEmpty) Left("Insufficient input")
    else Right((bytes.tail, bytes.head))
  }

  def getIntBE: Get[Int] = getByte.replicateA(4).map { bytes =>
    bytesToInt(bytes.toArray, ByteOrder.BIG_ENDIAN)
  }

  def getIntLE: Get[Int] = getByte.replicateA(4).map { bytes =>
    bytesToInt(bytes.toArray, ByteOrder.LITTLE_ENDIAN)
  }

  def getString(n: Int): Get[String] = getByte.replicateA(n).map { bytes =>
    new String(bytes.toArray)
  }

  private def bytesToInt(fourBytes: Array[Byte], order: ByteOrder): Int = {
    val bb = ByteBuffer.allocate(4).order(order)
    bb.put(fourBytes)
    bb.flip()
    bb.getInt()
  }

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

  implicit def eqGet[A: Eq]: Eq[Get[A]] = Eq.instance { (g1, g2) =>
    val nSamples = 32
    val genBytes = Gen.listOf(Gen.choose(0, 255).map(_.toByte))
    Iterator
      .continually(genBytes.sample)
      .take(nSamples)
      .flatten
      .forall(bs => g1.run(bs) === g2.run(bs))
  }

  implicit def monoid[A: Monoid]: Monoid[Get[A]] = new Monoid[Get[A]] {
    override def empty: Get[A] = Monad[Get].pure(Monoid[A].empty)

    override def combine(x: Get[A], y: Get[A]): Get[A] =
      (x, y).mapN((a1, a2) => a1 |+| a2)
  }
}