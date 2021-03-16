package fpcourse

import java.nio.{ByteBuffer, ByteOrder}

case class Get[A](run: List[Byte] => Either[String, (List[Byte], A)])

object Get {
  def skip(n: Int): Get[Unit] = ???
  def bytesRead: Get[Long] = ???
  
  def isEmpty: Get[Boolean] = Get { bytes =>
    Right((bytes, bytes.isEmpty))
  }

  def getByte: Get[Byte] = Get { bytes =>
    if(bytes.isEmpty) Left("Insufficient input")
    else Right((bytes.tail, bytes.head))
  }

  def getIntBE: Get[Int] = Get { bytes =>
    if(bytes.length < 4) Left("Insufficient input")
    else {
      val n = bytesToInt(bytes.take(4).toArray, ByteOrder.BIG_ENDIAN)
      val rest = bytes.drop(4)
      Right((rest, n))
    }
  }

  def getIntLE: Get[Int] = Get { bytes =>
    if(bytes.length < 4) Left("Insufficient input")
    else {
      val n = bytesToInt(bytes.take(4).toArray, ByteOrder.LITTLE_ENDIAN)
      val rest = bytes.drop(4)
      Right((rest, n))
    }
  }

  private def bytesToInt(fourBytes: Array[Byte], order: ByteOrder): Int = {
    val bb = ByteBuffer.allocate(4).order(order)
    bb.put(fourBytes)
    bb.flip()
    bb.getInt()
  }
}