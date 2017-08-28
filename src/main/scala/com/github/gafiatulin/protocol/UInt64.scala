package com.github.gafiatulin.protocol

import scodec.Codec
import scodec.codecs.int64L

final case class UInt64(underlying: Long)

case object UInt64{
  final def longToBigInt(unsignedLong: Long): BigInt =
    (BigInt(unsignedLong >>> 1) << 1) + (unsignedLong & 1)

  final def bigIntToLong(n: BigInt): Long = {
    val smallestBit = (n & 1).toLong
    ((n >> 1).toLong << 1) | smallestBit
  }
  final implicit val codec: Codec[UInt64] = int64L.xmap(UInt64.apply, _.underlying)
  final implicit val bigIntCodec: Codec[BigInt] = Codec[UInt64].xmap(n => UInt64.longToBigInt(n.underlying), b => UInt64(UInt64.bigIntToLong(b)))
}
