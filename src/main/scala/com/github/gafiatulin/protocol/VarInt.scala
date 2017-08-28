package com.github.gafiatulin.protocol

import scodec.Attempt.Successful
import scodec.bits.BitVector
import scodec.codecs.{uint16L, uint32L, uint8L}
import scodec.{Attempt, Codec, DecodeResult}

case object VarInt {
  private final val long2AttemptPF: PartialFunction[Long, Attempt[BitVector]] = {
    case n if n < 0xFDL       => uint8L.encode(n.toInt)
    case n if n < 0xFFFFL     => uint8L.encode(0xFD).flatMap(a => uint16L.encode(n.toInt).map(a ++))
    case n if n < 0xFFFFFFFFL => uint8L.encode(0xFE).flatMap(a => uint32L.encode(n).map(a ++))
    case n                    => uint8L.encode(0xFF).flatMap(a => UInt64.bigIntCodec.encode(BigInt(n)).map(a ++))
  }

  private final val int2AttemptLongPF: PartialFunction[DecodeResult[Int], Attempt[DecodeResult[Long]]] = {
    case dr if dr.value == 0xFF => UInt64.bigIntCodec.decode(dr.remainder).map(_.map(_.toLong))
    case dr if dr.value == 0xFE => uint32L.decode(dr.remainder)
    case dr if dr.value == 0xFD => uint16L.decode(dr.remainder).map(_.map(_.toLong))
    case dr => Successful(DecodeResult(dr.value.toLong, dr.remainder))
  }

  final implicit val varIntCodec: Codec[Long] = Codec[Long](
    long2AttemptPF,
    (bv: BitVector) => uint8L.decode(bv).flatMap(int2AttemptLongPF)
  )
}