package com.github.gafiatulin.protocol

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bytes, listOfN, variableSizeBytesLong}

case object VariableSize{
  final val varBytes: Codec[ByteVector] = variableSizeBytesLong(VarInt.varIntCodec, bytes)
  final implicit def varList[A](codec: Codec[A]): Codec[List[A]] = listOfN(VarInt.varIntCodec.xmap(_.toInt, (i: Int) => i.toLong), codec)
}
