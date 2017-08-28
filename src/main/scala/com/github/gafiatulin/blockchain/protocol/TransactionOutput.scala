package com.github.gafiatulin.blockchain.protocol

import scodec.Codec
import scodec.codecs._

final case class TransactionOutput(value: Long, scriptPubKey: ScriptPubKey)

case object TransactionOutput{
  final implicit val codec: Codec[TransactionOutput] =
    (("value" | int64L) :: ("scriptPubKey" | variableSizeBytesLong(VarInt.varIntCodec, Codec[ScriptPubKey]))).as[TransactionOutput]
}
