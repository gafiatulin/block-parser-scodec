package com.github.gafiatulin.blockchain.protocol

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

final case class TransactionInput(previousOutput: OutPoint, signatureScript: ByteVector, sequence: Long)

case object TransactionInput{
  final implicit val codec: Codec[TransactionInput] =
    (("previousOutput" | Codec[OutPoint]) :: ("signatureScript" | VariableSize.varBytes) :: ("sequence" | uint32L)).as[TransactionInput]
}


