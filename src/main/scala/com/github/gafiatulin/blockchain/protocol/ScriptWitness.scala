package com.github.gafiatulin.blockchain.protocol

import scodec.Codec
import scodec.bits.ByteVector

final case class ScriptWitness(stack: List[ByteVector])

case object ScriptWitness{
  final implicit val codec: Codec[ScriptWitness] = VariableSize.varList[ByteVector](VariableSize.varBytes).as[ScriptWitness]
}

