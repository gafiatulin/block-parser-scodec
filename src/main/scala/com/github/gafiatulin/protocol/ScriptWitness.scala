package com.github.gafiatulin.protocol

import scodec.bits.ByteVector
import scodec.Codec

final case class ScriptWitness(stack: List[ByteVector])

case object ScriptWitness{
  final implicit val codec: Codec[ScriptWitness] = VariableSize.varList[ByteVector](VariableSize.varBytes).as[ScriptWitness]
}

