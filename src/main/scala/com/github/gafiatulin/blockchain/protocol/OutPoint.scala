package com.github.gafiatulin.blockchain.protocol

import scodec.Codec
import scodec.codecs._

final case class OutPoint(hash: Hash, index: Long)

case object OutPoint{
  final implicit val codec: Codec[OutPoint] = (("hash" | Hash.codec(32)) :: ("index" | uint32L)).as[OutPoint]
}
