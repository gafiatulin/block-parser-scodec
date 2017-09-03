package com.github.gafiatulin.blockchain.protocol

import scodec.Codec
import scodec.codecs._

final case class Block(blockHeader: BlockHeader, transactions: List[Transaction]){
  def blockHash(hashing: Option[Array[Byte] => Array[Byte]] = None): Hash = blockHeader.hash(hashing)
}

case object Block{
  final implicit val codec: Codec[Block] =
    (("blockHeader" | Codec[BlockHeader]) :: ("transactions" | VariableSize.varList(Transaction.codec))).as[Block]
}


