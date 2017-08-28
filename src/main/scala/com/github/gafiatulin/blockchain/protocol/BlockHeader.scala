package com.github.gafiatulin.blockchain.protocol

import com.github.gafiatulin.blockchain.util.HashUtil
import scodec.Codec
import scodec.codecs._

final case class BlockHeader(version: Long, previousBlockHash: Hash, merkleRootHash: Hash, time: Long, bits: Long, nonce: Long){
  def hash(hashing: Option[Array[Byte] => Array[Byte]] = None): Hash =
    BlockHeader.codec.encode(this).toOption.fold(Hash.nil(32))(bv => HashUtil.toHash(
      hashing.getOrElse(HashUtil.hash256 _).apply(bv.toByteArray)
    ))
}

object BlockHeader {
  final implicit val codec: Codec[BlockHeader] = (
    ("version" | uint32L) ::
    ("previousBlockHash" | Hash.codec(32)) ::
    ("merkleRootHash" | Hash.codec(32)) ::
    ("time" | uint32L) ::
    ("bits" | uint32L) ::
    ("nonce" | uint32L)
  ).as[BlockHeader]
}
