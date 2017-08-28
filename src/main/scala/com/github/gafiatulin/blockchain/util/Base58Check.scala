package com.github.gafiatulin.blockchain.util

case object Base58Check {
  def checksum(data: Array[Byte]): Array[Byte] = HashUtil.hash256(data).take(4)

  def encode(netPrefix: Array[Byte], data: Array[Byte]): String = {
    val prefixAndData = netPrefix ++ data
    Base58.encode(prefixAndData ++ checksum(prefixAndData))
  }

  def decode(encoded: String): Array[Byte] = {
    val raw = Base58.decode(encoded)
    val (versionAndHash, checksum) = raw.splitAt(raw.length - 4)
    require(checksum sameElements Base58Check.checksum(versionAndHash), s"Invalid Base58Check data $encoded")
    require(versionAndHash(0) == 0, "")
    versionAndHash.tail
  }
}