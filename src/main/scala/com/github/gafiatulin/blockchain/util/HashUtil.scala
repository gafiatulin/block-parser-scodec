package com.github.gafiatulin.blockchain.util

import java.security.MessageDigest

import com.github.gafiatulin.blockchain.protocol.Hash
import org.spongycastle.crypto.digests.RIPEMD160Digest
import scodec.bits.ByteVector

case object HashUtil {
  final def ripeMd160(bytes: Array[Byte]): Array[Byte] = {
    val messageDigest = new RIPEMD160Digest
    messageDigest.update(bytes, 0, bytes.length)
    val out = Array.fill[Byte](messageDigest.getDigestSize)(0)
    messageDigest.doFinal(out, 0)
    out
  }

  final def sha1(bytes: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-1").digest(bytes)

  final def sha256(bytes: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bytes)

  final def hash160(bytes: Array[Byte]): Array[Byte] =
    ripeMd160(sha256(bytes))

  final def hash256(bytes: Array[Byte]): Array[Byte] =
    sha256(sha256(bytes))

  final def toHash(bytes: Array[Byte]) = Hash(ByteVector(bytes))
}
