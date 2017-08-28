package com.github.gafiatulin.protocol

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.bytes

final case class Hash(value: ByteVector) {
  def intSize: Option[Int] = value.intSize
  def size: Long = value.size
  override def toString: String = value.reverse.toHex
}

object Hash {
  final implicit def codec(size: Int): Codec[Hash] = bytes(size).xmap(Hash.apply, _.value)
  final def nil(size: Int) = Hash(ByteVector.fill(size.toLong)(0))
}