package com.github.gafiatulin.blockchain.util

import java.math.BigInteger

import scala.annotation.tailrec

case object Base58 {
  private val alphabet: String = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val map: Map[Char, Int] = alphabet.zipWithIndex.toMap

  def encode: PartialFunction[Array[Byte], String] = {
    case input if input.isEmpty => ""
    case input =>
      val builder = new StringBuilder
      @tailrec def go(current: BigInteger): Unit = current match {
        case BigInteger.ZERO => ()
        case _ =>
          val Array(x, remainder) = current.divideAndRemainder(BigInteger.valueOf(58L))
          builder.append(alphabet.charAt(remainder.intValue))
          go(x)
      }
      go(new BigInteger(1, input))
      input.takeWhile(_ == 0).foreach(_ => builder.append(alphabet.charAt(0)))
      builder.toString().reverse
  }

  def decode(base58: String): Array[Byte] = {
    val zeroes = base58.takeWhile(_ == '1').map(_ => 0: Byte).toArray
    val trim = base58.dropWhile(_ == '1').toList
    val decoded = trim.foldLeft(BigInteger.ZERO)((a, b) => a.multiply(BigInteger.valueOf(58L)).add(BigInteger.valueOf(map(b).toLong)))
    if (trim.isEmpty) zeroes else zeroes ++ decoded.toByteArray.dropWhile(_ == 0)
  }
}