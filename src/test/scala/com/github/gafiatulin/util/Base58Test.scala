package com.github.gafiatulin.util

import org.scalatest.{Inspectors, Matchers, WordSpec}
import scodec.bits.{ByteVector, HexStringSyntax}

//https://github.com/bitcoin/bitcoin/blob/master/src/test/data/base58_encode_decode.json
class Base58Test extends WordSpec with Matchers with Inspectors {
  val testData: Seq[(ByteVector, String)] = Seq(
    hex"" -> "",
    hex"61" -> "2g",
    hex"626262" -> "a3gV",
    hex"636363" -> "aPEr",
    hex"73696d706c792061206c6f6e6720737472696e67" -> "2cFupjhnEsSn59qHXstmK2ffpLv2",
    hex"00eb15231dfceb60925886b67d065299925915aeb172c06647" -> "1NS17iag9jJgTHD1VXjvLCEnZuQ3rJDE9L",
    hex"516b6fcd0f" -> "ABnLTmg",
    hex"bf4f89001e670274dd" -> "3SEo3LWLoPntC",
    hex"ecac89cad93923c02321" -> "EJDM8drfXA6uyA",
    hex"10c8511e" -> "Rt5zm",
    hex"00000000000000000000" -> "1111111111"
  )

  "Base58" must {
    "encode" in {
      forAll(testData){ case (bv, s) =>
        Base58.encode(bv.toArray) shouldBe s
      }
    }
    "decode" in {
      forAll(testData){ case (bv, s) =>
        Base58.decode(s) shouldBe bv.toArray
      }
    }
  }
}
