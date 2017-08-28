package com.github.gafiatulin.blockchain.util

import org.scalatest.{Inspectors, Matchers, WordSpec}

import scala.util.Success

class Bech32Test extends WordSpec with Matchers with Inspectors{

  //https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#test-vectors
  val testData = Seq(
    "A12UEL5L",
    "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs",
    "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw",
    "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j",
    "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
  )

  "Bech32" must {
    "decode" in {
      forAll(testData){ s =>
        Bech32.decode(s) shouldBe a[Success[_]]
      }
    }

    "round trip" in {
      forAll(testData){ s =>
        Bech32.decode(s).flatMap((Bech32.encode _).tupled) shouldBe Success(s.toLowerCase)
      }
    }
  }
}
