package com.github.gafiatulin.blockchain.protocol

import com.github.gafiatulin.blockchain.util.NetParams
import org.scalatest.{Inspectors, Matchers, WordSpec}
import scodec.bits.{ByteVector, HexStringSyntax}

import scala.io.Source

class BlockTest extends WordSpec with Matchers with Inspectors {
  val testData = Seq(
    (
      new NetParams {
        val hrp: String = "bc"
        val pkhPrefix: Array[Byte] = hex"00".toArray
        val shPrefix: Array[Byte] = hex"05".toArray
      },
      "btc_main_482333",
      "00000000000000000099931f05a6ef98d4e5de0aba96c2be9729663e9375a6ea",
      1879,
      (
        5,
        "7c317ba871f2256048175ead74fecc240789594273b576befe4b6e5657f08ea1",
        1,
        "1Kn3B4B9Kd6aCnbPWkty4vh4bV9z7k4LmC",
        2300000000L
      )
    ),
    (
      new NetParams {
        val hrp: String = "tb"
        val pkhPrefix: Array[Byte] = hex"6F".toArray
        val shPrefix: Array[Byte] = hex"C4".toArray
      },
      "btc_test_1180333",
      "0000000065e92bbc865da6121eed1ba9fa733e7c8459fe6bf43181536b9fd703",
      73,
      (
        16,
        "aa98e576c3b7ad0793772f3627eb706819e345aa95a951e1ab22774fba68fadf",
        5,
        "2NCeVtgVEQAfJXhFWZE29D6jVuavYjYJMV4",
        200000000L
      )
    ),
    (
      new NetParams {
        val hrp: String = "ltc"
        val pkhPrefix: Array[Byte] = hex"30".toArray
        val shPrefix: Array[Byte] = hex"32".toArray
      },
      "ltc_main_1266746",
      "3ee91fabf8b975ae8a10c87407a86c4439d8576500fd4f55850559dcf0ac2295",
      126,
      (
        10,
        "3090a2d8ecd78fd97ebbf4bad37066df3bcfb971fdabb4cef87e17b55347f9f2",
        1,
        "LhyLNfBkoKshT7R8Pce6vkB9T2cP2o84hx",
        9470530462L
      )
    ),
    (
      new NetParams {
        val hrp: String = "tltc"
        val pkhPrefix: Array[Byte] = hex"6F".toArray
        val shPrefix: Array[Byte] = hex"3A".toArray
      },
      "ltc_test_161005",
      "ffb9dae510ecf25f46415b879d8be68a483477526619ab6af2690c2ecd7e25de",
      3,
      (
        1,
        "4b4caf8a24569d7e2d2fbb3011bc61612fee7edfc2adb8d5707e78e2481b33f7",
        0,
        "QTJgpAiqtwVAjNdpjZrzhYVrxWXG8dVSQ2",
        9677200000L
      )
    )
  )
  "Block" must{
    "be decoded from raw data" in {
      forAll(testData){case (netP, resource, blockHash, transactionNumber, (txN, txId, outN, address, amount)) =>
        val maybeBlock = ByteVector.fromHex(Source.fromResource(resource).mkString).flatMap(data => Block.codec.decodeValue(data.bits).toOption)
        maybeBlock shouldBe a[Some[_]]
        val block = maybeBlock.get
        block.blockHeader.hash().toString shouldBe blockHash
        block.transactions.size shouldBe transactionNumber
        val tx = block.transactions(txN)
        tx.txID.toString shouldBe txId
        val output = tx.outputs(outN)
        output.value shouldBe amount
        output.scriptPubKey.address(netP) shouldBe Some(address)
      }
    }
  }
}
