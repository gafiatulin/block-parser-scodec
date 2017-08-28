package com.github.gafiatulin.blockchain.protocol

import com.github.gafiatulin.blockchain.util.{Base58Check, Bech32, HashUtil, NetParams}
import scodec.bits.{BitVector, ByteVector, HexStringSyntax}
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err}

sealed trait ScriptPubKey{ self =>
  type T >: self.type <: ScriptPubKey
  def companion: ScriptPubKeyCompanion[T]
  def instance: T = self
  def address(netParams: NetParams): Option[String]
}

sealed trait ScriptPubKeyCompanion[T <: ScriptPubKey]{
  def codec: Codec[T]
}

final case class Pay2PublicKeyHash(publicKeyHash: ByteVector) extends ScriptPubKey{
  type T = Pay2PublicKeyHash
  override def address(netParams: NetParams): Option[String] = Some(Base58Check.encode(netParams.pkhPrefix, publicKeyHash.toArray))
  override def companion: ScriptPubKeyCompanion[Pay2PublicKeyHash] = Pay2PublicKeyHash
}

case object Pay2PublicKeyHash extends ScriptPubKeyCompanion[Pay2PublicKeyHash]{
  final implicit val codec: Codec[Pay2PublicKeyHash] = (
    ("OP_DUP" | constant(hex"76")) :~>:
    ScriptPubKey.CommonScriptCodecs.OP_HASH160 :~>:
    ScriptPubKey.CommonScriptCodecs.PUSH_N_BYTE(20) :~>:
    ScriptPubKey.CommonScriptCodecs.HASH(20) <~
    ScriptPubKey.CommonScriptCodecs.OP_EQUALVERIFY <~
    ScriptPubKey.CommonScriptCodecs.OP_CHECK_SIG
  ).as[Pay2PublicKeyHash]
}

final case class Pay2ScriptHash(scriptHash: ByteVector) extends ScriptPubKey{
  type T = Pay2ScriptHash
  override def address(netParams: NetParams): Option[String] = Some(Base58Check.encode(netParams.shPrefix, scriptHash.toArray))
  override def companion: ScriptPubKeyCompanion[Pay2ScriptHash] = Pay2ScriptHash
}

case object Pay2ScriptHash extends ScriptPubKeyCompanion[Pay2ScriptHash]{
  final implicit val codec: Codec[Pay2ScriptHash] = (
      ScriptPubKey.CommonScriptCodecs.OP_HASH160 :~>:
      ScriptPubKey.CommonScriptCodecs.PUSH_N_BYTE(20) :~>:
      ScriptPubKey.CommonScriptCodecs.HASH(20) <~
      ("OP_EQUAL" | constant(hex"87"))
    ).as[Pay2ScriptHash]
}

final case class Pay2PublicKey(publicKey: ByteVector) extends ScriptPubKey{
  require(publicKey.size == 33L || publicKey.size == 65L)
  type T = Pay2PublicKey
  override def address(netParams: NetParams): Option[String] = Some(Base58Check.encode(netParams.pkhPrefix, HashUtil.hash160(publicKey.toArray)))
  override def companion: ScriptPubKeyCompanion[Pay2PublicKey] = Pay2PublicKey
}

case object Pay2PublicKey extends ScriptPubKeyCompanion[Pay2PublicKey]{
  final implicit val codec: Codec[Pay2PublicKey] =
    (variableSizeBytes(int8, bytes) :: ScriptPubKey.CommonScriptCodecs.OP_CHECK_SIG).as[Pay2PublicKey]
}

final case class MultiSiqMOfN(m: Int, publicKeys: List[ByteVector], n: Int) extends ScriptPubKey{
  require(m >= 2 && m <= 16)
  type T = MultiSiqMOfN
  override def address(netParams: NetParams): Option[String] = Some(MultiSiqMOfN.codec.encode(this).toOption.fold("")(bits => Base58Check.encode(netParams.shPrefix, HashUtil.hash160(bits.toByteArray))))
  override def companion: ScriptPubKeyCompanion[MultiSiqMOfN] = MultiSiqMOfN
}

case object MultiSiqMOfN extends ScriptPubKeyCompanion[MultiSiqMOfN]{
  private final val encoder = Encoder[MultiSiqMOfN](
    (ms: MultiSiqMOfN) => for{
      m <- ScriptPubKey.CommonScriptCodecs.OP_N_NONZERO.encode(ms.m)
      keys <- list(variableSizeBytes(int8, bytes)).encode(ms.publicKeys)
      n <- ScriptPubKey.CommonScriptCodecs.OP_N_NONZERO.encode(ms.n)
    } yield m ++ keys ++ n ++ hex"ae".bits
  )

  private final val decoder = Decoder[MultiSiqMOfN](
    (bits: BitVector) => {
      val (mAndKeysBits, nAndCheckBits) = bits.splitAt(bits.size - 2 * 8)
      val mAndKCodec = (("m" | ScriptPubKey.CommonScriptCodecs.OP_N_NONZERO) :: ("publicKeys" | list(variableSizeBytes(int8, bytes)))).as[(Int, List[ByteVector])]
      val nAndCheckCodec = ("numberOfPublicKeys" | ScriptPubKey.CommonScriptCodecs.OP_N_NONZERO) <~ ("OP_CHECKMULTISIG" | constant(hex"ae"))
      mAndKCodec.decode(mAndKeysBits).flatMap{
        case dr if dr.remainder.isEmpty =>
          nAndCheckCodec.decode(nAndCheckBits).map(drN => DecodeResult(MultiSiqMOfN(dr.value._1, dr.value._2, drN.value), drN.remainder))
        case dr => Attempt.failure(Err(s"Didn't fully consume m and publicKeys. Remainder hex: ${dr.remainder.toHex}"))
      }
    }
  )

  final implicit val codec: Codec[MultiSiqMOfN] = Codec[MultiSiqMOfN](encoder, decoder)
}

sealed trait WitnessScriptPubKey extends ScriptPubKey{
  def witnessVersion: Int
  protected def bech32Address(hrp: String, bv: ByteVector): String =
    Bech32.encode(hrp, witnessVersion.toByte +: Bech32.to5Bit(bv.toArray)).getOrElse("")
}

final case class Pay2WitnessPublicKeyHash(witnessVersion: Int, publicKeyHash: ByteVector) extends WitnessScriptPubKey{
  type T = Pay2WitnessPublicKeyHash
  override def companion: ScriptPubKeyCompanion[Pay2WitnessPublicKeyHash] = Pay2WitnessPublicKeyHash
  override def address(netParams: NetParams): Option[String] = Some(bech32Address(netParams.hrp, publicKeyHash))
}

case object Pay2WitnessPublicKeyHash extends ScriptPubKeyCompanion[Pay2WitnessPublicKeyHash]{
  final implicit val codec: Codec[Pay2WitnessPublicKeyHash] = (
    ("witness version" | ScriptPubKey.CommonScriptCodecs.OP_N) ::
    ScriptPubKey.CommonScriptCodecs.PUSH_N_BYTE(20) :~>:
    ScriptPubKey.CommonScriptCodecs.HASH(20)
  ).as[Pay2WitnessPublicKeyHash]
}

final case class Pay2WitnessScriptHash(witnessVersion: Int, scriptHash: ByteVector) extends WitnessScriptPubKey{
  type T = Pay2WitnessScriptHash
  override def companion: ScriptPubKeyCompanion[Pay2WitnessScriptHash] = Pay2WitnessScriptHash
  override def address(netParams: NetParams): Option[String] = Some(bech32Address(netParams.hrp, scriptHash))
}

case object Pay2WitnessScriptHash extends ScriptPubKeyCompanion[Pay2WitnessScriptHash]{
  final implicit val codec: Codec[Pay2WitnessScriptHash] = (
    ("witness version" | ScriptPubKey.CommonScriptCodecs.OP_N) ::
    ScriptPubKey.CommonScriptCodecs.PUSH_N_BYTE(32) :~>:
    ScriptPubKey.CommonScriptCodecs.HASH(32)
  ).as[Pay2WitnessScriptHash]
}

final case class OtherScriptPubKey(data: ByteVector) extends ScriptPubKey {
  type T = OtherScriptPubKey
  override def address(netParams: NetParams): Option[String] = None
  override def companion: ScriptPubKeyCompanion[OtherScriptPubKey] = OtherScriptPubKey
}

case object OtherScriptPubKey extends ScriptPubKeyCompanion[OtherScriptPubKey]{
  final implicit val codec: Codec[OtherScriptPubKey] = bytes.as[OtherScriptPubKey]
}

case object ScriptPubKey{
  case object CommonScriptCodecs{
    private final val OP_RESERVED: Int = hex"50".toInt()
    private final val OP_NOP: Int = hex"61".toInt()
    val OP_ZERO: Codec[Int] = constant(hex"00").xmap(_ => 0, {case 0 => ()})
    val OP_N_NONZERO: Codec[Int] = "OP_N" | int8.exmapc{
      case opN: Int if opN > OP_RESERVED && opN < OP_NOP => Attempt.successful(opN - OP_RESERVED)
      case opN: Int => Attempt.failure(Err(s"OP_N is limited to [1..16], hex: [51..60], got hex: ${BitVector.fromInt(opN, 8).toHex}"))
    }{
      case n: Int if n > 0 && n <= 16 => Attempt.successful(n + OP_RESERVED)
      case n: Int => Attempt.failure(Err(s"OP_N is limited to [1..16], hex: [51..60], got n: $n"))
    }
    val OP_N: Codec[Int] = choice(OP_N_NONZERO, OP_ZERO)
    val OP_HASH160: Codec[Unit] = "OP_HASH160" | constant(hex"a9")
    val OP_CHECK_SIG: Codec[Unit] = "OP_CHECK_SIG" | constant(hex"ac")
    def PUSH_N_BYTE(n: Int): Codec[Unit] = s"$n" | constant(n)
    def HASH(size: Int): Codec[ByteVector] = "hash" | bytes(size)
    val OP_EQUALVERIFY: Codec[Unit] = "OP_EQUALVERIFY" | constant(hex"88")
  }

  final implicit val codec: Codec[ScriptPubKey] =
    Codec[ScriptPubKey](
      Encoder[ScriptPubKey]((scp: ScriptPubKey) => scp.companion.codec.encode(scp.instance)),
      Decoder.choiceDecoder(
        Codec[Pay2PublicKeyHash],
        Codec[Pay2ScriptHash],
        Codec[Pay2PublicKey],
        Codec[MultiSiqMOfN],
        Codec[Pay2WitnessPublicKeyHash],
        Codec[Pay2WitnessScriptHash],
        Codec[OtherScriptPubKey]
      )
    )

}