package com.github.gafiatulin.blockchain.protocol

import com.github.gafiatulin.blockchain.util.HashUtil
import scodec.bits.{BitVector, HexStringSyntax}
import scodec.codecs._
import scodec.{Attempt, Codec, Decoder, Encoder}
import shapeless.{::, HNil}

sealed trait Transaction{
  def version: Long
  def inputs: List[TransactionInput]
  def outputs: List[TransactionOutput]
  def lockTime: Long
  def txID: Hash
}

final case class BaseTransaction(
  version: Long,
  inputs: List[TransactionInput],
  outputs: List[TransactionOutput],
  lockTime: Long
) extends Transaction{
  override def txID: Hash = BaseTransaction.codec.encode(this).toOption.fold(Hash.nil(32))(bv => HashUtil.toHash(HashUtil.hash256(bv.toByteArray)))
}

case object BaseTransaction{
  final implicit val codec: Codec[BaseTransaction] = (
      ("version" | uint32L) ::
      ("inputs" | VariableSize.varList(Codec[TransactionInput])) ::
      ("outputs" | VariableSize.varList(Codec[TransactionOutput])) ::
      ("lockTime" | uint32L)
    ).as[BaseTransaction]
}

final case class WitnessTransaction(
  version: Long,
  inputs: List[TransactionInput],
  outputs: List[TransactionOutput],
  witnesses: List[ScriptWitness],
  lockTime: Long
) extends Transaction{
  def asBase: BaseTransaction = BaseTransaction(version, inputs, outputs, lockTime)
  override def txID: Hash = asBase.txID
  def wTxId: Hash = WitnessTransaction.codec.encode(this).toOption.fold(Hash.nil(32))(bv => HashUtil.toHash(HashUtil.hash256(bv.toByteArray)))
}

case object WitnessTransaction{
    final implicit val codec: Codec[WitnessTransaction] = (
      ("version" | uint32L) ::
      constant(hex"00") :~>:
      constant(hex"01") :~>:
      VarInt.varIntCodec.flatZip((inputsNumber: Long) =>
        ("inputs" | listOfN(provide(inputsNumber.toInt), Codec[TransactionInput])) ::
        ("outputs" | VariableSize.varList(Codec[TransactionOutput])) ::
        ("witnesses" | listOfN(provide(inputsNumber.toInt), Codec[ScriptWitness])) ::
        ("lockTime" | uint32L)
      ).xmap[List[TransactionInput] :: List[TransactionOutput] :: List[ScriptWitness] :: Long :: HNil](_._2, hl => hl.head.size.toLong -> hl)
    ).as[WitnessTransaction]
}

case object Transaction{
  private final val t2AttemptPF: PartialFunction[Transaction, Attempt[BitVector]] = {
    case wt: WitnessTransaction => WitnessTransaction.codec.encode(wt)
    case bt: BaseTransaction => BaseTransaction.codec.encode(bt)
  }
  final implicit val codec: Codec[Transaction] =
    Codec[Transaction](Encoder[Transaction](t2AttemptPF), Decoder.choiceDecoder(Codec[WitnessTransaction], Codec[BaseTransaction]))
}