package jp.peg.lamport

import jp.peg.lamport.key._
import jp.peg.lamport.utils.CryptUtil

import scala.collection.{GenSeq, SeqLike, mutable}

/**
  * Created by h-kawayoke on 2017/11/30
  */

object Signature {
  def apply(value: Array[PrivateKey]): Signature = Signature(value.toSeq)
}

case class Signature(value: Seq[PrivateKey]) extends SeqLike[PrivateKey, Signature] with GenSeq[PrivateKey] {

  def apply(idx: Int): PrivateKey = value(idx)

  override def iterator: Iterator[PrivateKey] = value.iterator
  override def seq: Seq[PrivateKey] = value
  override def length: Int = value.length

  override protected[this] def newBuilder: mutable.Builder[PrivateKey, Signature] = new mutable.ReusableBuilder[PrivateKey, Signature] {
    val underlying = Seq.newBuilder[PrivateKey]
    def clear(): Unit = underlying.clear()
    def result(): Signature = Signature(underlying.result())
    def +=(elem: PrivateKey): this.type = {
      underlying += elem
      this
    }
  }

  def toByteString: ByteString = value.map(_.value).fold(ByteString.empty)(_ + _)
}

object LamportPrivKey {
  def init(): LamportPrivKey = {
    new LamportPrivKey((0 to 255).map(_ =>
      PrivateKeyPair(PrivateKey(CryptUtil.random(32)),PrivateKey(CryptUtil.random(32)))
    ))
  }
}

class LamportPrivKey(keys: Seq[PrivateKeyPair]) {
  require(keys.length == 256)

  def toPubKey = new LamportPubKey(keys.map(_.toPubKeyPair))

  def sign(message: ByteString): Signature = {
    Signature(message.toSha256.seq.flatMap(_.toBooleanSeq).zipWithIndex.map { case (isA, index) =>
      if(isA) keys(index).a else keys(index).b
    })
  }
}

class LamportPubKey(keys: Seq[PublicKeyPair]){
  def verify(sig: Signature, message: ByteString): Boolean = {
    message.toSha256.seq.flatMap(_.toBooleanSeq).zipWithIndex.map{ case (isA, index) =>
      sig(index).value.toSha256 == (if(isA) keys(index).a.value else keys(index).b.value)
    }.forall(identity)
  }
}