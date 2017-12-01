package jp.peg.lamport.key

import jp.peg.lamport.ByteString
import jp.peg.lamport.utils.CryptUtil

/**
  * Created by h-kawayoke on 2017/11/30
  */

abstract class Key(isPublicKey: Boolean) {
  def isPubKey: Boolean = isPublicKey
  def value: ByteString
  override def toString: String = value.toString()
}

object PrivateKey {
  def init(): PrivateKey = PrivateKey(CryptUtil.random(256))
}

case class PrivateKey(value: ByteString) extends Key(false) {

  def toPublicKey: PublicKey = PublicKey(value.toSha256)

}

case class PublicKey(value: ByteString) extends Key(true) {

}

trait KeyPair

case class PrivateKeyPair(a: PrivateKey,b: PrivateKey) extends KeyPair {
  def toPubKeyPair = PublicKeyPair(PublicKey(a.value.toSha256),PublicKey(b.value.toSha256))
}

case class PublicKeyPair(a: PublicKey, b: PublicKey) extends KeyPair
