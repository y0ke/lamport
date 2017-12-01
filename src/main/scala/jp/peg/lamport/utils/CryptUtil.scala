package jp.peg.lamport.utils

import java.security.MessageDigest

import jp.peg.lamport.ByteString

import scala.util.Random

/**
  * Created by h-kawayoke on 2017/11/30
  */
object CryptUtil {
  def sha256(b: Array[Byte]): Array[Byte] = MessageDigest.getInstance("SHA-256").digest(b)

  def sha256(b: Seq[Byte]): Seq[Byte] = sha256(b.toArray)

  def hash256(b: Array[Byte]): Array[Byte] = sha256(sha256(b))

  def randomByte: Byte = Random.nextInt(256).toByte
  def random(length: Int): ByteString = ByteString(0.until(length).map(_ ⇒ randomByte).toArray)
}
