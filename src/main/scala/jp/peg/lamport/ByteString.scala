package jp.peg.lamport

import jp.peg.lamport.utils.CryptUtil

import scala.collection.{GenSeq, SeqLike, mutable}
/**
  * Created by h-kawayoke on 2017/11/30
  */
object ByteString {
  def hex2bytes(hex: String): Seq[Byte] = {
    val h = if (hex.contains(" ")) hex.replace(" ", "") else hex
    (if (h.length % 2 == 1) s"0$h" else h).grouped(2).map(Integer.parseInt(_, 16).toByte).toSeq
  }

  def apply(value: String): ByteString = ByteString(hex2bytes(value))

  def apply(value: Array[Byte]): ByteString = ByteString(value.toSeq)

  val empty: ByteString = ByteString(Nil)
}

case class ByteString(value: Seq[Byte]) extends SeqLike[Byte, ByteString] with GenSeq[Byte] {

  override def iterator: Iterator[Byte] = value.iterator

  override def seq: Seq[Byte] = value

  override def length: Int = value.length

  def apply(idx: Int): Byte = value(idx)

  override protected[this] def newBuilder: mutable.Builder[Byte, ByteString] = new mutable.ReusableBuilder[Byte, ByteString] {
    val underlying = Seq.newBuilder[Byte]
    def clear(): Unit = underlying.clear()
    def result(): ByteString = ByteString(underlying.result())
    def +=(elem: Byte): this.type = {
      underlying += elem
      this
    }
  }

  def +(b: ByteString): ByteString = ByteString(value ++ b.value)

  private def lengthAsDecimal: Int = Math.ceil(Math.log10(Math.pow(2, value.length * 8))).toInt

  def toSha256: ByteString = ByteString(CryptUtil.sha256(value))

  def toString(n: Int = 16, includeLeadingZero: Boolean = true): String = {
    n match {
      case 16 if includeLeadingZero ⇒ value.map("%02x".format(_)).mkString
      case 16 ⇒
        val res = value.map { "%02x".format(_) }.mkString.dropWhile(_ == '0')
        if (res.isEmpty) "0" else res
      case 10 if includeLeadingZero ⇒
        val num = BigInt(toString(16, false), 16).toString()
        if (num.length < lengthAsDecimal) s"${"0" * (lengthAsDecimal - num.length)}$num" else num
      case 10 ⇒ BigInt(toString(16, false), 16).toString()
      case 2  ⇒
        val empty = "0" * 8
        if (includeLeadingZero) value.map { x ⇒ (empty + (x & 0xFF).toBinaryString).takeRight(8) }.mkString
        else value.map { x ⇒ (empty + (x & 0xFF).toBinaryString).takeRight(8) }.mkString.dropWhile(_ == '0')
    }
  }

  def toAsciiString = new String(value.toArray, "US-ASCII")
}
