package jp.peg

/**
  * Created by h-kawayoke on 2017/11/30
  */
package object lamport {
  implicit class ExtendedString(value: String) {
    def toByteString: Seq[Byte] = {
      val h = if (value.contains(" ")) value.replace(" ", "") else value
      (if (h.length % 2 == 1) s"0$h" else h).grouped(2).map(Integer.parseInt(_, 16).toByte).toSeq
    }
  }
  implicit class ExtendedByte(value: Byte){
    def toBooleanSeq: Seq[Boolean] = {
      String.format("%8s", Integer.toBinaryString(value & 0xFF)).replace(' ', '0').map(binary =>
        if(binary == '0') false else true
      )
    }
  }

  implicit class ExtendedByteArray(value: Array[Byte]){
    def toByteString: ByteString = ByteString(value)
  }

  implicit class ExtendedByteSeq(value: Array[Byte]){
    def toByteString: ByteString = ByteString(value)
  }
}
