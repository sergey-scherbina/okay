package okay2.codec

/** RFC 4648 base64 with padding (okay-codec's Base64.scala), one
 * source on every platform; damage is a `Left`, never a throw */
object Base64 {

  private val alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  private val reverse: Array[Int] = {
    val r = Array.fill(128)(-1)
    for (i <- alphabet.indices) r(alphabet(i).toInt) = i
    r
  }

  def encode(bs: Array[Byte]): String = {
    val out = new StringBuilder(((bs.length + 2) / 3) * 4)
    var i = 0
    while (i + 2 < bs.length) {
      val n = ((bs(i) & 0xFF) << 16) | ((bs(i + 1) & 0xFF) << 8) | (bs(i + 2) & 0xFF)
      out += alphabet((n >> 18) & 63)
      out += alphabet((n >> 12) & 63)
      out += alphabet((n >> 6) & 63)
      out += alphabet(n & 63)
      i += 3
    }
    bs.length - i match {
      case 1 =>
        val n = (bs(i) & 0xFF) << 16
        out += alphabet((n >> 18) & 63); out += alphabet((n >> 12) & 63)
        out += '='; out += '='
      case 2 =>
        val n = ((bs(i) & 0xFF) << 16) | ((bs(i + 1) & 0xFF) << 8)
        out += alphabet((n >> 18) & 63); out += alphabet((n >> 12) & 63)
        out += alphabet((n >> 6) & 63)
        out += '='
      case _ => ()
    }
    out.result()
  }

  def decode(s: String): Either[String, Array[Byte]] = {
    val body = s.reverse.dropWhile(_ == '=').reverse
    val pad = s.length - body.length
    if (s.length % 4 != 0 && s.nonEmpty) Left("base64: length is not a multiple of 4")
    else if (pad > 2) Left("base64: too much padding")
    else {
      val out = Array.ofDim[Byte](body.length * 3 / 4)
      var acc = 0
      var bits = 0
      var o = 0
      var bad = -1
      var i = 0
      while (i < body.length && bad < 0) {
        val c = body(i)
        val v = if (c.toInt < 128) reverse(c.toInt) else -1
        if (v < 0) bad = i
        else {
          acc = (acc << 6) | v
          bits += 6
          if (bits >= 8) {
            bits -= 8
            out(o) = ((acc >> bits) & 0xFF).toByte
            o += 1
          }
        }
        i += 1
      }
      if (bad >= 0) Left(s"base64: bad character at $bad")
      else Right(if (o == out.length) out else out.take(o))
    }
  }
}
