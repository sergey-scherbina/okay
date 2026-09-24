package okay.py

import java.nio.charset.StandardCharsets.UTF_8
import okay.codec.Json

/**
 * How a wire message's tree becomes bytes (polyglot-one-wire stage 5a), chosen
 * at COMPILE TIME by the given in scope where a worker is opened:
 *
 * {{{
 * import okay.py.WireFormat.Cbor.given        // CBOR instead of JSON
 * import okay.py.WireCompression.Deflate.given // raw DEFLATE on every message
 * val w = ForeignWorker.speaking(Seq(binary))  // negotiated at the handshake
 * }}}
 *
 * With neither import the defaults (JSON lines, no compression) apply, and
 * the wire is what it always was. The far side announces what it speaks in
 * its handshake; a choice it did not announce is refused by name, never
 * quietly downgraded.
 */
trait WireFormat:
  def name: String
  def encode(tree: Json): Array[Byte]
  def decode(bytes: Array[Byte]): Json

object WireFormat:
  /** JSON, the default */
  given json: WireFormat = new WireFormat:
    def name = "json"
    def encode(tree: Json): Array[Byte] = Json.print(tree).getBytes(UTF_8)
    def decode(bytes: Array[Byte]): Json = ForeignWorker.whole(String(bytes, UTF_8))

  /** `import okay.py.WireFormat.Cbor.given` */
  object Cbor:
    given cbor: WireFormat = new WireFormat:
      def name = "cbor"
      def encode(tree: Json): Array[Byte] = WireCbor.encode(tree)
      def decode(bytes: Array[Byte]): Json = WireCbor.decode(bytes).fold(why => throw IllegalStateException(why), identity)

trait WireCompression:
  def name: String
  def compress(bytes: Array[Byte]): Array[Byte]
  def decompress(bytes: Array[Byte]): Array[Byte]

object WireCompression:
  /** no compression, the default */
  given none: WireCompression = new WireCompression:
    def name = "none"
    def compress(bytes: Array[Byte]): Array[Byte] = bytes
    def decompress(bytes: Array[Byte]): Array[Byte] = bytes

  /** `import okay.py.WireCompression.Deflate.given`: raw DEFLATE (RFC 1951),
   * which every far side's standard library has (zlib's `wbits=-15`, Go's
   * compress/flate, Node's inflateRaw, Rust's flate2) */
  object Deflate:
    given deflate: WireCompression = new WireCompression:
      def name = "deflate"
      def compress(bytes: Array[Byte]): Array[Byte] =
        val d = java.util.zip.Deflater(java.util.zip.Deflater.DEFAULT_COMPRESSION, true)
        try
          d.setInput(bytes)
          d.finish()
          val out = java.io.ByteArrayOutputStream()
          val buf = new Array[Byte](8192)
          while !d.finished() do out.write(buf, 0, d.deflate(buf))
          out.toByteArray
        finally d.end()
      def decompress(bytes: Array[Byte]): Array[Byte] =
        val i = java.util.zip.Inflater(true)
        try
          i.setInput(bytes)
          val out = java.io.ByteArrayOutputStream()
          val buf = new Array[Byte](8192)
          while !i.finished() do
            val n = i.inflate(buf)
            if n == 0 && (i.needsInput() || i.needsDictionary()) then
              throw IllegalStateException("a DEFLATE message ended before its data did (cut short?)")
            out.write(buf, 0, n)
          out.toByteArray
        finally i.end()

/**
 * The wire's protocol tree as CBOR (RFC 8949), the subset stage 5a names:
 * integers, float16/32/64, text, definite arrays and maps, true, false,
 * null, undefined (read as null). Anything else is refused by name.
 */
object WireCbor:

  def encode(tree: Json): Array[Byte] =
    val out = java.io.ByteArrayOutputStream()
    def head(major: Int, n: Long): Unit =
      val m = major << 5
      if n < 24 then out.write(m | n.toInt)
      else if n < 0x100 then { out.write(m | 24); out.write(n.toInt) }
      else if n < 0x10000 then { out.write(m | 25); out.write((n >> 8).toInt); out.write(n.toInt & 0xff) }
      else if n < 0x100000000L then { out.write(m | 26); (3 to 0 by -1).foreach(i => out.write(((n >> (8 * i)) & 0xff).toInt)) }
      else { out.write(m | 27); (7 to 0 by -1).foreach(i => out.write(((n >> (8 * i)) & 0xff).toInt)) }
    def go(j: Json): Unit = j match
      case Json.JNull | Json.JErr(_) => out.write(0xf6)
      case Json.JBool(b) => out.write(if b then 0xf5 else 0xf4)
      case Json.JNum(v) if v.isWhole && math.abs(v) < 9.0e18 =>
        val n = v.toLong
        if n >= 0 then head(0, n) else head(1, -1 - n)
      case Json.JNum(v) =>
        out.write(0xfb)
        val bits = java.lang.Double.doubleToLongBits(v)
        (7 to 0 by -1).foreach(i => out.write(((bits >> (8 * i)) & 0xff).toInt))
      case Json.JStr(s) =>
        val b = s.getBytes(UTF_8)
        head(3, b.length.toLong)
        out.write(b)
      case Json.JArr(vs) =>
        head(4, vs.length.toLong)
        vs.foreach(go)
      case Json.JObj(fs) =>
        head(5, fs.length.toLong)
        fs.foreach { (k, v) => go(Json.JStr(k)); go(v) }
    go(tree)
    out.toByteArray

  def decode(bytes: Array[Byte]): Either[String, Json] =
    var at = 0
    def byte(): Int =
      if at >= bytes.length then throw IllegalStateException("a CBOR message ended early (cut short?)")
      val b = bytes(at) & 0xff
      at += 1
      b
    def uint(n: Int): Long = (0 until n).foldLeft(0L)((acc, _) => (acc << 8) | byte())
    def arg(info: Int): Long = info match
      case i if i < 24 => i.toLong
      case 24 => uint(1)
      case 25 => uint(2)
      case 26 => uint(4)
      case 27 => uint(8)
      case other => throw IllegalStateException(s"CBOR: an indefinite or reserved length ($other) is not in the wire's subset")
    def half(h: Int): Double =
      val exp = (h >> 10) & 0x1f
      val mant = h & 0x3ff
      val v = if exp == 0 then mant * math.pow(2, -24)
        else if exp != 31 then (mant + 1024) * math.pow(2, exp - 25)
        else if mant == 0 then Double.PositiveInfinity else Double.NaN
      if (h & 0x8000) != 0 then -v else v
    def item(): Json =
      val ib = byte()
      val major = ib >> 5
      val info = ib & 0x1f
      major match
        case 0 => Json.JNum(arg(info).toDouble)
        case 1 => Json.JNum((-1 - arg(info)).toDouble)
        case 3 =>
          val n = arg(info).toInt
          if at + n > bytes.length then throw IllegalStateException("a CBOR string ended early (cut short?)")
          val s = String(bytes, at, n, UTF_8)
          at += n
          Json.JStr(s)
        case 4 => Json.JArr(Vector.fill(arg(info).toInt)(item()))
        case 5 => Json.JObj(Vector.fill(arg(info).toInt) {
          item() match
            case Json.JStr(k) => (k, item())
            case other => throw IllegalStateException(s"CBOR: a map key that is not text: $other")
        })
        case 7 => info match
          case 20 => Json.JBool(false)
          case 21 => Json.JBool(true)
          case 22 | 23 => Json.JNull
          case 25 => Json.JNum(half(uint(2).toInt))
          case 26 => Json.JNum(java.lang.Float.intBitsToFloat(uint(4).toInt).toDouble)
          case 27 => Json.JNum(java.lang.Double.longBitsToDouble(uint(8)))
          case other => throw IllegalStateException(s"CBOR: simple value $other is not in the wire's subset")
        case other => throw IllegalStateException(s"CBOR: major type $other (byte strings, tags) is not in the wire's subset")
    try
      val j = item()
      if at != bytes.length then Left(s"CBOR: ${bytes.length - at} bytes after the message") else Right(j)
    catch case e: IllegalStateException => Left(e.getMessage)
