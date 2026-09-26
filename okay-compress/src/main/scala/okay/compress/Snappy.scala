package okay.compress

/**
 * SNAPPY, THE RAW FORMAT (specs/okay-compress.md, stage 6): a varint of
 * the uncompressed length, then elements — a LITERAL (its bytes follow)
 * or a COPY of earlier output (a length and an offset back, in 1, 2 or 4
 * bytes). No framing, no checksum: what a Parquet page and an Avro block
 * hold. Every platform.
 *
 * The compressor is greedy over a hash table of 4-byte sequences, the
 * reference's own strategy; what it writes any Snappy reads. The
 * decoder takes nothing on trust: a copy reaching before the start, an
 * element running past the input, or output longer or shorter than the
 * declared length is refused as `Corrupt`, never an index error.
 */
object Snappy extends Codec:
  def name = "snappy"

  private val HashBits = 14
  private val MinMatch = 4
  private val MaxCopy = 64

  def compress(in: Array[Byte]): Array[Byte] =
    val n = in.length
    val out = java.io.ByteArrayOutputStream(n / 2 + 16)
    varint(out, n)
    val table = Array.fill(1 << HashBits)(-1)
    var lit = 0                 // where the pending literal starts
    var i = 0
    while i + MinMatch <= n do
      val h = hash(Le.i32(in, i))
      val cand = table(h)
      table(h) = i
      if cand >= 0 && Le.i32(in, cand) == Le.i32(in, i) then
        var len = MinMatch
        while i + len < n && in(cand + len) == in(i + len) do len += 1
        literal(out, in, lit, i - lit)
        copy(out, i - cand, len)
        i += len
        lit = i
      else i += 1
    literal(out, in, lit, n - lit)
    out.toByteArray

  private def hash(v: Int): Int = (v * 0x1e35a7bd) >>> (32 - HashBits)

  private def varint(out: java.io.ByteArrayOutputStream, v: Int): Unit =
    var x = v
    while (x & ~0x7f) != 0 do
      out.write((x & 0x7f) | 0x80)
      x >>>= 7
    out.write(x)

  private def literal(out: java.io.ByteArrayOutputStream, in: Array[Byte], from: Int, len: Int): Unit =
    if len > 0 then
      val n = len - 1
      if n < 60 then out.write(n << 2)
      else if n < (1 << 8) then { out.write(60 << 2); out.write(n) }
      else if n < (1 << 16) then { out.write(61 << 2); out.write(n); out.write(n >>> 8) }
      else if n < (1 << 24) then { out.write(62 << 2); out.write(n); out.write(n >>> 8); out.write(n >>> 16) }
      else { out.write(63 << 2); out.write(n); out.write(n >>> 8); out.write(n >>> 16); out.write(n >>> 24) }
      out.write(in, from, len)

  /** a copy of `len` bytes from `offset` back, as elements of at most 64 */
  private def copy(out: java.io.ByteArrayOutputStream, offset: Int, len: Int): Unit =
    var left = len
    while left > 0 do
      // never leave a remainder under 4: a 1-byte-offset copy needs 4..11
      val take = if left > MaxCopy && left - MaxCopy < MinMatch then left - MinMatch else math.min(left, MaxCopy)
      if take >= 4 && take <= 11 && offset < 2048 then
        out.write(1 | ((take - 4) << 2) | ((offset >>> 8) << 5))
        out.write(offset & 0xff)
      else if offset < 65536 then
        out.write(2 | ((take - 1) << 2)); out.write(offset); out.write(offset >>> 8)
      else
        out.write(3 | ((take - 1) << 2))
        out.write(offset); out.write(offset >>> 8); out.write(offset >>> 16); out.write(offset >>> 24)
      left -= take

  def decompress(in: Array[Byte]): Array[Byte] =
    var at = 0
    var size = 0L
    var shift = 0
    var more = true
    while more do
      if at >= in.length || shift > 28 then throw Corrupt("a Snappy block cut inside its length")
      val b = in(at) & 0xff
      at += 1
      size |= (b & 0x7fL) << shift
      shift += 7
      more = (b & 0x80) != 0
    if size > Int.MaxValue - 16 then throw Corrupt(s"a Snappy block declaring $size bytes")
    val out = new Array[Byte](size.toInt)
    var o = 0
    def need(k: Int): Unit = if at + k > in.length then throw Corrupt("a Snappy element cut short")
    while at < in.length do
      val tag = in(at) & 0xff
      at += 1
      (tag & 3) match
        case 0 =>
          var len = (tag >>> 2) + 1
          if len > 60 then
            val extra = len - 60
            need(extra)
            var v = 0
            var k = 0
            while k < extra do { v |= (in(at + k) & 0xff) << (8 * k); k += 1 }
            at += extra
            len = v + 1
            if len <= 0 then throw Corrupt("a Snappy literal of impossible length")
          need(len)
          if o + len > out.length then throw Corrupt("a Snappy literal past the declared length")
          System.arraycopy(in, at, out, o, len)
          at += len
          o += len
        case kind =>
          val (len, offset) = kind match
            case 1 =>
              need(1)
              val r = (((tag >>> 2) & 7) + 4, ((tag >>> 5) << 8) | (in(at) & 0xff))
              at += 1; r
            case 2 =>
              need(2)
              val r = ((tag >>> 2) + 1, (in(at) & 0xff) | ((in(at + 1) & 0xff) << 8))
              at += 2; r
            case _ =>
              need(4)
              val r = ((tag >>> 2) + 1, Le.i32(in, at))
              at += 4; r
          if offset <= 0 || offset > o then throw Corrupt(s"a Snappy copy $offset back, at byte $o")
          if o + len > out.length then throw Corrupt("a Snappy copy past the declared length")
          // byte by byte: a copy may overlap what it writes (a run)
          var k = 0
          while k < len do { out(o + k) = out(o - offset + k); k += 1 }
          o += len
    if o != out.length then throw Corrupt(s"a Snappy block of $o bytes declaring ${out.length}")
    out
