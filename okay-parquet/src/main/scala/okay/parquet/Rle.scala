package okay.parquet

/**
 * THE RLE / BIT-PACKED HYBRID (Parquet's encodings doc): runs, each a
 * varint header — even: an RLE run of `header >> 1` copies of one value
 * in `ceil(width / 8)` little-endian bytes; odd: `header >> 1` groups of
 * eight values bit-packed LSB first. Definition levels and dictionary
 * indices are written in it.
 */
object Rle:

  /** `n` values of `width` bits from `bytes[from, until)` */
  def decode(bytes: Array[Byte], from: Int, until: Int, width: Int, n: Int): Array[Int] =
    if width < 0 || width > 32 then throw Refused(s"a bit width of $width")
    val out = new Array[Int](n)
    if width == 0 then return out
    var at = from
    var o = 0
    def byte(): Int =
      if at >= until then throw Refused("RLE data cut short")
      val b = bytes(at) & 0xff
      at += 1
      b
    val bytesPer = (width + 7) / 8
    while o < n do
      var h = 0L
      var shift = 0
      var more = true
      while more do
        val b = byte()
        h |= (b & 0x7fL) << shift
        shift += 7
        more = (b & 0x80) != 0
        if shift > 35 then throw Refused("an RLE run header too long")
      if (h & 1) == 0 then
        val count = (h >>> 1).toInt
        var v = 0
        var k = 0
        while k < bytesPer do { v |= byte() << (8 * k); k += 1 }
        val take = math.min(count, n - o)
        java.util.Arrays.fill(out, o, o + take, v)
        o += take
      else
        val values = (h >>> 1).toInt * 8
        val needBytes = ((values.toLong * width + 7) / 8).toInt
        if at + needBytes > until then
          // a last group may be cut to the bytes its real values need
          if at >= until then throw Refused("RLE bit-packed run cut short")
        var bit = 0L
        val base = at
        var k = 0
        val mask = if width == 32 then -1 else (1 << width) - 1
        while k < values do
          var v = 0L
          var got = 0
          while got < width do
            val idx = base + ((bit + got) >>> 3).toInt
            val b = if idx < until then bytes(idx) & 0xff else 0
            val off = ((bit + got) & 7).toInt
            val take = math.min(8 - off, width - got)
            v |= ((b >>> off) & ((1 << take) - 1)).toLong << got
            got += take
          if o < n then { out(o) = (v.toInt & mask); o += 1 }
          bit += width
          k += 1
        at = math.min(until, base + needBytes)
    out

  /** `values` of `width` bits, as RLE runs where a value repeats and
   * bit-packed groups where it does not */
  def encode(values: Array[Int], width: Int): Array[Byte] =
    val out = java.io.ByteArrayOutputStream()
    def varint(v: Long): Unit =
      var x = v
      while (x & ~0x7fL) != 0 do { out.write(((x & 0x7f) | 0x80).toInt); x >>>= 7 }
      out.write(x.toInt)
    val bytesPer = (width + 7) / 8
    val n = values.length
    var i = 0
    while i < n do
      var j = i + 1
      while j < n && values(j) == values(i) do j += 1
      if j - i >= 8 || j == n then
        // a run (and the tail, whatever its length, as a run each)
        varint((j - i).toLong << 1)
        var k = 0
        while k < bytesPer do { out.write((values(i) >>> (8 * k)) & 0xff); k += 1 }
        i = j
      else
        // bit-pack groups of eight until a run of eight begins
        var end = i
        var stop = false
        while !stop && end < n do
          var r = end + 1
          while r < n && values(r) == values(end) do r += 1
          if r - end >= 8 then stop = true else end = r
        val groups = math.max(1, (end - i + 7) / 8)
        varint((groups.toLong << 1) | 1)
        val bits = new Array[Byte]((groups * 8 * width + 7) / 8)
        var k = 0
        while k < groups * 8 do
          val v = if i + k < n then values(i + k) else 0
          var b = 0
          while b < width do
            if ((v >>> b) & 1) != 0 then
              val pos = k * width + b
              bits(pos >>> 3) = (bits(pos >>> 3) | (1 << (pos & 7))).toByte
            b += 1
          k += 1
        out.write(bits)
        i = math.min(n, i + groups * 8)
    out.toByteArray

  /** the bits a value up to `max` needs */
  def width(max: Int): Int = 32 - Integer.numberOfLeadingZeros(max)
