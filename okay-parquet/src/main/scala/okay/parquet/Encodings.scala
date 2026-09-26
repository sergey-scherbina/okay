package okay.parquet

/**
 * THE DELTA ENCODINGS AND BYTE_STREAM_SPLIT, read (specs/parquet.md,
 * stage 2) — what Parquet's v2 writers use for sorted integers and for
 * strings that share prefixes, and pyarrow writes on request.
 */
private[parquet] object Encodings:

  /** a varint's value and where it ended */
  private def uvarint(b: Array[Byte], at0: Int, until: Int): (Long, Int) =
    var at = at0
    var v = 0L
    var shift = 0
    var more = true
    while more do
      if at >= until || shift > 63 then throw Refused("a DELTA header cut short")
      val x = b(at) & 0xff
      at += 1
      v |= (x & 0x7fL) << shift
      shift += 7
      more = (x & 0x80) != 0
    (v, at)

  private def zigzag(v: Long): Long = (v >>> 1) ^ -(v & 1)

  /**
   * DELTA_BINARY_PACKED: `n` integers (the header's own count may be
   * larger, a page's values are the first `n`), and where the encoding
   * ended — the byte-array encodings put data after it. `bits32` wraps
   * sums as INT32 does.
   */
  def deltaInts(b: Array[Byte], from: Int, until: Int, bits32: Boolean): (Array[Long], Int) =
    val (blockSize, a1) = uvarint(b, from, until)
    val (miniBlocks, a2) = uvarint(b, a1, until)
    val (total, a3) = uvarint(b, a2, until)
    val (first, a4) = uvarint(b, a3, until)
    if blockSize <= 0 || miniBlocks <= 0 || blockSize % miniBlocks != 0 || (blockSize / miniBlocks) % 8 != 0 then
      throw Refused(s"a DELTA_BINARY_PACKED header of block $blockSize in $miniBlocks miniblocks")
    if total < 0 || total > Int.MaxValue then throw Refused(s"a DELTA_BINARY_PACKED run of $total values")
    val perMini = (blockSize / miniBlocks).toInt
    val out = new Array[Long](total.toInt)
    def wrap(v: Long): Long = if bits32 then v.toInt.toLong else v
    var at = a4
    var n = 0
    if total > 0 then { out(0) = wrap(zigzag(first)); n = 1 }
    while n < total do
      val (minDelta, a5) = uvarint(b, at, until)
      at = a5
      val min = zigzag(minDelta)
      if at + miniBlocks > until then throw Refused("a DELTA_BINARY_PACKED block cut short")
      val widths = Array.tabulate(miniBlocks.toInt)(k => b(at + k) & 0xff)
      at += miniBlocks.toInt
      var m = 0
      while m < widths.length && n < total do
        val w = widths(m)
        if w > 64 then throw Refused(s"a DELTA_BINARY_PACKED miniblock of width $w")
        val bytes = (perMini * w + 7) / 8
        var k = 0
        while k < perMini do
          var v = 0L
          var got = 0
          val bit0 = k.toLong * w
          while got < w do
            val idx = at + ((bit0 + got) >>> 3).toInt
            val byte = if idx < until then b(idx) & 0xff else 0
            val off = ((bit0 + got) & 7).toInt
            val take = math.min(8 - off, w - got)
            v |= ((byte >>> off) & ((1 << take) - 1)).toLong << got
            got += take
          if n < total then
            out(n) = wrap(out(n - 1) + min + v)
            n += 1
          k += 1
        at += bytes
        m += 1
    (out, math.min(at, until))

  /** DELTA_LENGTH_BYTE_ARRAY: lengths DELTA-packed, then the bytes */
  def deltaLengths(b: Array[Byte], from: Int, until: Int, n: Int): Array[Array[Byte]] =
    val (lengths, a) = deltaInts(b, from, until, bits32 = true)
    if lengths.length < n then throw Refused(s"DELTA_LENGTH_BYTE_ARRAY with ${lengths.length} lengths for $n values")
    var at = a
    Array.tabulate(n) { i =>
      val len = lengths(i).toInt
      if len < 0 || at + len > until then throw Refused("DELTA_LENGTH_BYTE_ARRAY bytes cut short")
      val out = java.util.Arrays.copyOfRange(b, at, at + len)
      at += len
      out
    }

  /** DELTA_BYTE_ARRAY: each value a prefix of the one before and a suffix */
  def deltaStrings(b: Array[Byte], from: Int, until: Int, n: Int): Array[Array[Byte]] =
    val (prefixes, a) = deltaInts(b, from, until, bits32 = true)
    val suffixes = deltaLengths(b, a, until, n)
    if prefixes.length < n then throw Refused(s"DELTA_BYTE_ARRAY with ${prefixes.length} prefixes for $n values")
    val out = new Array[Array[Byte]](n)
    var prev = Array.emptyByteArray
    var i = 0
    while i < n do
      val p = prefixes(i).toInt
      if p < 0 || p > prev.length then throw Refused(s"DELTA_BYTE_ARRAY: a prefix of $p bytes of a ${prev.length}-byte value")
      val v = new Array[Byte](p + suffixes(i).length)
      System.arraycopy(prev, 0, v, 0, p)
      System.arraycopy(suffixes(i), 0, v, p, suffixes(i).length)
      out(i) = v
      prev = v
      i += 1
    out

  /** BYTE_STREAM_SPLIT back to PLAIN: byte `k` of value `i` is at `k * n + i` */
  def unsplit(b: Array[Byte], from: Int, until: Int, n: Int, width: Int): Array[Byte] =
    if until - from < n.toLong * width then throw Refused("BYTE_STREAM_SPLIT values cut short")
    val out = new Array[Byte](n * width)
    var i = 0
    while i < n do
      var k = 0
      while k < width do { out(i * width + k) = b(from + k * n + i); k += 1 }
      i += 1
    out
