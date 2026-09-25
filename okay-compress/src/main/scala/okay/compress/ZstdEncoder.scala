package okay.compress

/** ZSTD frames (RFC 8878): for now RAW blocks with the content size and
 * checksum — valid ZSTD that every decoder reads; the real compressor is
 * stage 3 (specs/okay-compress.md) */
object ZstdEncoder:
  private final val BlockMax = 128 << 10

  def compress(bytes: Array[Byte]): Array[Byte] =
    val out = Out(bytes.length + bytes.length / BlockMax * 3 + 32)
    out.int32(0xfd2fb528)
    out.byte(0xc0 | 0x04)                 // an 8-byte content size, a checksum, windowed
    out.byte(windowDescriptor(bytes.length))
    out.int64(bytes.length.toLong)
    var from = 0
    var last = false
    while !last do
      val len = math.min(BlockMax, bytes.length - from)
      last = from + len == bytes.length
      val h = (len << 3) | (0 << 1) | (if last then 1 else 0)
      out.byte(h); out.byte(h >>> 8); out.byte(h >>> 16)
      out.bytes(bytes, from, len)
      from += len
    out.int32(XxHash.xxh64(bytes, 0, bytes.length).toInt)
    out.result()

  /** the smallest window (exponent, no mantissa) holding a block of the input */
  private[compress] def windowDescriptor(n: Int): Int =
    var exp = 0
    while (1L << (10 + exp)) < math.min(n.toLong, BlockMax.toLong).max(1L) do exp += 1
    exp << 3
