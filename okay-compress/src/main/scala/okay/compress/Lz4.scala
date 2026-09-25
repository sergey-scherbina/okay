package okay.compress

/**
 * LZ4 BLOCKS (the LZ4 block format, Yann Collet): a sequence is a token
 * (literal length, match length - 4), the literals, a 2-byte offset and
 * the rest of the match length; the last sequence has literals only.
 *
 * The compressor is LZ4's fast one: a hash table of the 4-byte sequences
 * seen, a match taken greedily and extended forward, the search step
 * growing while nothing matches (LZ4's "acceleration"). It keeps the
 * format's end rules: the last 5 bytes are literals, and no match starts
 * within the last 12.
 */
object Lz4Block:
  private final val MinMatch = 4
  private final val LastLiterals = 5
  private final val MfLimit = 12
  private final val HashLog = 16
  private final val MaxOffset = 65535

  /** the most a block of `n` bytes can grow to */
  def bound(n: Int): Int = n + n / 255 + 16

  /** `src[from, from + len)` compressed into `dst` at `at`; answers the
   * compressed length (`dst` must hold `bound(len)` from `at`) */
  def compress(src: Array[Byte], from: Int, len: Int, dst: Array[Byte], at: Int): Int =
    var op = at
    val end = from + len
    val anchorEnd = end - LastLiterals
    var anchor = from
    if len >= MfLimit + 1 then
      // a table sized to the input (a small block need not clear 256 KiB)
      val hashLog = math.max(10, math.min(HashLog, 32 - Integer.numberOfLeadingZeros(len - 1)))
      val table = new Array[Int](1 << hashLog)           // position + 1; 0 is empty
      val mfLimit = end - MfLimit
      var i = from
      var misses = 1 << 6
      while i < mfLimit do
        val seq = Le.i32(src, i)
        val h = (seq * -1640531535) >>> (32 - hashLog)    // 2654435761, Knuth's
        val ref = table(h) - 1
        table(h) = i + 1
        if ref >= from && i - ref <= MaxOffset && Le.i32(src, ref) == seq then
          var m = MinMatch
          while i + m < anchorEnd && src(ref + m) == src(i + m) do m += 1
          op = sequence(src, anchor, i - anchor, i - ref, m, dst, op)
          i += m
          anchor = i
          misses = 1 << 6
          // the position just before the match's end, so a run continues
          if i - 2 < mfLimit && i - 2 > from then
            table((Le.i32(src, i - 2) * -1640531535) >>> (32 - hashLog)) = i - 2 + 1
        else
          i += misses >>> 6
          misses += 1
    // the last literals; the answer is a LENGTH, not the end position
    lastLiterals(src, anchor, end - anchor, dst, op) - at

  private def length(extra: Int, dst: Array[Byte], at: Int): Int =
    var op = at
    var r = extra
    while r >= 255 do { dst(op) = -1; op += 1; r -= 255 }
    dst(op) = r.toByte
    op + 1

  private def sequence(src: Array[Byte], lit: Int, litLen: Int, offset: Int, matchLen: Int,
                       dst: Array[Byte], at: Int): Int =
    var op = at
    val token = op
    op += 1
    val ml = matchLen - MinMatch
    dst(token) = ((math.min(litLen, 15) << 4) | math.min(ml, 15)).toByte
    if litLen >= 15 then op = length(litLen - 15, dst, op)
    System.arraycopy(src, lit, dst, op, litLen)
    op += litLen
    dst(op) = offset.toByte; dst(op + 1) = (offset >>> 8).toByte
    op += 2
    if ml >= 15 then op = length(ml - 15, dst, op)
    op

  private def lastLiterals(src: Array[Byte], lit: Int, litLen: Int, dst: Array[Byte], at: Int): Int =
    var op = at
    dst(op) = (math.min(litLen, 15) << 4).toByte
    op += 1
    if litLen >= 15 then op = length(litLen - 15, dst, op)
    System.arraycopy(src, lit, dst, op, litLen)
    op + litLen

  /** one block decompressed into `dst` from `at`, which may already hold
   * earlier blocks a match may reach back into; answers the new end.
   * `limit` is how far `dst` may be written.
   *
   * No closure touches `ip` or `op`: a local var a closure mutates becomes
   * a heap `IntRef`, and every access in this loop would go through it —
   * measured 5-10x slower than aircompressor before (okay-compress stage 5).
   * Short copies are loops: `arraycopy`'s setup costs more than 16 bytes. */
  def decompress(src: Array[Byte], from: Int, len: Int, dst: Array[Byte], at: Int, limit: Int, floor: Int = 0): Int =
    val end = from + len
    var ip = from
    var op = at
    var done = false
    while !done do
      if ip >= end then corrupt("the block ends without its last literals (cut short?)")
      val token = src(ip) & 0xff
      ip += 1
      var litLen = token >>> 4
      if litLen == 15 then
        var b = 255
        while b == 255 do
          if ip >= end then corrupt("a length runs past the block (cut short?)")
          b = src(ip) & 0xff
          ip += 1
          litLen += b
      if litLen > end - ip then corrupt(s"$litLen literals where ${end - ip} bytes remain (cut short?)")
      if litLen > limit - op then corrupt(s"the block decompresses past its $limit-byte limit")
      if litLen <= 16 then
        var k = 0
        while k < litLen do { dst(op + k) = src(ip + k); k += 1 }
      else System.arraycopy(src, ip, dst, op, litLen)
      ip += litLen
      op += litLen
      if ip == end then done = true                       // the last sequence: literals only
      else
        if end - ip < 2 then corrupt("an offset cut short")
        val offset = (src(ip) & 0xff) | (src(ip + 1) & 0xff) << 8
        ip += 2
        if offset == 0 || offset > op - floor then corrupt(s"offset $offset reaches before the start of the output")
        var matchLen = (token & 15) + MinMatch
        if (token & 15) == 15 then
          var b = 255
          while b == 255 do
            if ip >= end then corrupt("a length runs past the block (cut short?)")
            b = src(ip) & 0xff
            ip += 1
            matchLen += b
        if matchLen > limit - op then corrupt(s"the block decompresses past its $limit-byte limit")
        val ref = op - offset
        if offset >= matchLen && matchLen > 16 then System.arraycopy(dst, ref, dst, op, matchLen)
        else
          // short, or overlapping (a match repeating its own output): byte by byte
          var k = 0
          while k < matchLen do { dst(op + k) = dst(ref + k); k += 1 }
        op += matchLen
    op

  private def corrupt(why: String): Nothing = throw Corrupt(s"not an LZ4 block this reads: $why")

/**
 * The LZ4 FRAME format (lz4_Frame_format.md): magic 184D2204, a
 * descriptor (FLG, BD, optional content size and dictionary id, the
 * header checksum = second byte of XXH32 of the descriptor), blocks
 * (a 4-byte size whose high bit says "stored uncompressed", optional
 * block checksums), an end mark, an optional XXH32 of the content.
 * Skippable frames (184D2A50..5F) are skipped; frames concatenate.
 *
 * Written here: independent blocks of up to 4 MiB, the content size and
 * the content checksum, a block stored as it is when compression does
 * not shrink it. Read: every flag, blocks dependent or not (a dependent
 * block's matches reach into the earlier output, which one contiguous
 * output buffer gives for free). A dictionary id is refused by name.
 */
object Lz4Frame extends Codec:
  def name = "lz4"
  private final val Magic = 0x184d2204
  private final val BlockMax = 4 << 20

  def compress(bytes: Array[Byte]): Array[Byte] =
    val out = Out(bytes.length + bytes.length / 255 + 64)
    out.int32(Magic)
    val desc = out.n
    out.byte(0x40 | 0x20 | 0x08 | 0x04)                 // version 01, independent blocks, content size, content checksum
    out.byte(7 << 4)                                     // block max size: 4 MiB
    out.int64(bytes.length.toLong)
    out.byte((XxHash.xxh32(out.buf, desc, out.n - desc) >>> 8) & 0xff)
    var from = 0
    while from < bytes.length do
      val len = math.min(BlockMax, bytes.length - from)
      out.room(4 + Lz4Block.bound(len))
      val c = Lz4Block.compress(bytes, from, len, out.buf, out.n + 4)
      if c < len then
        Le.put32(out.buf, out.n, c)
        out.n += 4 + c
      else
        out.int32(len | 0x80000000)                      // stored: compression did not pay
        out.bytes(bytes, from, len)
      from += len
    out.int32(0)                                         // end mark
    out.int32(XxHash.xxh32(bytes, 0, bytes.length))
    out.result()

  def decompress(bytes: Array[Byte]): Array[Byte] =
    def corrupt(why: String): Nothing = throw Corrupt(s"not an LZ4 frame this reads: $why")
    var ip = 0
    def need(k: Int, what: String): Unit =
      if bytes.length - ip < k then corrupt(s"$what cut short")
    val out = Out(bytes.length * 3)
    if bytes.isEmpty then corrupt("no frame at all")
    while ip < bytes.length do
      need(4, "the magic number")
      val magic = Le.i32(bytes, ip)
      ip += 4
      if (magic & 0xfffffff0) == 0x184d2a50 then        // a skippable frame
        need(4, "a skippable frame's size")
        val size = Le.i32(bytes, ip)
        ip += 4
        if size < 0 || size > bytes.length - ip then corrupt("a skippable frame's size runs past the input")
        ip += size
      else
        if magic != Magic then corrupt(f"magic 0x$magic%08x, not LZ4's 0x184d2204")
        val start = out.n
        val desc = ip
        need(2, "the frame descriptor")
        val flg = bytes(ip) & 0xff
        val bd = bytes(ip + 1) & 0xff
        ip += 2
        if (flg >>> 6) != 1 then corrupt(s"frame version ${flg >>> 6}; this reads version 1")
        val blockChecksum = (flg & 0x10) != 0
        val hasSize = (flg & 0x08) != 0
        val contentChecksum = (flg & 0x04) != 0
        if (flg & 0x01) != 0 then corrupt("the frame names a dictionary, which this does not hold")
        val blockMax = ((bd >>> 4) & 7) match
          case 4 => 64 << 10
          case 5 => 256 << 10
          case 6 => 1 << 20
          case 7 => 4 << 20
          case other => corrupt(s"block max size id $other")
        var size = -1L
        if hasSize then
          need(8, "the content size")
          size = Le.i64(bytes, ip)
          ip += 8
          if size < 0 || size > Int.MaxValue - 16 then corrupt(s"a content size of $size bytes")
          out.room(size.toInt)
        need(1, "the header checksum")
        val hc = bytes(ip) & 0xff
        if hc != ((XxHash.xxh32(bytes, desc, ip - desc) >>> 8) & 0xff) then corrupt("the header checksum does not match")
        ip += 1
        var blocks = true
        while blocks do
          need(4, "a block size")
          val raw = Le.i32(bytes, ip)
          ip += 4
          if raw == 0 then blocks = false
          else
            val stored = (raw & 0x80000000) != 0
            val len = raw & 0x7fffffff
            if len > blockMax then corrupt(s"a block of $len bytes past the frame's $blockMax")
            need(len, "a block")
            if stored then out.bytes(bytes, ip, len)
            else
              out.room(blockMax)
              out.n = Lz4Block.decompress(bytes, ip, len, out.buf, out.n, out.n + blockMax, start)
            if blockChecksum then
              need(4, "a block checksum")
              if Le.i32(bytes, ip + len) != XxHash.xxh32(bytes, ip, len) then corrupt("a block checksum does not match")
              ip += 4
            ip += len
        if hasSize && out.n - start != size then corrupt(s"the frame said $size bytes and held ${out.n - start}")
        if contentChecksum then
          need(4, "the content checksum")
          if Le.i32(bytes, ip) != XxHash.xxh32(out.buf, start, out.n - start) then corrupt("the content checksum does not match")
          ip += 4
    out.result()
