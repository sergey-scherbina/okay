package okay.compress

/**
 * ZSTD (RFC 8878, "Zstandard Compression and the 'application/zstd' Media
 * Type"). DECOMPRESSION of every frame the RFC defines without a
 * dictionary: raw, RLE and compressed blocks; literals raw, RLE, or
 * Huffman-coded in one or four streams, a table carried or repeated;
 * sequences coded with FSE tables predefined, RLE, carried or repeated;
 * the three repeat offsets; the XXH64 content checksum; concatenated and
 * skippable frames. Compression is [[ZstdEncoder]].
 */
object Zstd extends Codec:
  def name = "zstd"

  def compress(bytes: Array[Byte]): Array[Byte] = ZstdEncoder.compress(bytes)

  private final val Magic = 0xfd2fb528
  private final val BlockMax = 128 << 10

  private[compress] def corrupt(why: String): Nothing = throw Corrupt(s"not a ZSTD frame this reads: $why")

  def decompress(bytes: Array[Byte]): Array[Byte] =
    if bytes.isEmpty then corrupt("no frame at all")
    val out = Out(bytes.length * 4)
    var ip = 0
    def need(k: Int, what: String): Unit = if bytes.length - ip < k then corrupt(s"$what cut short")
    while ip < bytes.length do
      need(4, "the magic number")
      val magic = Le.i32(bytes, ip)
      ip += 4
      if (magic & 0xfffffff0) == 0x184d2a50 then
        need(4, "a skippable frame's size")
        val size = Le.i32(bytes, ip)
        ip += 4
        if size < 0 || size > bytes.length - ip then corrupt("a skippable frame runs past the input")
        ip += size
      else
        if magic != Magic then corrupt(f"magic 0x$magic%08x, not ZSTD's 0xfd2fb528")
        ip = frame(bytes, ip, out)
    out.result()

  /** one frame from after its magic; answers the position after it */
  private def frame(src: Array[Byte], at: Int, out: Out): Int =
    var ip = at
    def need(k: Int, what: String): Unit = if src.length - ip < k then corrupt(s"$what cut short")
    need(1, "the frame header")
    val fhd = src(ip) & 0xff
    ip += 1
    val fcsFlag = fhd >>> 6
    val single = (fhd & 0x20) != 0
    if (fhd & 0x08) != 0 then corrupt("a reserved bit of the frame header is set")
    val checksum = (fhd & 0x04) != 0
    val dictSize = Array(0, 1, 2, 4)(fhd & 3)
    if !single then
      need(1, "the window descriptor")
      val wd = src(ip) & 0xff
      ip += 1
      val exp = wd >>> 3
      if exp > 31 - 10 then corrupt(s"a window of 2^${10 + exp} bytes")
    need(dictSize, "the dictionary id")
    var dict = 0L
    var k = 0
    while k < dictSize do { dict |= (src(ip + k) & 0xffL) << (8 * k); k += 1 }
    ip += dictSize
    if dict != 0 then corrupt(s"the frame needs dictionary $dict, which this does not hold")
    val fcsSize = fcsFlag match
      case 0 => if single then 1 else 0
      case 1 => 2
      case 2 => 4
      case _ => 8
    need(fcsSize, "the content size")
    var size = -1L
    if fcsSize > 0 then
      size = 0L
      k = 0
      while k < fcsSize do { size |= (src(ip + k) & 0xffL) << (8 * k); k += 1 }
      if fcsSize == 2 then size += 256
      ip += fcsSize
      if size < 0 || size > Int.MaxValue - 16 then corrupt(s"a content size of $size bytes")
      out.room(size.toInt)
    val start = out.n
    val st = State()
    var last = false
    while !last do
      need(3, "a block header")
      val h = (src(ip) & 0xff) | (src(ip + 1) & 0xff) << 8 | (src(ip + 2) & 0xff) << 16
      ip += 3
      last = (h & 1) != 0
      val kind = (h >>> 1) & 3
      val bsize = h >>> 3
      kind match
        case 0 =>
          need(bsize, "a raw block")
          out.bytes(src, ip, bsize)
          ip += bsize
        case 1 =>
          need(1, "an RLE block")
          out.room(bsize)
          java.util.Arrays.fill(out.buf, out.n, out.n + bsize, src(ip))
          out.n += bsize
          ip += 1
        case 2 =>
          if bsize > BlockMax then corrupt(s"a compressed block of $bsize bytes past 128 KiB")
          need(bsize, "a compressed block")
          block(src, ip, bsize, out, start, st)
          ip += bsize
        case _ => corrupt("a block of the reserved type")
    if size >= 0 && out.n - start != size then corrupt(s"the frame said $size bytes and held ${out.n - start}")
    if checksum then
      need(4, "the content checksum")
      if Le.i32(src, ip) != XxHash.xxh64(out.buf, start, out.n - start).toInt then corrupt("the content checksum does not match")
      ip += 4
    ip

  /** what a frame's blocks share: the Huffman table, the three sequence
   * tables and the repeat offsets */
  private final class State:
    var huffman: Huffman = null
    var ll: Fse = null
    var of: Fse = null
    var ml: Fse = null
    val reps: Array[Int] = Array(1, 4, 8)

  // ---- a compressed block ----------------------------------------------------

  private def block(src: Array[Byte], from: Int, len: Int, out: Out, frameStart: Int, st: State): Unit =
    val end = from + len
    // the literals section
    if len < 1 then corrupt("an empty compressed block")
    val b0 = src(from) & 0xff
    val ltype = b0 & 3
    val sf = (b0 >>> 2) & 3
    var ip = from
    var literals: Array[Byte] = null
    var litFrom = 0
    var litLen = 0
    def byte(i: Int): Int = if i < end then src(i) & 0xff else corrupt("the literals header cut short")
    ltype match
      case 0 | 1 =>
        val (size, hdr) = sf match
          case 0 | 2 => (b0 >>> 3, 1)
          case 1 => ((b0 >>> 4) | byte(from + 1) << 4, 2)
          case _ => ((b0 >>> 4) | byte(from + 1) << 4 | byte(from + 2) << 12, 3)
        ip = from + hdr
        if ltype == 0 then
          if end - ip < size then corrupt("raw literals cut short")
          literals = src; litFrom = ip; litLen = size
          ip += size
        else
          if ip >= end then corrupt("RLE literals cut short")
          literals = Array.fill(size)(src(ip)); litFrom = 0; litLen = size
          ip += 1
      case _ =>
        val (regen, comp, hdr, four) = sf match
          case 0 | 1 =>
            val v = b0 | byte(from + 1) << 8 | byte(from + 2) << 16
            ((v >>> 4) & 0x3ff, (v >>> 14) & 0x3ff, 3, sf == 1)
          case 2 =>
            val v = (b0 | byte(from + 1) << 8 | byte(from + 2) << 16 | byte(from + 3) << 24).toLong & 0xffffffffL
            (((v >>> 4) & 0x3fff).toInt, ((v >>> 18) & 0x3fff).toInt, 4, true)
          case _ =>
            val v = (b0 | byte(from + 1) << 8 | byte(from + 2) << 16 | byte(from + 3) << 24).toLong & 0xffffffffL |
              (byte(from + 4).toLong << 32)
            (((v >>> 4) & 0x3ffff).toInt, ((v >>> 22) & 0x3ffff).toInt, 5, true)
        ip = from + hdr
        if end - ip < comp then corrupt("compressed literals cut short")
        var p = ip
        if ltype == 2 then
          val (h, used) = Huffman.read(src, p, ip + comp)
          st.huffman = h
          p += used
        else if st.huffman == null then corrupt("treeless literals with no earlier Huffman table")
        literals = st.huffman.decode(src, p, ip + comp, regen, four)
        litFrom = 0; litLen = regen
        ip += comp
    if litLen > BlockMax then corrupt(s"$litLen literals past 128 KiB")
    // the sequences section
    if ip >= end then corrupt("the block ends before its sequences section")
    var nSeq = src(ip) & 0xff
    ip += 1
    if nSeq >= 128 then
      if nSeq < 255 then
        if ip >= end then corrupt("the sequence count cut short")
        nSeq = ((nSeq - 128) << 8) + (src(ip) & 0xff); ip += 1
      else
        if end - ip < 2 then corrupt("the sequence count cut short")
        nSeq = (src(ip) & 0xff) + ((src(ip + 1) & 0xff) << 8) + 0x7f00; ip += 2
    if nSeq == 0 then
      out.bytes(literals, litFrom, litLen)
      if ip != end then corrupt("bytes after a block with no sequences")
    else
      if ip >= end then corrupt("the compression modes cut short")
      val modes = src(ip) & 0xff
      ip += 1
      if (modes & 3) != 0 then corrupt("the reserved bits of the compression modes are set")
      def table(mode: Int, prev: Fse, predefined: Fse, maxSymbol: Int, maxLog: Int, what: String): Fse = mode match
        case 0 => predefined
        case 1 =>
          if ip >= end then corrupt(s"the $what RLE symbol cut short")
          val s = src(ip) & 0xff
          ip += 1
          if s > maxSymbol then corrupt(s"$what symbol $s")
          Fse.rle(s)
        case 2 =>
          val (t, used) = Fse.read(src, ip, end, maxSymbol, maxLog, what)
          ip += used
          t
        case _ => if prev == null then corrupt(s"a repeated $what table with none before it") else prev
      st.ll = table(modes >>> 6, st.ll, Fse.LlDefault, 35, 9, "literals-length")
      st.of = table((modes >>> 4) & 3, st.of, Fse.OfDefault, 31, 8, "offset")
      st.ml = table((modes >>> 2) & 3, st.ml, Fse.MlDefault, 52, 9, "match-length")
      sequences(src, ip, end, nSeq, literals, litFrom, litLen, out, frameStart, st)

  private val LlBase = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
    16, 18, 20, 22, 24, 28, 32, 40, 48, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536)
  private val LlBits = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  private val MlBase = Array.tabulate(32)(_ + 3) ++ Array(35, 37, 39, 41, 43, 47, 51, 59, 67, 83, 99, 131, 259, 515,
    1027, 2051, 4099, 8195, 16387, 32771, 65539)
  private val MlBits = Array.fill(32)(0) ++ Array(1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

  private def sequences(src: Array[Byte], from: Int, end: Int, nSeq: Int, lit: Array[Byte], litFrom: Int, litLen: Int,
                        out: Out, frameStart: Int, st: State): Unit =
    val bits = BackBits(src, from, end)
    var llState = bits.read(st.ll.log)
    var ofState = bits.read(st.of.log)
    var mlState = bits.read(st.ml.log)
    var lp = litFrom
    val litEnd = litFrom + litLen
    val reps = st.reps
    var s = 0
    while s < nSeq do
      val ofCode = st.of.symbol(ofState)
      val mlCode = st.ml.symbol(mlState)
      val llCode = st.ll.symbol(llState)
      if ofCode > 31 then corrupt(s"offset code $ofCode")
      val ov = (1L << ofCode) + bits.readLong(ofCode)
      val ml = MlBase(mlCode) + bits.read(MlBits(mlCode))
      val ll = LlBase(llCode) + bits.read(LlBits(llCode))
      // the repeat offsets (RFC 8878 3.1.2.5)
      val offset =
        if ov > 3 then
          val o = (ov - 3).toInt
          reps(2) = reps(1); reps(1) = reps(0); reps(0) = o
          o
        else
          val idx = ov.toInt - (if ll == 0 then 0 else 1)
          idx match
            case 0 => reps(0)
            case 1 => { val o = reps(1); reps(1) = reps(0); reps(0) = o; o }
            case 2 => { val o = reps(2); reps(2) = reps(1); reps(1) = reps(0); reps(0) = o; o }
            case _ =>
              val o = reps(0) - 1
              if o <= 0 then corrupt("a repeat offset of 0")
              reps(2) = reps(1); reps(1) = reps(0); reps(0) = o
              o
      // literals, then the match
      if ll > litEnd - lp then corrupt(s"a sequence wants $ll literals where ${litEnd - lp} remain")
      out.bytes(lit, lp, ll)
      lp += ll
      if offset > out.n - frameStart then corrupt(s"offset $offset reaches before the frame's output")
      out.room(ml)
      val ref = out.n - offset
      if offset >= ml then System.arraycopy(out.buf, ref, out.buf, out.n, ml)
      else
        var k = 0
        while k < ml do { out.buf(out.n + k) = out.buf(ref + k); k += 1 }
      out.n += ml
      s += 1
      if s < nSeq then
        llState = st.ll.next(llState, bits)
        mlState = st.ml.next(mlState, bits)
        ofState = st.of.next(ofState, bits)
    if !bits.exhausted then corrupt("the sequences bitstream has bits left over")
    out.bytes(lit, lp, litEnd - lp)

/**
 * A backward bitstream (RFC 8878 4.1): written forward, read from its END;
 * the last byte's highest set bit marks where the bits start.
 */
private[compress] final class BackBits(src: Array[Byte], from: Int, end: Int):
  if end <= from then Zstd.corrupt("an empty bitstream")
  private val lastByte = src(end - 1) & 0xff
  if lastByte == 0 then Zstd.corrupt("a bitstream without its end mark")
  /** bits left, counted from the stream's start; below 0 is past it */
  var pos: Int = (end - from) * 8 - (Integer.numberOfLeadingZeros(lastByte) - 24) - 1

  def exhausted: Boolean = pos == 0
  def overflowed: Boolean = pos < 0

  /** `n` bits (at most 56) at `at`, zeros where the stream has none */
  private def bitsAt(at: Int, n: Int): Long =
    if n == 0 then 0L
    else
      var v = 0L
      var shift = 0
      var bytePos = at >> 3
      val drop = at & 7
      var got = 0
      while got < n + drop do
        val b = if bytePos >= 0 && from + bytePos < end then src(from + bytePos) & 0xffL else 0L
        v |= b << shift
        shift += 8; got += 8; bytePos += 1
      (v >>> drop) & ((1L << n) - 1)

  def readLong(n: Int): Long =
    if n == 0 then 0L
    else
      pos -= n
      if pos >= 0 then bitsAt(pos, n)
      else if pos + n > 0 then bitsAt(0, pos + n) << (-pos)   // the high bits exist, the low ones are past the start
      else 0L
  def read(n: Int): Int = readLong(n).toInt

  /** the next `n` bits without consuming them */
  def peek(n: Int): Int =
    if pos >= n then bitsAt(pos - n, n).toInt
    else if pos > 0 then (bitsAt(0, pos) << (n - pos)).toInt
    else 0
  def skip(n: Int): Unit = pos -= n

/** an FSE decoding table (RFC 8878 4.1.1): per state, a symbol, the bits to
 * read, and the baseline the read is added to */
private[compress] final class Fse(val log: Int, sym: Array[Int], nbBits: Array[Int], base: Array[Int]):
  def symbol(state: Int): Int = sym(state)
  def next(state: Int, bits: BackBits): Int = base(state) + bits.read(nbBits(state))

private[compress] object Fse:
  /** the table of a normalised distribution; -1 is "less than one" */
  def build(norm: Array[Int], log: Int): Fse =
    val size = 1 << log
    val sym = new Array[Int](size)
    var high = size - 1
    var s = 0
    while s < norm.length do
      if norm(s) == -1 then { sym(high) = s; high -= 1 }
      s += 1
    val step = (size >>> 1) + (size >>> 3) + 3
    val mask = size - 1
    var pos = 0
    s = 0
    while s < norm.length do
      var k = 0
      while k < norm(s) do
        sym(pos) = s
        pos = (pos + step) & mask
        while pos > high do pos = (pos + step) & mask
        k += 1
      s += 1
    if pos != 0 then Zstd.corrupt("an FSE distribution that does not fill its table")
    val next = norm.map(p => if p == -1 then 1 else p)
    val nbBits = new Array[Int](size)
    val base = new Array[Int](size)
    var u = 0
    while u < size do
      val x = next(sym(u))
      next(sym(u)) = x + 1
      val nb = log - (31 - Integer.numberOfLeadingZeros(x))
      nbBits(u) = nb
      base(u) = (x << nb) - size
      u += 1
    Fse(log, sym, nbBits, base)

  /** a one-symbol table: every state that symbol, no bits read */
  def rle(symbol: Int): Fse = Fse(0, Array(symbol), Array(0), Array(0))

  /** an FSE table description (RFC 8878 4.1.1), read FORWARD; answers the
   * table and the bytes it took */
  def read(src: Array[Byte], from: Int, end: Int, maxSymbol: Int, maxLog: Int, what: String): (Fse, Int) =
    var bitPos = 0L
    def bits(n: Int): Int =
      var v = 0L
      var k = 0
      while k < n do
        val at = bitPos + k
        val byteAt = from + (at >> 3).toInt
        if byteAt >= end then Zstd.corrupt(s"the $what table description cut short")
        v |= (((src(byteAt) >> (at & 7).toInt) & 1).toLong) << k
        k += 1
      v.toInt
    val log = bits(4) + 5
    bitPos = 4
    if log > maxLog then Zstd.corrupt(s"a $what table of accuracy $log past $maxLog")
    var remaining = (1 << log) + 1
    var threshold = 1 << log
    var nb = log + 1
    val norm = Array.fill(maxSymbol + 1)(0)
    var s = 0
    var previous0 = false
    while remaining > 1 && s <= maxSymbol do
      if previous0 then
        var repeat = bits(2); bitPos += 2
        while repeat == 3 do { s += 3; repeat = bits(2); bitPos += 2 }
        s += repeat
        if s > maxSymbol then Zstd.corrupt(s"a $what distribution past symbol $maxSymbol")
        previous0 = false
      if s <= maxSymbol && remaining > 1 then
        val max = (2 * threshold - 1) - remaining
        val low = bits(nb - 1)
        var count =
          if low < max then { bitPos += nb - 1; low }
          else
            val v = bits(nb)
            bitPos += nb
            if v >= threshold then v - max else v
        count -= 1
        remaining -= math.abs(count)
        norm(s) = count
        s += 1
        previous0 = count == 0
        while remaining < threshold do { nb -= 1; threshold >>= 1 }
    if remaining != 1 then Zstd.corrupt(s"a $what distribution that does not sum to its table")
    (build(norm.take(s), log), ((bitPos + 7) >> 3).toInt)

  val LlDefault: Fse = build(Array(4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1, 1,
    -1, -1, -1, -1), 6)
  val MlDefault: Fse = build(Array(1, 4, 3, 2, 2, 2, 2, 2, 2) ++ Array.fill(37)(1) ++ Array.fill(7)(-1), 6)
  val OfDefault: Fse = build(Array(1, 1, 1, 1, 1, 1, 2, 2, 2) ++ Array.fill(15)(1) ++ Array.fill(5)(-1), 5)

/** a Huffman decoding table for literals (RFC 8878 4.2): `bits` bits of
 * the stream index a symbol and its code length */
private[compress] final class Huffman(maxBits: Int, sym: Array[Byte], len: Array[Byte]):
  /** `n` literals from `[from, end)`, in one stream or four */
  def decode(src: Array[Byte], from: Int, end: Int, n: Int, four: Boolean): Array[Byte] =
    val out = new Array[Byte](n)
    if !four then stream(src, from, end, out, 0, n)
    else
      if end - from < 6 then Zstd.corrupt("the Huffman jump table cut short")
      val s1 = Le.u16(src, from); val s2 = Le.u16(src, from + 2); val s3 = Le.u16(src, from + 4)
      val start = from + 6
      if start + s1 + s2 + s3 > end then Zstd.corrupt("Huffman streams longer than their literals")
      val quarter = (n + 3) / 4
      if quarter * 3 > n then Zstd.corrupt(s"four streams for $n literals")
      stream(src, start, start + s1, out, 0, quarter)
      stream(src, start + s1, start + s1 + s2, out, quarter, quarter)
      stream(src, start + s1 + s2, start + s1 + s2 + s3, out, 2 * quarter, quarter)
      stream(src, start + s1 + s2 + s3, end, out, 3 * quarter, n - 3 * quarter)
    out

  private def stream(src: Array[Byte], from: Int, end: Int, out: Array[Byte], at: Int, n: Int): Unit =
    val bits = BackBits(src, from, end)
    var i = 0
    while i < n do
      val idx = bits.peek(maxBits)
      out(at + i) = sym(idx)
      bits.skip(len(idx))
      i += 1
    if !bits.exhausted then Zstd.corrupt("a Huffman stream does not end where its literals do")

private[compress] object Huffman:
  final val MaxBits = 11

  /** a Huffman tree description (RFC 8878 4.2.1); answers the table and
   * the bytes it took */
  def read(src: Array[Byte], from: Int, end: Int): (Huffman, Int) =
    if from >= end then Zstd.corrupt("the Huffman tree description cut short")
    val hb = src(from) & 0xff
    val (weights, used) =
      if hb >= 128 then
        val n = hb - 127
        val bytes = (n + 1) / 2
        if from + 1 + bytes > end then Zstd.corrupt("the Huffman weights cut short")
        (Array.tabulate(n)(i => (src(from + 1 + i / 2) >> (if i % 2 == 0 then 4 else 0)) & 15), 1 + bytes)
      else
        if from + 1 + hb > end then Zstd.corrupt("the Huffman weights cut short")
        (fseWeights(src, from + 1, from + 1 + hb), 1 + hb)
    (fromWeights(weights), used)

  /** the weights, FSE-coded with two interleaved states */
  private def fseWeights(src: Array[Byte], from: Int, end: Int): Array[Int] =
    val (t, used) = Fse.read(src, from, end, 255, 6, "Huffman weights")
    val bits = BackBits(src, from + used, end)
    var s1 = bits.read(t.log)
    var s2 = bits.read(t.log)
    val out = scala.collection.mutable.ArrayBuffer.empty[Int]
    var done = false
    while !done do
      out += t.symbol(s1)
      s1 = t.next(s1, bits)
      if bits.overflowed then { out += t.symbol(s2); done = true }
      else
        out += t.symbol(s2)
        s2 = t.next(s2, bits)
        if bits.overflowed then { out += t.symbol(s1); done = true }
      if out.length > 255 then Zstd.corrupt("more than 255 Huffman weights")
    out.toArray

  private def fromWeights(stated: Array[Int]): Huffman =
    if stated.exists(w => w > MaxBits) then Zstd.corrupt("a Huffman weight past 11")
    val sum = stated.foldLeft(0L)((acc, w) => if w > 0 then acc + (1L << (w - 1)) else acc)
    if sum == 0 then Zstd.corrupt("Huffman weights that are all zero")
    val maxBits = 64 - java.lang.Long.numberOfLeadingZeros(sum)       // the next power of two's log
    val total = 1L << maxBits
    val rest = total - sum
    if java.lang.Long.bitCount(rest) != 1 then Zstd.corrupt("Huffman weights that do not complete a tree")
    val lastWeight = 63 - java.lang.Long.numberOfLeadingZeros(rest) + 1
    val weights = stated :+ lastWeight
    if maxBits > MaxBits then Zstd.corrupt(s"a Huffman code of $maxBits bits")
    // the table: by weight ascending, symbols in order within a weight
    val size = 1 << maxBits
    val sym = new Array[Byte](size)
    val len = new Array[Byte](size)
    val rankStart = new Array[Int](maxBits + 2)
    val counts = new Array[Int](maxBits + 2)
    weights.foreach(w => if w > 0 then counts(w) += 1)
    var next = 0
    var w = 1
    while w <= maxBits do
      rankStart(w) = next
      next += counts(w) << (w - 1)
      w += 1
    var s = 0
    while s < weights.length do
      val wt = weights(s)
      if wt > 0 then
        val n = 1 << (wt - 1)
        val start = rankStart(wt)
        var k = 0
        while k < n do
          sym(start + k) = s.toByte
          len(start + k) = (maxBits + 1 - wt).toByte
          k += 1
        rankStart(wt) = start + n
      s += 1
    Huffman(maxBits, sym, len)
