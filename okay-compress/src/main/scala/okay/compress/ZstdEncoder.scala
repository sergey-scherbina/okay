package okay.compress

/**
 * ZSTD COMPRESSION (RFC 8878), the frames `Zstd.decompress` and every
 * other decoder read.
 *
 * - A single-segment frame: the content size is stated, so a match may
 *   reach anywhere back in the frame; the XXH64 checksum closes it.
 * - Blocks of up to 128 KiB. Each is LZ77-parsed: a hash chain of the
 *   4-byte sequences seen, the longest match among the most recent
 *   candidates, one step of lazy matching, and the three repeat offsets
 *   tried first. A block that does not shrink is stored raw; a block of
 *   one byte repeated, as RLE.
 * - Sequences are FSE-coded with the RFC's predefined tables, or with a
 *   table built from the block's own counts when that is shorter (the
 *   table's description is paid for). The encoder is derived from the
 *   same decoding table the decoder uses — for each symbol, which state
 *   leads to each next state — so the two cannot disagree.
 * - Literals are Huffman-coded (four streams past 1 KiB) when that is
 *   shorter than storing them raw.
 */
object ZstdEncoder:
  private final val BlockMax = 128 << 10
  private final val MinMatch = 4
  private final val HashLog = 17

  /** how hard a level searches: candidates per position on the hash chain,
   * whether one step of lazy matching is tried, and whether the step grows
   * through input that does not match (okay-compress stage 5: the first cut
   * searched 24 deep with lazy matching everywhere, and ran 20x slower than
   * aircompressor's level 3 for a smaller output) */
  private final case class Effort(depth: Int, lazyMatch: Boolean, accelerate: Boolean)
  private def effort(level: Int): Effort =
    if level <= 1 then Effort(1, false, true)
    else if level <= 3 then Effort(4, true, true)
    else if level <= 6 then Effort(16, true, false)
    else Effort(64, true, false)

  /** the default level, 3: ZSTD's own default */
  def compress(bytes: Array[Byte]): Array[Byte] = compress(bytes, 3)

  def compress(bytes: Array[Byte], level: Int): Array[Byte] =
    val e = effort(level)
    val out = Out(bytes.length / 2 + 64)
    out.int32(0xfd2fb528)
    // single segment, a checksum, and the content size in the fewest bytes
    // the RFC allows: 1 (< 256), 2 (256 + a 16-bit value), 4, or 8
    val n = bytes.length.toLong
    if n < 256 then { out.byte(0x20 | 0x04); out.byte(n.toInt) }
    else if n < 65536 + 256 then { out.byte(0x40 | 0x20 | 0x04); out.byte((n - 256).toInt); out.byte(((n - 256) >>> 8).toInt) }
    else if n < (1L << 32) then { out.byte(0x80 | 0x20 | 0x04); out.int32(n.toInt) }
    else { out.byte(0xc0 | 0x20 | 0x04); out.int64(n) }
    val m = Matcher(bytes, e.depth)
    val reps = Array(1, 4, 8)
    var from = 0
    var last = false
    while !last do
      val len = math.min(BlockMax, bytes.length - from)
      last = from + len == bytes.length
      block(bytes, from, len, last, m, reps, out, e)
      from += len
    out.int32(XxHash.xxh64(bytes, 0, bytes.length).toInt)
    out.result()

  private def header(out: Out, size: Int, kind: Int, last: Boolean): Unit =
    val h = (size << 3) | (kind << 1) | (if last then 1 else 0)
    out.byte(h); out.byte(h >>> 8); out.byte(h >>> 16)

  private def block(src: Array[Byte], from: Int, len: Int, last: Boolean, m: Matcher, reps: Array[Int], out: Out, e: Effort): Unit =
    if len > 0 && (1 until len).forall(i => src(from + i) == src(from)) then
      header(out, len, 1, last); out.byte(src(from))
    else
      val saved = reps.clone()
      val body = compressed(src, from, len, m, reps, e)
      if body == null || body.length >= len then
        System.arraycopy(saved, 0, reps, 0, 3)        // a raw block leaves the decoder's offsets as they were
        header(out, len, 0, last); out.bytes(src, from, len)
      else
        header(out, body.length, 2, last); out.bytes(body, 0, body.length)

  // ---- matching ---------------------------------------------------------------

  /** a hash chain over the whole input: `head` the latest position of a
   * hash, `prev` the one before a position with the same hash */
  private final class Matcher(src: Array[Byte], depthLimit: Int):
    private val head = Array.fill(1 << HashLog)(-1)
    private val prev = new Array[Int](math.max(1, src.length))
    private var inserted = 0
    private def hash(i: Int): Int = (Mem.i32(src, i) * -1640531535) >>> (32 - HashLog)

    /** every position before `upTo` into the chain */
    def insertUpTo(upTo: Int): Unit =
      val limit = math.min(upTo, src.length - 3)
      while inserted < limit do
        val h = hash(inserted)
        prev(inserted) = head(h)
        head(h) = inserted
        inserted += 1

    /** the length of the match at `i` against `ref`, up to `end` */
    def matchLength(ref: Int, i: Int, end: Int): Int = Mem.common(src, ref, src, i, end - i)

    /** the longest match at `i` (its length and offset), 0 if none */
    def best(i: Int, end: Int): Long =
      if i + MinMatch > end || i > src.length - 4 then 0L
      else
        insertUpTo(i)
        var cand = head(hash(i))
        var bestLen = 0
        var bestOff = 0
        var depth = 0
        while cand >= 0 && depth < depthLimit do
          // a candidate that cannot beat the best so far is skipped on one byte
          if bestLen == 0 || (i + bestLen < end && src(cand + bestLen) == src(i + bestLen)) then
            val l = matchLength(cand, i, end)
            if l > bestLen then { bestLen = l; bestOff = i - cand }
          cand = prev(cand)
          depth += 1
        if bestLen >= MinMatch then (bestLen.toLong << 32) | bestOff else 0L

  // ---- one compressed block --------------------------------------------------

  /** the sequences of a block: literal length, match length, offset VALUE
   * (the repeat code, or offset + 3) */
  private final class Seqs(cap: Int):
    val ll = new Array[Int](cap); val ml = new Array[Int](cap); val ov = new Array[Int](cap)
    var n = 0
    def add(l: Int, m: Int, o: Int): Unit = { ll(n) = l; ml(n) = m; ov(n) = o; n += 1 }

  /** the offset value the decoder turns into `offset`, updating `reps` as
   * the decoder will (RFC 8878 3.1.2.5) */
  private def offsetValue(offset: Int, litLen: Int, reps: Array[Int]): Int =
    if litLen > 0 && offset == reps(0) then 1
    else if litLen > 0 && offset == reps(1) then { reps(1) = reps(0); reps(0) = offset; 2 }
    else if litLen > 0 && offset == reps(2) then { reps(2) = reps(1); reps(1) = reps(0); reps(0) = offset; 3 }
    else if litLen == 0 && offset == reps(1) then { reps(1) = reps(0); reps(0) = offset; 1 }
    else if litLen == 0 && offset == reps(2) then { reps(2) = reps(1); reps(1) = reps(0); reps(0) = offset; 2 }
    else if litLen == 0 && offset == reps(0) - 1 then { reps(2) = reps(1); reps(1) = reps(0); reps(0) = offset; 3 }
    else { reps(2) = reps(1); reps(1) = reps(0); reps(0) = offset; offset + 3 }

  private def compressed(src: Array[Byte], from: Int, len: Int, m: Matcher, reps: Array[Int], e: Effort): Array[Byte] =
    val end = from + len
    val lits = Out(len)
    val seqs = Seqs(len / MinMatch + 1)
    var anchor = from
    var i = from
    // the last bytes are literals: a match would not pay for its sequence
    val matchEnd = end
    val searchEnd = end - MinMatch
    var misses = 1 << 6
    while i < searchEnd do
      // a repeat offset first: cheap, and the commonest win in tabular data
      var len1 = 0
      var off1 = 0
      var r = 0
      while r < 3 do
        val o = if r == 0 && i == anchor then reps(1) else reps(r)
        if o > 0 && o <= i && src(i) == src(i - o) then
          val l = m.matchLength(i - o, i, matchEnd)
          if l > len1 then { len1 = l; off1 = o }
        r += 1
      val found = m.best(i, matchEnd)
      if (found >>> 32).toInt > len1 + 1 then { len1 = (found >>> 32).toInt; off1 = found.toInt }
      if len1 >= MinMatch then
        misses = 1 << 6
        // lazy: a longer match one byte on wins (only worth the search for a short one)
        if e.lazyMatch && len1 < 32 then
          val next = m.best(i + 1, matchEnd)
          if (next >>> 32).toInt > len1 + 1 then
            i += 1
            len1 = (next >>> 32).toInt; off1 = next.toInt
        val litLen = i - anchor
        lits.bytes(src, anchor, litLen)
        seqs.add(litLen, len1, offsetValue(off1, litLen, reps))
        i += len1
        anchor = i
      else if e.accelerate then
        // through input that does not match, the step grows (LZ4's acceleration)
        i += misses >>> 6
        misses += 1
      else i += 1
    lits.bytes(src, anchor, end - anchor)
    encodeBlock(lits.buf, lits.n, seqs)

  // ---- the block's bytes ------------------------------------------------------

  private def encodeBlock(lit: Array[Byte], litLen: Int, seqs: Seqs): Array[Byte] =
    val out = Out(litLen + seqs.n * 4 + 64)
    literals(lit, litLen, out)
    sequences(seqs, out)
    out.result()

  private def literals(lit: Array[Byte], n: Int, out: Out): Unit =
    val huff = HuffmanEncoder.encode(lit, n)
    if huff != null && huff.length + 5 < n then out.bytes(huff, 0, huff.length)
    else rawLiterals(lit, n, out)

  private[compress] def rawLiterals(lit: Array[Byte], n: Int, out: Out): Unit =
    if n < 32 then out.byte(n << 3)
    else if n < 4096 then { out.byte(0x04 | ((n & 15) << 4)); out.byte(n >>> 4) }
    else { out.byte(0x0c | ((n & 15) << 4)); out.byte(n >>> 4); out.byte(n >>> 12) }
    out.bytes(lit, 0, n)

  private val LlBase = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
    16, 18, 20, 22, 24, 28, 32, 40, 48, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536)
  private val LlBits = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  private val MlBase = Array.tabulate(32)(_ + 3) ++ Array(35, 37, 39, 41, 43, 47, 51, 59, 67, 83, 99, 131, 259, 515,
    1027, 2051, 4099, 8195, 16387, 32771, 65539)
  private val MlBits = Array.fill(32)(0) ++ Array(1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

  private def code(v: Int, base: Array[Int]): Int =
    var c = base.length - 1
    while base(c) > v do c -= 1
    c

  private def sequences(seqs: Seqs, out: Out): Unit =
    val n = seqs.n
    if n < 128 then out.byte(n)
    else if n < 0x7f00 then { out.byte((n >>> 8) + 128); out.byte(n) }
    else { out.byte(255); out.byte(n - 0x7f00); out.byte((n - 0x7f00) >>> 8) }
    if n == 0 then return
    val llc = Array.tabulate(n)(k => code(seqs.ll(k), LlBase))
    val mlc = Array.tabulate(n)(k => code(seqs.ml(k) , MlBase))
    val ofc = Array.tabulate(n)(k => 31 - Integer.numberOfLeadingZeros(seqs.ov(k)))
    // a table per stream: its own when that is shorter than the predefined one
    val (llMode, llT, llDesc) = FseEncoder.choose(llc, 35, 9, FseEncoder.LlDefaultNorm, 6)
    val (ofMode, ofT, ofDesc) = FseEncoder.choose(ofc, 31, 8, FseEncoder.OfDefaultNorm, 5)
    val (mlMode, mlT, mlDesc) = FseEncoder.choose(mlc, 52, 9, FseEncoder.MlDefaultNorm, 6)
    out.byte((llMode << 6) | (ofMode << 4) | (mlMode << 2))
    out.bytes(llDesc, 0, llDesc.length); out.bytes(ofDesc, 0, ofDesc.length); out.bytes(mlDesc, 0, mlDesc.length)
    // the states, last sequence first: each is the state the decoder must be
    // in to emit its symbol and reach the next one
    val llS = new Array[Int](n); val ofS = new Array[Int](n); val mlS = new Array[Int](n)
    llS(n - 1) = llT.anyState(llc(n - 1)); ofS(n - 1) = ofT.anyState(ofc(n - 1)); mlS(n - 1) = mlT.anyState(mlc(n - 1))
    var k = n - 2
    while k >= 0 do
      llS(k) = llT.from(llc(k), llS(k + 1)); ofS(k) = ofT.from(ofc(k), ofS(k + 1)); mlS(k) = mlT.from(mlc(k), mlS(k + 1))
      k -= 1
    // the bits, in the reverse of the decoder's reading order
    val w = BitWriter(n * 8 + 16)
    def extras(k: Int): Unit =
      w.write(seqs.ll(k) - LlBase(llc(k)), LlBits(llc(k)))
      w.write(seqs.ml(k) - MlBase(mlc(k)), MlBits(mlc(k)))
      w.write(seqs.ov(k) - (1 << ofc(k)), ofc(k))
    extras(n - 1)
    k = n - 2
    while k >= 0 do
      w.write(ofS(k + 1) - ofT.base(ofS(k)), ofT.bits(ofS(k)))
      w.write(mlS(k + 1) - mlT.base(mlS(k)), mlT.bits(mlS(k)))
      w.write(llS(k + 1) - llT.base(llS(k)), llT.bits(llS(k)))
      extras(k)
      k -= 1
    w.write(mlS(0), mlT.log); w.write(ofS(0), ofT.log); w.write(llS(0), llT.log)
    w.close(out)

/** a forward bit writer whose stream a [[BackBits]] reads from the end:
 * bits appended LSB first, closed with a 1 bit and zero padding */
private[compress] final class BitWriter(capacity: Int):
  private val out = Out(capacity)
  private var acc = 0L
  private var nacc = 0
  def write(v: Long, n: Int): Unit =
    if n > 0 then
      acc |= (v & ((1L << n) - 1)) << nacc
      nacc += n
      while nacc >= 8 do { out.byte(acc.toInt); acc >>>= 8; nacc -= 8 }
  def write(v: Int, n: Int): Unit = write(v.toLong, n)
  def close(into: Out): Unit =
    write(1, 1)
    if nacc > 0 then { out.byte(acc.toInt); acc = 0; nacc = 0 }
    into.bytes(out.buf, 0, out.n)
  def closed(): Array[Byte] = { val o = Out(out.n + 2); close(o); o.result() }

/** an FSE ENCODING table, derived from the decoding table: for each symbol
 * and each state the decoder should reach next, the state it must come from */
private[compress] final class FseEncoder(val log: Int, val base: Array[Int], val bits: Array[Int], owner: Array[Array[Int]],
                                         firstOf: Array[Int]):
  def from(symbol: Int, nextState: Int): Int = owner(symbol)(nextState)
  def anyState(symbol: Int): Int = firstOf(symbol)

private[compress] object FseEncoder:
  val LlDefaultNorm: Array[Int] = Array(4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1, 1,
    -1, -1, -1, -1)
  val MlDefaultNorm: Array[Int] = Array(1, 4, 3, 2, 2, 2, 2, 2, 2) ++ Array.fill(37)(1) ++ Array.fill(7)(-1)
  val OfDefaultNorm: Array[Int] = Array(1, 1, 1, 1, 1, 1, 2, 2, 2) ++ Array.fill(15)(1) ++ Array.fill(5)(-1)

  /** the encoder of a distribution, through the decoder's own table */
  def of(norm: Array[Int], log: Int): FseEncoder =
    val d = Fse.build(norm, log)
    val size = 1 << log
    val symbols = norm.length
    val base = Array.tabulate(size)(d.baseAt)
    val bits = Array.tabulate(size)(d.bitsAt)
    val owner = Array.fill(symbols)(Array.emptyIntArray)
    val firstOf = Array.fill(symbols)(-1)
    var u = 0
    while u < size do
      val s = d.symbol(u)
      if owner(s).isEmpty then owner(s) = new Array[Int](size)
      if firstOf(s) < 0 then firstOf(s) = u
      val from = base(u)
      val to = from + (1 << bits(u))
      var x = from
      while x < to do { owner(s)(x) = u; x += 1 }
      u += 1
    FseEncoder(log, base, bits, owner, firstOf)

  private val cache = scala.collection.mutable.Map.empty[Int, FseEncoder]
  private def predefined(which: Int, norm: Array[Int], log: Int): FseEncoder =
    cache.synchronized(cache.getOrElseUpdate(which, of(norm, log)))

  /** the mode (0 predefined, 1 RLE, 2 own table), its encoder and the
   * table's description: whichever codes these symbols in fewer bits */
  def choose(syms: Array[Int], maxSymbol: Int, maxLog: Int, dfltNorm: Array[Int], dfltLog: Int)
      : (Int, FseEncoder, Array[Byte]) =
    val counts = new Array[Int](maxSymbol + 1)
    syms.foreach(s => counts(s) += 1)
    val used = counts.count(_ > 0)
    val predefinedOk = syms.forall(s => s < dfltNorm.length && dfltNorm(s) != 0)
    if used == 1 then
      val s = syms(0)
      (1, of(Array.tabulate(s + 1)(k => if k == s then 1 else 0), 0), Array(s.toByte))
    else
      val pre = if predefinedOk then predefined(dfltNorm.length, dfltNorm, dfltLog) else null
      val preCost = if pre == null then Long.MaxValue else cost(counts, dfltNorm, dfltLog)
      // big enough for every symbol present to hold a cell, at most the stream's maximum
      val need = 32 - Integer.numberOfLeadingZeros(used)
      val log = math.min(maxLog, math.max(math.max(5, need + 1), 32 - Integer.numberOfLeadingZeros(syms.length) - 1))
      val norm = normalise(counts, log)
      val desc = describe(norm, log)
      val ownCost = cost(counts, norm, log) + desc.length * 8L
      if ownCost < preCost then (2, of(norm, log), desc)
      else (0, pre, Array.emptyByteArray)

  /** the bits a distribution spends on these counts: -log2 of each symbol's share */
  private def cost(counts: Array[Int], norm: Array[Int], log: Int): Long =
    var bits = 0.0
    var s = 0
    while s < counts.length do
      if counts(s) > 0 then
        val p = if s < norm.length then (if norm(s) == -1 then 1 else norm(s)) else 0
        if p == 0 then return Long.MaxValue
        bits += counts(s) * (log - math.log(p.toDouble) / math.log(2))
      s += 1
    bits.toLong

  /** counts scaled to sum to 2^log, every present symbol at least 1 */
  private[compress] def normalise(counts: Array[Int], log: Int): Array[Int] =
    val total = counts.sum.toLong
    val size = 1 << log
    val last = counts.lastIndexWhere(_ > 0)
    val norm = new Array[Int](last + 1)
    var assigned = 0
    var s = 0
    while s <= last do
      if counts(s) > 0 then
        norm(s) = math.max(1, ((counts(s).toLong * size) / total).toInt)
        assigned += norm(s)
      s += 1
    // the remainder to the largest, or taken from the largest
    var diff = size - assigned
    while diff != 0 do
      val big = norm.indices.maxBy(norm)
      if diff > 0 then { norm(big) += diff; diff = 0 }
      else
        val take = math.min(-diff, norm(big) - 1)
        if take == 0 then throw IllegalStateException("cannot normalise these counts")
        norm(big) -= take; diff += take
    norm

  /** a distribution's table description (RFC 8878 4.1.1), the writer's side
   * of `Fse.read` */
  private[compress] def describe(norm: Array[Int], log: Int): Array[Byte] =
    val w = Out(64)
    var acc = 0L
    var nacc = 0
    def put(v: Int, n: Int): Unit =
      acc |= (v.toLong & ((1L << n) - 1)) << nacc
      nacc += n
      while nacc >= 8 do { w.byte(acc.toInt); acc >>>= 8; nacc -= 8 }
    put(log - 5, 4)
    var remaining = (1 << log) + 1
    var threshold = 1 << log
    var nb = log + 1
    var s = 0
    while remaining > 1 && s < norm.length do
      val count = norm(s) + 1                          // the written value: prob + 1
      val max = (2 * threshold - 1) - remaining
      if count < max then put(count, nb - 1)
      else if count < threshold then put(count, nb)
      else put(count + max, nb)
      remaining -= math.abs(norm(s))
      s += 1
      while remaining < threshold do { nb -= 1; threshold >>= 1 }
      if norm(s - 1) == 0 then
        // a run of zeros: in 2-bit counts, 3 meaning "three more and go on"
        var zeros = 0
        while s < norm.length && norm(s) == 0 do { zeros += 1; s += 1 }
        while zeros >= 3 do { put(3, 2); zeros -= 3 }
        put(zeros, 2)
    if nacc > 0 then w.byte(acc.toInt)
    w.result()

/** Huffman-coded literals (RFC 8878 4.2), the encoder's side of [[Huffman]] */
private[compress] object HuffmanEncoder:
  /** the literals section (header, tree description, streams), or null
   * when Huffman cannot code these bytes (one symbol, or too few) */
  def encode(lit: Array[Byte], n: Int): Array[Byte] =
    if n < 64 then return null
    val counts = new Array[Int](256)
    var i = 0
    while i < n do { counts(lit(i) & 0xff) += 1; i += 1 }
    val present = counts.count(_ > 0)
    if present < 2 then return null
    val lens = lengths(counts, Huffman.MaxBits)
    val maxBits = lens.max
    // the decoder infers the last weight from a COMPLETE code: anything else is not written
    if lens.foldLeft(0L)((k, l) => if l > 0 then k + (1L << (maxBits - l)) else k) != (1L << maxBits) then return null
    // weights: maxBits + 1 - length; the last present symbol's is implied
    val lastSym = counts.lastIndexWhere(_ > 0)
    val weights = Array.tabulate(lastSym)(s => if lens(s) == 0 then 0 else maxBits + 1 - lens(s))
    if weights.length > 128 || weights.length == 0 then return null   // the direct form holds 128 weights
    val tree = Out(1 + (weights.length + 1) / 2)
    tree.byte(127 + weights.length)
    var k = 0
    while k < weights.length do
      val hi = weights(k)
      val lo = if k + 1 < weights.length then weights(k + 1) else 0
      tree.byte((hi << 4) | lo)
      k += 2
    // canonical codes, as the decoder's table assigns them
    val codes = canonical(lens, maxBits)
    val four = n > 1024
    val streams =
      if !four then Vector(stream(lit, 0, n, lens, codes))
      else
        val q = (n + 3) / 4
        Vector(stream(lit, 0, q, lens, codes), stream(lit, q, q, lens, codes),
          stream(lit, 2 * q, q, lens, codes), stream(lit, 3 * q, n - 3 * q, lens, codes))
    if four && streams.take(3).exists(_.length > 0xffff) then return null
    val comp = tree.n + streams.map(_.length).sum + (if four then 6 else 0)
    val out = Out(comp + 5)
    // the header: compressed type 2; a single stream only with 10-bit sizes,
    // four streams with 10, 14 or 18
    if !four then
      if n >= 1024 || comp >= 1024 then return null
      val v = 2 | (0 << 2) | (n << 4) | (comp << 14)
      out.byte(v); out.byte(v >>> 8); out.byte(v >>> 16)
    else if n < 1024 && comp < 1024 then
      val v = 2 | (1 << 2) | (n << 4) | (comp << 14)
      out.byte(v); out.byte(v >>> 8); out.byte(v >>> 16)
    else if n < 16384 && comp < 16384 then
      val v = 2L | (2L << 2) | (n.toLong << 4) | (comp.toLong << 18)
      out.byte(v.toInt); out.byte((v >>> 8).toInt); out.byte((v >>> 16).toInt); out.byte((v >>> 24).toInt)
    else if n < (1 << 18) && comp < (1 << 18) then
      val v = 2L | (3L << 2) | (n.toLong << 4) | (comp.toLong << 22)
      out.byte(v.toInt); out.byte((v >>> 8).toInt); out.byte((v >>> 16).toInt); out.byte((v >>> 24).toInt); out.byte((v >>> 32).toInt)
    else return null
    out.bytes(tree.buf, 0, tree.n)
    if four then
      Vector(0, 1, 2).foreach { j => out.byte(streams(j).length); out.byte(streams(j).length >>> 8) }
    streams.foreach(s => out.bytes(s, 0, s.length))
    out.result()

  /** one stream, written so a [[BackBits]] reads its first literal first:
   * the literals are written LAST first */
  private def stream(lit: Array[Byte], from: Int, n: Int, lens: Array[Int], codes: Array[Int]): Array[Byte] =
    val w = BitWriter(n + 8)
    var i = from + n - 1
    while i >= from do
      val s = lit(i) & 0xff
      w.write(codes(s), lens(s))
      i -= 1
    w.closed()

  /** code lengths, at most `limit` bits: a Huffman tree, then the lengths
   * past the limit folded back (a Kraft sum kept at exactly 1) */
  private[compress] def lengths(counts: Array[Int], limit: Int): Array[Int] =
    val syms = counts.indices.filter(counts(_) > 0).toArray
    val lens = new Array[Int](256)
    if syms.length == 1 then { lens(syms(0)) = 1; return lens }
    // the tree by repeatedly joining the two lightest
    val pq = scala.collection.mutable.PriorityQueue.empty[(Long, Int)](using Ordering.by[(Long, Int), Long](_._1).reverse)
    val parent = new Array[Int](syms.length * 2)
    syms.indices.foreach(k => pq.enqueue((counts(syms(k)).toLong, k)))
    var next = syms.length
    while pq.size > 1 do
      val (w1, a) = pq.dequeue(); val (w2, b) = pq.dequeue()
      parent(a) = next; parent(b) = next
      pq.enqueue((w1 + w2, next))
      next += 1
    val root = next - 1
    syms.indices.foreach { k =>
      var d = 0; var x = k
      while x != root do { x = parent(x); d += 1 }
      lens(syms(k)) = d
    }
    // limit the depth: clamp, then repay the Kraft debt from the shallowest
    if lens.max > limit then
      syms.foreach(s => if lens(s) > limit then lens(s) = limit)
      def kraft = syms.foldLeft(0L)((acc, s) => acc + (1L << (limit - lens(s))))
      val full = 1L << limit
      var k = kraft
      // lengthen the longest codes that are shorter than the limit until it fits
      val order = syms.sortBy(s => (-lens(s), counts(s)))
      while k > full do
        val s = order.find(s => lens(s) < limit).getOrElse(throw IllegalStateException("cannot limit the code"))
        k -= 1L << (limit - lens(s) - 1)
        lens(s) += 1
      // spend a surplus on the most frequent symbols
      val byFreq = syms.sortBy(s => -counts(s))
      var progress = true
      while k < full && progress do
        progress = false
        for s <- byFreq do
          if lens(s) > 1 && k + (1L << (limit - lens(s))) <= full then
            k += 1L << (limit - lens(s)); lens(s) -= 1; progress = true
    // the decoder needs the Kraft sum exactly 1: the implied last weight completes it
    lens

  /** the codes the decoder's table gives: weight ascending (longest codes
   * first), symbols in order within a weight */
  private def canonical(lens: Array[Int], maxBits: Int): Array[Int] =
    val codes = new Array[Int](256)
    var next = 0
    var l = maxBits
    while l >= 1 do
      var s = 0
      while s < 256 do
        if lens(s) == l then
          // the decoder's index of this symbol's first entry, read as `maxBits` bits
          codes(s) = next >>> (maxBits - l)
          next += 1 << (maxBits - l)
        s += 1
      l -= 1
    codes
