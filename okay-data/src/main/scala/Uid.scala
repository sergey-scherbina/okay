package okay

/**
 * A SORTABLE 128-BIT IDENTITY (specs/coordination-free.md): issued
 * locally, ordered by time, spelled either as a ULID or as a UUID.
 *
 * WHY NOT `UUID.randomUUID()`. Version 4 is random by construction,
 * which is the one property a key in a log-first store must not have:
 * it destroys locality in every B-tree it touches, and it cannot
 * answer "what happened between these two moments" without a
 * secondary index. This library keeps the journal as truth and derives
 * the relational store from it, so a key that carries its own time is
 * not decoration — it is what makes a range scan possible at all.
 *
 * ONE VALUE, TWO SPELLINGS, which is the whole design. A ULID and a
 * UUIDv7 are the same shape: 48 bits of Unix milliseconds, then
 * entropy. They differ in six bits — UUIDv7 spends four on a version
 * and two on a variant (RFC 9562) — and in how they are written:
 * Crockford base32, 26 characters, against hyphenated hex, 36. Making
 * them two types would write the hard part (monotonicity) twice and
 * let the two drift. So the canonical value here IS a conforming
 * UUIDv7, and `ulid` is that same 128 bits in base32.
 *
 * THE LAYOUT, and why order survives it:
 *
 * {{{
 * hi  bits 63..16   unix_ts_ms      48   the physical clock
 *     bits 15..12   version = 7      4   constant
 *     bits 11..0    counter         12   rand_a, used as a counter
 * lo  bits 63..62   variant = 0b10   2   constant
 *     bits 61..0    random          62   rand_b
 * }}}
 *
 * Sorting compares hi then lo, unsigned. The version and variant bits
 * are CONSTANT, so they contribute nothing to the comparison and the
 * order is: time, then counter, then randomness. RFC 9562 section 6.2
 * names this use of `rand_a` — a monotonic counter — explicitly, so
 * the sortability is bought inside the standard rather than beside it.
 *
 * WHAT IT COSTS: 62 bits of randomness where a plain ULID has 80. The
 * 18 bits go to the version, the variant and the counter, and what
 * they buy is STRICT monotonicity — two ids from the same millisecond
 * are ordered, not merely distinct. For a key that is also a sort
 * order, that is the better trade, and an id that only needs to be
 * unguessable should not be this type at all.
 *
 * THE COUNTER IS 12 BITS, so 4 096 ids fit in a millisecond before the
 * clock borrows the next one — `Hlc.at(source, counterBits = 12)`
 * does the borrowing, and it is the same clock an LWW register uses.
 * That is the arc's one shared primitive doing its second job.
 */
final case class Uid(hi: Long, lo: Long):
  /** the Unix millisecond this id was issued in */
  def millis: Long = hi >>> 16
  /** which id within that millisecond: 0..4095 */
  def counter: Int = (hi & 0xFFFL).toInt
  /** 26 characters, Crockford base32, sorts as the value does */
  def ulid: String = Uid.renderUlid(this)
  /** 36 characters, RFC 9562 version 7 */
  def uuid: String = Uid.renderUuid(this)
  override def toString: String = ulid

object Uid:

  /** Crockford's alphabet: no I, L, O or U, so nothing reads as
   * something else when a human retypes it */
  private final val Alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

  private final val Decode: Array[Byte] =
    val a = Array.fill[Byte](128)(-1)
    var i = 0
    while i < Alphabet.length do
      a(Alphabet.charAt(i).toInt) = i.toByte
      // Crockford: a human who types I or L means 1, and O means 0
      i += 1
    a('i'.toInt) = 1; a('I'.toInt) = 1
    a('l'.toInt) = 1; a('L'.toInt) = 1
    a('o'.toInt) = 0; a('O'.toInt) = 0
    var j = 0
    while j < Alphabet.length do
      val c = Alphabet.charAt(j)
      if c.isUpper then a(c.toLower.toInt) = j.toByte
      j += 1
    a

  private final val Hex = "0123456789abcdef"

  /**
   * The generator. Clock and randomness are both PARAMETERS: the
   * hazards this type exists to survive — a clock stepping backwards,
   * and two ids inside one millisecond — are only reachable from a
   * test if the test can move them.
   */
  final class Gen private[okay] (clock: Hlc.Clock, random: () => Long):
    def next(): Uid =
      val s = clock.next()
      val hi = (s.millis << 16) | (7L << 12) | (s.counter.toLong & 0xFFFL)
      val lo = (2L << 62) | (random() & 0x3FFFFFFFFFFFFFFFL)
      Uid(hi, lo)

  /** a generator over a millisecond source and an entropy source */
  def at(source: () => Long, random: () => Long = () => scala.util.Random.nextLong()): Gen =
    Gen(Hlc.at(source, counterBits = 12), random)

  /** the ambient generator: the system clock, `scala.util.Random`.
   * NOT a cryptographic source — a UUIDv7 is unique, not unguessable,
   * and a secret should be made by okay-security instead. */
  val system: Gen = Gen(Hlc.at(() => System.currentTimeMillis(), counterBits = 12),
                        () => scala.util.Random.nextLong())

  /** the next id from the ambient generator */
  def next(): Uid = system.next()

  /** hi then lo, both UNSIGNED — the variant bit sets lo's sign bit on
   * every id there will ever be, so a signed compare of `lo` would
   * order them backwards */
  given Ordering[Uid] with
    def compare(a: Uid, b: Uid): Int =
      val h = java.lang.Long.compare(a.hi ^ Long.MinValue, b.hi ^ Long.MinValue)
      if h != 0 then h
      else java.lang.Long.compare(a.lo ^ Long.MinValue, b.lo ^ Long.MinValue)

  // ── spelling ───────────────────────────────────────────────────

  /** five bits at `shift` counted from the low end of the 128 */
  private def fiveAt(hi: Long, lo: Long, shift: Int): Int =
    if shift >= 64 then ((hi >>> (shift - 64)) & 31L).toInt
    else if shift <= 59 then ((lo >>> shift) & 31L).toInt
    else (((lo >>> shift) | (hi << (64 - shift))) & 31L).toInt

  private def renderUlid(u: Uid): String =
    val out = new Array[Char](26)
    var i = 0
    while i < 26 do
      // char 0 holds the top: 26 * 5 = 130 bits over a 128-bit value,
      // so the first character carries three significant bits and can
      // never exceed '7'
      val shift = (25 - i) * 5
      out(i) = Alphabet.charAt(fiveAt(u.hi, u.lo, shift))
      i += 1
    new String(out)

  private def renderUuid(u: Uid): String =
    val out = new Array[Char](36)
    var p = 0
    def nib(v: Long, from: Int, count: Int): Unit =
      var k = from
      var n = 0
      while n < count do
        out(p) = Hex.charAt(((v >>> (k * 4)) & 0xFL).toInt)
        p += 1; k -= 1; n += 1
    nib(u.hi, 15, 8); out(p) = '-'; p += 1     // time_low
    nib(u.hi, 7, 4);  out(p) = '-'; p += 1     // time_mid
    nib(u.hi, 3, 4);  out(p) = '-'; p += 1     // version + rand_a
    nib(u.lo, 15, 4); out(p) = '-'; p += 1     // variant + rand_b
    nib(u.lo, 11, 12)
    new String(out)

  /** 26 Crockford characters, or None. Accepts lower case, and the
   * I/L/O confusions Crockford prescribes. */
  def parseUlid(s: String): Option[Uid] =
    if s.length != 26 then None
    // 26 characters carry 130 bits over a 128-bit value, so the first
    // one has three significant bits: anything above '7' does not fit
    // and would silently shift the top of the value into nowhere
    else if { val d = if s.charAt(0) < 128 then Decode(s.charAt(0).toInt).toInt else -1
              d < 0 || d > 7 } then None
    else
      var hi = 0L
      var lo = 0L
      var i = 0
      var ok = true
      while i < 26 && ok do
        val c = s.charAt(i).toInt
        val v = if c < 0 || c > 127 then -1 else Decode(c).toInt
        if v < 0 then ok = false
        else
          // shift the whole 128 left by five and drop `v` in at the
          // bottom: hi takes what leaves lo's top
          hi = (hi << 5) | (lo >>> 59)
          lo = (lo << 5) | v.toLong
          i += 1
      if !ok then None else Some(Uid(hi, lo))

  /** 36 characters with hyphens, or 32 without, or None */
  def parseUuid(s: String): Option[Uid] =
    val t = if s.length == 36 then s.replace("-", "") else s
    if t.length != 32 then None
    else
      var hi = 0L
      var lo = 0L
      var i = 0
      var ok = true
      while i < 32 && ok do
        val c = Character.digit(t.charAt(i), 16)
        if c < 0 then ok = false
        else
          if i < 16 then hi = (hi << 4) | c.toLong
          else lo = (lo << 4) | c.toLong
          i += 1
      if !ok then None else Some(Uid(hi, lo))
