package okay2

/**
 * A SORTABLE 128-BIT IDENTITY — the Scala 3 core's okay-data `Uid`:
 * issued locally, ordered by time, spelled either as a ULID or as a
 * UUID. One value, two spellings: the canonical value IS a conforming
 * UUIDv7 (RFC 9562), and `ulid` is the same 128 bits in Crockford
 * base32.
 *
 * {{{
 * hi  bits 63..16   unix_ts_ms      48   the physical clock
 *     bits 15..12   version = 7      4   constant
 *     bits 11..0    counter         12   rand_a, used as a counter
 * lo  bits 63..62   variant = 0b10   2   constant
 *     bits 61..0    random          62   rand_b
 * }}}
 *
 * Sorting compares hi then lo, unsigned; the constant bits contribute
 * nothing, so the order is time, then counter, then randomness. 4 096
 * ids fit in a millisecond before the clock (`Hlc.at(source, 12)`)
 * borrows the next. An id that only needs to be unguessable should not
 * be this type: 62 bits of randomness, and not a cryptographic source.
 */
final case class Uid(hi: Long, lo: Long) {
  /** the Unix millisecond this id was issued in */
  def millis: Long = hi >>> 16
  /** which id within that millisecond: 0..4095 */
  def counter: Int = (hi & 0xFFFL).toInt
  /** 26 characters, Crockford base32, sorts as the value does */
  def ulid: String = Uid.renderUlid(this)
  /** 36 characters, RFC 9562 version 7 */
  def uuid: String = Uid.renderUuid(this)
  override def toString: String = ulid
}

object Uid {

  /** Crockford's alphabet: no I, L, O or U */
  private final val Alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

  private val Decode: Array[Byte] = {
    val a = Array.fill[Byte](128)(-1)
    var i = 0
    while (i < Alphabet.length) {
      val c = Alphabet.charAt(i)
      a(c.toInt) = i.toByte
      if (c.isUpper) a(c.toLower.toInt) = i.toByte
      i += 1
    }
    // Crockford: a human who types I or L means 1, and O means 0
    a('i'.toInt) = 1; a('I'.toInt) = 1
    a('l'.toInt) = 1; a('L'.toInt) = 1
    a('o'.toInt) = 0; a('O'.toInt) = 0
    a
  }

  private final val Hex = "0123456789abcdef"

  /** the generator: clock and randomness are both PARAMETERS, so the
   * hazards — a clock stepping back, two ids in one millisecond — are
   * reachable from a test */
  final class Gen private[okay2] (clock: Hlc.Clock, random: () => Long) {
    def next(): Uid = {
      val s = clock.next()
      val hi = (s.millis << 16) | (7L << 12) | (s.counter.toLong & 0xFFFL)
      val lo = (2L << 62) | (random() & 0x3FFFFFFFFFFFFFFFL)
      Uid(hi, lo)
    }
  }

  /** a generator over a millisecond source and an entropy source */
  def at(source: () => Long, random: () => Long = () => scala.util.Random.nextLong()): Gen =
    new Gen(Hlc.at(source, counterBits = 12), random)

  /** the ambient generator: the system clock, `scala.util.Random` */
  val system: Gen = new Gen(Hlc.at(() => System.currentTimeMillis(), counterBits = 12), () => scala.util.Random.nextLong())

  /** the next id from the ambient generator */
  def next(): Uid = system.next()

  /** hi then lo, both UNSIGNED — the variant bit sets lo's sign bit on
   * every id, so a signed compare of `lo` would order them backwards */
  implicit val ordering: Ordering[Uid] = new Ordering[Uid] {
    def compare(a: Uid, b: Uid): Int = {
      val h = java.lang.Long.compare(a.hi ^ Long.MinValue, b.hi ^ Long.MinValue)
      if (h != 0) h else java.lang.Long.compare(a.lo ^ Long.MinValue, b.lo ^ Long.MinValue)
    }
  }

  /** five bits at `shift` counted from the low end of the 128 */
  private def fiveAt(hi: Long, lo: Long, shift: Int): Int =
    if (shift >= 64) ((hi >>> (shift - 64)) & 31L).toInt
    else if (shift <= 59) ((lo >>> shift) & 31L).toInt
    else (((lo >>> shift) | (hi << (64 - shift))) & 31L).toInt

  private def renderUlid(u: Uid): String = {
    val out = new Array[Char](26)
    var i = 0
    // 26 * 5 = 130 bits over 128: the first character never exceeds '7'
    while (i < 26) { out(i) = Alphabet.charAt(fiveAt(u.hi, u.lo, (25 - i) * 5)); i += 1 }
    new String(out)
  }

  private def renderUuid(u: Uid): String = {
    val out = new Array[Char](36)
    var p = 0
    def nib(v: Long, from: Int, count: Int): Unit = {
      var k = from
      var n = 0
      while (n < count) { out(p) = Hex.charAt(((v >>> (k * 4)) & 0xFL).toInt); p += 1; k -= 1; n += 1 }
    }
    nib(u.hi, 15, 8); out(p) = '-'; p += 1     // time_low
    nib(u.hi, 7, 4);  out(p) = '-'; p += 1     // time_mid
    nib(u.hi, 3, 4);  out(p) = '-'; p += 1     // version + rand_a
    nib(u.lo, 15, 4); out(p) = '-'; p += 1     // variant + rand_b
    nib(u.lo, 11, 12)
    new String(out)
  }

  private def digit(c: Char): Int = if (c < 128) Decode(c.toInt).toInt else -1

  /** 26 Crockford characters, or None; lower case and the I/L/O
   * confusions accepted */
  def parseUlid(s: String): Option[Uid] =
    if (s.length != 26) None
    // the first character has three significant bits: above '7' would
    // shift the top of the value into nowhere
    else if (digit(s.charAt(0)) < 0 || digit(s.charAt(0)) > 7) None
    else {
      var hi = 0L
      var lo = 0L
      var i = 0
      var ok = true
      while (i < 26 && ok) {
        val v = digit(s.charAt(i))
        if (v < 0) ok = false
        else {
          hi = (hi << 5) | (lo >>> 59)
          lo = (lo << 5) | v.toLong
          i += 1
        }
      }
      if (ok) Some(Uid(hi, lo)) else None
    }

  /** 36 characters with hyphens, or 32 without, or None */
  def parseUuid(s: String): Option[Uid] = {
    val t = if (s.length == 36) s.replace("-", "") else s
    if (t.length != 32) None
    else {
      var hi = 0L
      var lo = 0L
      var i = 0
      var ok = true
      while (i < 32 && ok) {
        val c = Character.digit(t.charAt(i), 16)
        if (c < 0) ok = false
        else {
          if (i < 16) hi = (hi << 4) | c.toLong else lo = (lo << 4) | c.toLong
          i += 1
        }
      }
      if (ok) Some(Uid(hi, lo)) else None
    }
  }
}
