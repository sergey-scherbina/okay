package okay.scalus

/**
 * A CBOR value, just enough for the node-to-node MESSAGES (never for a
 * block's body, which is scalus's to decode). Two things okay-codec's
 * `Cbor.In` does not do and a socket needs: tell an INCOMPLETE item
 * from a malformed one — a message may span several mux segments — and
 * read indefinite-length containers, which a Haskell node may send.
 */
enum Cv:
  case UInt(n: BigInt)
  case NInt(n: BigInt)            // the value itself, negative
  case Bytes(b: Array[Byte])
  case Text(s: String)
  case Arr(items: Vector[Cv])
  case Map(pairs: Vector[(Cv, Cv)])
  case Tag(tag: Long, of: Cv, raw: Array[Byte])   // raw: the tagged item's own bytes
  case Bool(b: Boolean)
  case Null

object Cv:
  /** a decode that ran out of bytes is `Incomplete`, not an error */
  enum Read:
    case Done(value: Cv, length: Int)
    case Incomplete
    case Bad(reason: String)

  // the decoder's two exits from deep inside a nested read: out of
  // bytes (the item may complete with the next segment), or not CBOR
  // the node protocols use. Stackless: they carry no trace, only which.
  private final class Short extends RuntimeException(null, null, false, false)
  private final class Malformed(m: String) extends RuntimeException(m, null, false, false)

  /** decode one item from `bs` at `at` */
  def read(bs: Array[Byte], at: Int = 0): Read =
    var i = at
    def byte(): Int =
      if i >= bs.length then throw Short()
      val b = bs(i) & 0xFF; i += 1; b
    def arg(info: Int): BigInt = info match
      case n if n < 24 => BigInt(n)
      case 24 | 25 | 26 | 27 =>
        val k = 1 << (info - 24)
        if i + k > bs.length then throw Short()
        val v = BigInt(1, bs.slice(i, i + k)); i += k; v
      case 31 => BigInt(-1)                        // indefinite
      case n => throw Malformed(s"reserved additional info $n")
    def len(n: BigInt): Int =
      if n > bs.length - i then throw Short()     // a length the bytes cannot hold yet
      n.toInt
    def item(): Cv =
      val b = byte()
      val major = b >> 5
      val a = arg(b & 0x1F)
      major match
        case 0 => UInt(a)
        case 1 => NInt(-1 - a)
        case 2 | 3 if a < 0 => throw Malformed("indefinite-length strings are not used by the node protocols")
        case 2 => val n = len(a); val v = bs.slice(i, i + n); i += n; Bytes(v)
        case 3 => val n = len(a); val v = String(bs.slice(i, i + n), "UTF-8"); i += n; Text(v)
        case 4 =>
          if a < 0 then Arr(Vector.unfold(())(_ => if peekBreak() then None else Some((item(), ()))))
          else Arr(Vector.fill(len(a))(item()))
        case 5 =>
          if a < 0 then Map(Vector.unfold(())(_ => if peekBreak() then None else Some(((item(), item()), ()))))
          else Map(Vector.fill(len(a))((item(), item())))
        case 6 =>
          val from = i
          val v = item()
          Tag(a.toLong, v, bs.slice(from, i))
        case _ => (b & 0x1F) match
          case 20 => Bool(false)
          case 21 => Bool(true)
          case 22 => Null
          case n => throw Malformed(s"unsupported simple value $n")
    def peekBreak(): Boolean =
      if i >= bs.length then throw Short()
      if (bs(i) & 0xFF) == 0xFF then { i += 1; true } else false
    try
      val v = item()
      Read.Done(v, i - at)
    catch
      case _: Short => Read.Incomplete
      case m: Malformed => Read.Bad(m.getMessage)
