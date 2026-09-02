package okay.codec

import scala.collection.mutable.ArrayBuffer

/**
 * CBOR (RFC 8949) as the second algebra over the SAME Schema: what
 * JSON renders as text, CBOR renders as typed binary items — one
 * derived Schema serves both, which is the cross-format contract of
 * specs/codecs.md. Products are maps keyed by field names, sums are
 * one-entry maps keyed by case name, None is null — the same
 * semantic content as the JSON dialect, so the two decode to equal
 * values. Decoding is total in the value: errors come back as Left,
 * never as a throw.
 *
 * `Out` and `In` are the item-level primitives (the major-type
 * header, an integer, a length-prefixed string or byte string) named
 * and made public so a SECOND writer of this format — the staged
 * fold in Staged.scala — calls exactly what this one does, not a
 * reimplementation of RFC 8949's varint encoding.
 */
object Cbor {

  // ---------------------------------------------------------------- encode

  /** the CBOR item primitives, once: `put` below and Staged.scala's
   * generated encoder both call only these */
  final class Out:
    private val buf = ArrayBuffer[Byte]()

    def header(major: Int, n: Long): Unit =
      val m = major << 5
      if n < 24 then buf += (m | n.toInt).toByte
      else if n < 256 then { buf += (m | 24).toByte; buf += n.toByte }
      else if n < 65536 then
        buf += (m | 25).toByte
        buf += (n >> 8).toByte; buf += n.toByte
      else if n < (1L << 32) then
        buf += (m | 26).toByte
        var i = 24
        while i >= 0 do { buf += (n >> i).toByte; i -= 8 }
      else
        buf += (m | 27).toByte
        var i = 56
        while i >= 0 do { buf += (n >> i).toByte; i -= 8 }

    def integer(n: Long): Unit =
      if n >= 0 then header(0, n) else header(1, -1 - n)

    def text(s: String): Unit =
      val bs = s.getBytes("UTF-8")
      header(3, bs.length.toLong)
      buf ++= bs

    def byteString(bs: Array[Byte]): Unit =
      header(2, bs.length.toLong)
      buf ++= bs

    def double(d: Double): Unit =
      buf += 0xFB.toByte
      val bits = java.lang.Double.doubleToLongBits(d)
      var i = 56
      while i >= 0 do { buf += (bits >> i).toByte; i -= 8 }

    def bool(b: Boolean): Unit = buf += (if b then 0xF5 else 0xF4).toByte
    def nul(): Unit = buf += 0xF6.toByte
    def arrayHeader(n: Long): Unit = header(4, n)
    def mapHeader(n: Long): Unit = header(5, n)

    def toArray: Array[Byte] = buf.toArray

  private def put[A](out: Out, s: Schema[A], a: A): Unit = s match
    case Schema.SIso(u, _, from) => put(out, u(), from(a))
    case Schema.SInt => out.integer(a.toLong)
    case Schema.SLong => out.integer(a)
    case Schema.SDouble => out.double(a)
    case Schema.SBool => out.bool(a)
    case Schema.SString => out.text(a)
    case Schema.SChar => out.text(a.toString)
    // major type 2: a byte string, which is what CBOR is for
    case Schema.SBytes => out.byteString(a)
    case Schema.SOption(of) => a match
      case None => out.nul()
      case Some(x) => put(out, of(), x)
    case Schema.SList(of) =>
      out.arrayHeader(a.length.toLong)
      a.foreach(put(out, of(), _))
    case Schema.SVector(of) =>
      out.arrayHeader(a.length.toLong)
      a.foreach(put(out, of(), _))
    case p: Schema.SProduct[A] =>
      out.mapHeader(p.fields.length.toLong)
      p.eachField(a)([X] => (n: String, sc: Schema[X], x: X) => { out.text(n); put(out, sc, x) }): Unit
    case su: Schema.SSum[A] =>
      out.mapHeader(1)
      su.theCase(a)([X <: A] => (n: String, sc: Schema[X], x: X) => { out.text(n); put(out, sc, x) })

  /** value to bytes in one move */
  def write[A](a: A)(using s: Schema[A]): Array[Byte] =
    val out = new Out
    put(out, s, a)
    out.toArray

  // ---------------------------------------------------------------- decode

  /** the CBOR item primitives on the read side, once: `get` below and
   * Staged.scala's generated decoder both call only these */
  final class In(bs: Array[Byte]):
    private var i = 0
    def peek: Int = if i < bs.length then bs(i) & 0xFF else -1
    def byte(): Either[String, Int] =
      if i < bs.length then { val b = bs(i) & 0xFF; i += 1; Right(b) }
      else Left("truncated CBOR")
    def take(n: Int): Either[String, Array[Byte]] =
      if n >= 0 && i + n <= bs.length then { val a = bs.slice(i, i + n); i += n; Right(a) }
      else Left("truncated CBOR")
    private def long(n: Int): Either[String, Long] =
      take(n).map(_.foldLeft(0L)((acc, b) => (acc << 8) | (b & 0xFF)))

    /** the major type and the argument (a length, a count, or the
     * integer itself) — every item starts here */
    def head(): Either[String, (Int, Long)] =
      byte().flatMap { b =>
        val major = b >> 5
        (b & 0x1F) match
          case n if n < 24 => Right((major, n.toLong))
          case 24 => long(1).map((major, _))
          case 25 => long(2).map((major, _))
          case 26 => long(4).map((major, _))
          case 27 => long(8).map((major, _))
          case x => Left(s"unsupported additional info $x")
      }

    def intItem(): Either[String, Long] =
      head().flatMap {
        case (0, n) => Right(n)
        case (1, n) => Right(-1 - n)
        case (m, _) => Left(s"expected an integer, got major $m")
      }

    def textItem(): Either[String, String] =
      head().flatMap {
        case (3, n) => take(n.toInt).map(String(_, "UTF-8"))
        case (m, _) => Left(s"expected a text string, got major $m")
      }

    def doubleItem(): Either[String, Double] =
      byte().flatMap {
        case 0xFB => long(8).map(java.lang.Double.longBitsToDouble(_))
        case b => Left(f"expected a double (0xFB), got 0x$b%02X")
      }

    def boolItem(): Either[String, Boolean] =
      byte().flatMap {
        case 0xF4 => Right(false)
        case 0xF5 => Right(true)
        case b => Left(f"expected a boolean, got 0x$b%02X")
      }

    def byteStringItem(): Either[String, Array[Byte]] =
      head().flatMap {
        case (2, n) => take(n.toInt)
        case (m, _) => Left(s"expected a byte string, got major $m")
      }

    def isNull: Boolean = peek == 0xF6
    def skipNull(): Unit = byte(): Unit

    def arrayHeader(): Either[String, Long] =
      head().flatMap {
        case (4, n) => Right(n)
        case (m, _) => Left(s"expected an array, got major $m")
      }

    def mapHeader(): Either[String, Long] =
      head().flatMap {
        case (5, n) => Right(n)
        case (m, _) => Left(s"expected a map, got major $m")
      }

  private def get[A](in: In, s: Schema[A]): Either[String, A] = s match
    case Schema.SIso(u, to, _) => get(in, u()).flatMap(to)
    case Schema.SInt => in.intItem().map(_.toInt)
    case Schema.SLong => in.intItem()
    case Schema.SDouble => in.doubleItem()
    case Schema.SBool => in.boolItem()
    case Schema.SString => in.textItem()
    case Schema.SChar => in.textItem().flatMap(x =>
      if x.length == 1 then Right(x.head) else Left(s"expected one character, got ${x.length}"))
    case Schema.SBytes => in.byteStringItem()
    case Schema.SOption(of) =>
      if in.isNull then { in.skipNull(); Right(None) }
      else get(in, of()).map(Some(_))
    case l: Schema.SList[a] =>
      in.arrayHeader().flatMap { n =>
        (0L until n).foldLeft(Right(Nil): Either[String, List[a]]) { (acc, _) =>
          acc.flatMap(xs => get(in, l.of()).map(xs :+ _))
        }
      }
    case vec: Schema.SVector[a] =>
      in.arrayHeader().flatMap { n =>
        (0L until n).foldLeft(Right(Vector.empty): Either[String, Vector[a]]) { (acc, _) =>
          acc.flatMap(xs => get(in, vec.of()).map(xs :+ _))
        }
      }
    case p: Schema.SProduct[A] =>
      in.mapHeader().flatMap { n =>
        (0L until n).foldLeft(Right(Map.empty[String, Any]): Either[String, Map[String, Any]]) {
          (acc, _) =>
            acc.flatMap { m =>
              in.textItem().flatMap { k =>
                p.fields.find(_._1 == k) match
                  case Some((_, sc)) => field(in, sc()).map(v => m + (k -> v))
                  case None => Left(s"unknown field '$k' of ${p.name}")
              }
            }
        }.flatMap { m =>
          p.fields.zipWithIndex.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, fi) =>
            val (f, i) = fi
            acc.flatMap { xs =>
              (m.get(f._1), f._2()) match
                // absent: the declared default first, then
                // None-if-optional, then the refusal (codec-defaults)
                case (None, sc) => (p.defaults.lift(i).flatten, sc) match
                  case (Some(d), _) => Right(xs :+ d())
                  case (None, _: Schema.SOption[?]) => Right(xs :+ None)
                  case _ => Left(s"missing field '${f._1}' in ${p.name}")
                case (found, _) =>
                  found.toRight(s"missing field '${f._1}' in ${p.name}").map(xs :+ _)
            }
          }.map(p.make)
        }
      }
    case su: Schema.SSum[A] =>
      in.mapHeader().flatMap {
        case 1 => in.textItem().flatMap { name =>
          su.cases.find(_._1 == name)
            .toRight(s"unknown case '$name' of ${su.name}")
            .flatMap((_, sc) => get(in, sc()))
        }
        case n => Left(s"expected a one-entry map, got $n entries")
      }

  /** one field at its own type; the value joins the product's erased
   * parts (Mirror's fromProduct takes Any) */
  private def field[X](in: In, sc: Schema[X]): Either[String, Any] = get(in, sc)

  /** bytes to value in one move; errors as values */
  def read[A](bytes: Array[Byte])(using s: Schema[A]): Either[String, A] =
    get(In(bytes), s)

  /** one item at A's schema, on an ALREADY-OPEN cursor or accumulator
   * — the fallback door Staged.scala's generated code calls when a
   * node's run-time schema does not have the Mirror's shape (an Iso,
   * a hand-written instance, or the same type met again inside
   * itself), so that node still gets exactly this fold's answer */
  def encodeItem[A](out: Out, a: A)(using s: Schema[A]): Unit = put(out, s, a)
  def decodeItem[A](in: In)(using s: Schema[A]): Either[String, A] = get(in, s)
}
