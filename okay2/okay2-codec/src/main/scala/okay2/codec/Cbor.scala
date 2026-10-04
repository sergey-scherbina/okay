package okay2.codec

import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable.ArrayBuffer
import okay2.{Cont, />}

/**
 * CBOR (RFC 8949) as the second algebra over the SAME Schema
 * (okay-codec's Cbor.scala): what JSON renders as text, CBOR renders as
 * typed binary items — one derived Schema serves both. Products are
 * maps keyed by field names, sums one-entry maps keyed by case name,
 * None is null — the same content as the JSON dialect, so the two
 * decode to equal values. Decoding is total: errors come back as Left.
 *
 * `Out` and `In` are the item-level primitives, public so a second
 * writer of this format calls exactly what this one does. Every walk is
 * native below `Codecs.NativeThreshold` open containers and a
 * `Cont.defer` trampoline past it.
 */
object Cbor {

  // ---------------------------------------------------------------- encode

  /** the CBOR item primitives, once */
  final class Out {
    private val buf = ArrayBuffer[Byte]()

    /** `n` is the argument read UNSIGNED: a uint64 past 2^63 arrives
     * here as a negative Long, and a signed `n < 24` would have written
     * it as a one-byte head */
    def header(major: Int, n: Long): Unit = {
      val m = major << 5
      def below(x: Long) = java.lang.Long.compareUnsigned(n, x) < 0
      if (below(24)) buf += (m | n.toInt).toByte
      else if (below(256)) { buf += (m | 24).toByte; buf += n.toByte }
      else if (below(65536)) {
        buf += (m | 25).toByte
        buf += (n >> 8).toByte; buf += n.toByte
      } else if (below(1L << 32)) {
        buf += (m | 26).toByte
        var i = 24
        while (i >= 0) { buf += (n >> i).toByte; i -= 8 }
      } else {
        buf += (m | 27).toByte
        var i = 56
        while (i >= 0) { buf += (n >> i).toByte; i -= 8 }
      }
      ()
    }

    def integer(n: Long): Unit =
      if (n >= 0) header(0, n) else header(1, -1 - n)

    /** RFC 8949 §3.4.3 preferred serialization: a plain integer across
     * the whole 64-bit unsigned range (major 0, or major 1 for −1−v), a
     * tag 2/3 bignum over the big-endian magnitude past it — how Plutus
     * Data and the Cardano ledger write integers */
    def bigInt(v: BigInt): Unit = {
      val (major, arg) = if (v.signum >= 0) (0, v) else (1, -v - 1)
      if (arg.bitLength <= 64) header(major, arg.longValue)
      else {
        header(6, if (major == 0) 2L else 3L)
        val bs = arg.toByteArray
        byteString(if (bs.length > 1 && bs(0) == 0) bs.drop(1) else bs)
      }
    }

    def text(s: String): Unit = {
      val bs = s.getBytes(UTF_8)
      header(3, bs.length.toLong)
      buf ++= bs: Unit
    }

    def byteString(bs: Array[Byte]): Unit = {
      header(2, bs.length.toLong)
      buf ++= bs: Unit
    }

    def double(d: Double): Unit = {
      buf += 0xFB.toByte
      val bits = java.lang.Double.doubleToLongBits(d)
      var i = 56
      while (i >= 0) { buf += (bits >> i).toByte; i -= 8 }
    }

    def bool(b: Boolean): Unit = buf += (if (b) 0xF5 else 0xF4).toByte: Unit
    def nul(): Unit = buf += 0xF6.toByte: Unit
    def arrayHeader(n: Long): Unit = header(4, n)
    def mapHeader(n: Long): Unit = header(5, n)

    def toArray: Array[Byte] = buf.toArray
  }

  /** the write side, as a fold: `Schema.fold` with `Put`, the value walk
   * on `Schema.Step`; memoised per schema by identity */
  private def put[A](out: Out, s: Schema[A], a: A): Unit = Schema.Step.walk(putter(s), out, a)

  private type Put[A] = Schema.Step[Out, A, Unit]
  private val putter = new Schema.Folded[Put](new Schema.Algebra[Put] {
    import Schema.Step
    def int = Step.leaf((o: Out, a: Int) => o.integer(a.toLong))
    def long = Step.leaf((o: Out, a: Long) => o.integer(a))
    def double = Step.leaf((o: Out, a: Double) => o.double(a))
    def bool = Step.leaf((o: Out, a: Boolean) => o.bool(a))
    def string = Step.leaf((o: Out, a: String) => o.text(a))
    def char = Step.leaf((o: Out, a: Char) => o.text(a.toString))
    // major type 2: a byte string, which is what CBOR is for
    def bytes = Step.leaf((o: Out, a: Array[Byte]) => o.byteString(a))
    def bigInt = Step.leaf((o: Out, a: BigInt) => o.bigInt(a))
    def option[A](o: Schema.SOption[A], of: () => Put[A]) = Step.option((out: Out) => out.nul(), of)
    private def seq[C, A](items: C => Iterable[A], size: C => Int, of: () => Put[A]): Put[C] =
      Step.elems[Out, C, A, Unit, Unit](
        (o, a) => o.arrayHeader(size(a).toLong), items, of, (_, _) => (), (_, _) => (), (_, _, _) => ())
    def list[A](l: Schema.SList[A], of: () => Put[A]) = seq[List[A], A](identity, _.length, of)
    def vector[A](v: Schema.SVector[A], of: () => Put[A]) = seq[Vector[A], A](identity, _.length, of)
    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Put, Any])]) =
      Step.fields[Out, A, Unit, Unit, Put](
        (o, _) => o.mapHeader(p.fields.length.toLong), p.parts, fields,
        (o, _, n) => o.text(n), (_, _) => (), (_, _, _) => ())
    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Put, A])]) =
      Step.one[Out, A, Unit, Unit, Put](
        (o, _) => o.mapHeader(1), su.caseOf, cases, (o, n) => o.text(n), (_, _, _, _) => ())
    /** the newtype node: not a level, as on the read side */
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Put[B]) = Step.via(iso.from, under)
    def ref[A](name: String) =
      throw new IllegalStateException(s"a lazy carrier never meets a back edge, got one at $name")
  })

  /** value to bytes in one move */
  def write[A](a: A)(implicit s: Schema[A]): Array[Byte] = {
    val out = new Out
    put(out, s, a)
    out.toArray
  }

  // ---------------------------------------------------------------- decode

  /** the CBOR item primitives on the read side, once */
  final class In(bs: Array[Byte]) {
    private var i = 0

    /** how many items are open around the one being read — the
     * native/trampoline decision's input, not a depth refusal */
    private var open = 0
    def enter(): Unit = open += 1
    def leave(): Unit = open -= 1

    /** how deep the reader is right now, counting containers */
    def depth: Int = open

    def peek: Int = if (i < bs.length) bs(i) & 0xFF else -1
    def byte(): Either[String, Int] =
      if (i < bs.length) { val b = bs(i) & 0xFF; i += 1; Right(b) }
      else Left("truncated CBOR")
    def take(n: Int): Either[String, Array[Byte]] =
      if (n >= 0 && i + n <= bs.length) { val a = bs.slice(i, i + n); i += n; Right(a) }
      else Left("truncated CBOR")
    private def long(n: Int): Either[String, Long] =
      take(n).map(_.foldLeft(0L)((acc, b) => (acc << 8) | (b & 0xFF).toLong))

    /** a declared length or count, refused unless the bytes left could
     * hold it (cbor-length-wraps): every item takes at least one byte,
     * so `perItem` bytes per element bound a count without trusting it */
    private def declared(n: Long, perItem: Int, what: String): Either[String, Long] = {
      val left = bs.length - i
      if (n >= 0 && n <= (left / perItem).toLong) Right(n)
      else Left(s"$what declares ${java.lang.Long.toUnsignedString(n)}, but only $left bytes are left")
    }

    /** a string's bytes, by its declared length */
    private def body(n: Long, what: String): Either[String, Array[Byte]] =
      declared(n, 1, what).flatMap(k => take(k.toInt))

    /** the major type and the argument (a length, a count, or the
     * integer itself) — every item starts here */
    def head(): Either[String, (Int, Long)] =
      byte().flatMap { b =>
        val major = b >> 5
        (b & 0x1F) match {
          case n if n < 24 => Right((major, n.toLong))
          case 24 => long(1).map((major, _))
          case 25 => long(2).map((major, _))
          case 26 => long(4).map((major, _))
          case 27 => long(8).map((major, _))
          case x => Left(s"unsupported additional info $x")
        }
      }

    /** past 2^63 the raw argument reads negative and a Long cannot hold
     * it — refused, never read as -1 */
    def intItem(): Either[String, Long] =
      head().flatMap {
        case (0, n) if n >= 0 => Right(n)
        case (1, n) if n >= 0 => Right(-1 - n)
        case (0 | 1, _) => Left("integer out of range for a Long")
        case (m, _) => Left(s"expected an integer, got major $m")
      }

    /** every form `Out.bigInt` writes, and the non-preferred ones a
     * conforming encoder may still send (a tagged bignum for a small
     * value): major 0/1 read unsigned, tag 2/3 over a byte string */
    def bigIntItem(): Either[String, BigInt] = {
      def unsigned(n: Long) = if (n >= 0) BigInt(n) else BigInt(n) + (BigInt(1) << 64)
      head().flatMap {
        case (0, n) => Right(unsigned(n))
        case (1, n) => Right(-1 - unsigned(n))
        case (6, 2) => byteStringItem().map(bs => BigInt(1, bs))
        case (6, 3) => byteStringItem().map(bs => -1 - BigInt(1, bs))
        case (6, t) => Left(s"expected a bignum (tag 2 or 3), got tag $t")
        case (m, _) => Left(s"expected an integer, got major $m")
      }
    }

    def textItem(): Either[String, String] =
      head().flatMap {
        case (3, n) => body(n, "a text string").map(new String(_, UTF_8))
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
        case (2, n) => body(n, "a byte string")
        case (m, _) => Left(s"expected a byte string, got major $m")
      }

    def isNull: Boolean = peek == 0xF6
    def skipNull(): Unit = byte(): Unit

    def arrayHeader(): Either[String, Long] =
      head().flatMap {
        case (4, n) => declared(n, 1, "an array")
        case (m, _) => Left(s"expected an array, got major $m")
      }

    def mapHeader(): Either[String, Long] =
      head().flatMap {
        case (5, n) => declared(n, 2, "a map")
        case (m, _) => Left(s"expected a map, got major $m")
      }

    /** read one complete item and discard it — what a decoder does with
     * a field the schema does not declare (cbor-unknown-fields); native
     * below the threshold, the trampoline past it */
    def skipItem(): Either[String, Unit] =
      if (depth >= Codecs.NativeThreshold) Cont.reset(skipItemInsideC[Either[String, Unit]])
      else skipItemNative()

    private def skipItemNative(): Either[String, Unit] = {
      enter()
      val out = skipHereNative()
      leave()
      out
    }

    private def skipHereNative(): Either[String, Unit] =
      head().flatMap { case (major, n) =>
        major match {
          // 0/1: the argument WAS the integer; 7: head() consumed the
          // simple value or the float's bits with it
          case 0 | 1 | 7 => Right(())
          case 2 | 3 => body(n, "a string").map(_ => ())
          case 4 => declared(n, 1, "an array").flatMap(manyNative)
          case 5 => declared(n, 2, "a map").flatMap(k => manyNative(k * 2)) // a map is its pairs, flattened
          case 6 => skipItem() // the dispatcher: depth may have crossed the threshold
          case m => Left(s"unsupported major type $m")
        }
      }

    private def manyNative(count: Long): Either[String, Unit] = {
      var left = count
      var bad: Option[String] = None
      while (bad.isEmpty && left > 0) {
        skipItem() match { // the dispatcher, same reason
          case Left(e) => bad = Some(e)
          case Right(()) => left -= 1
        }
      }
      bad.toLeft(())
    }

    private def skipItemInsideC[R]: Either[String, Unit] /> R = {
      enter()
      skipHereC[R].flatMap { v => leave(); Cont.Pure[Either[String, Unit], R](v) }
    }

    private def skipHereC[R]: Either[String, Unit] /> R = head() match {
      case Left(e) => Cont.Pure(Left(e))
      case Right((major, n)) => major match {
        case 0 | 1 | 7 => Cont.Pure(Right(()))
        case 2 | 3 => Cont.Pure(body(n, "a string").map(_ => ()))
        case 4 => declared(n, 1, "an array").fold(e => Cont.Pure(Left(e)), manyC[R](_))
        case 5 => declared(n, 2, "a map").fold(e => Cont.Pure(Left(e)), k => manyC[R](k * 2))
        case 6 => Cont.delay(() => skipItemInsideC[R])
        case m => Cont.Pure(Left(s"unsupported major type $m"))
      }
    }

    private def manyC[R](count: Long): Either[String, Unit] /> R = {
      def loop(left: Long): Either[String, Unit] /> R =
        if (left <= 0) Cont.Pure(Right(()))
        else Cont.defer(() => skipItemInsideC[R]) {
          case Left(e) => Cont.Pure[Either[String, Unit], R](Left(e))
          case Right(()) => loop(left - 1)
        }
      loop(count)
    }
  }

  /** the public entry: below the threshold the recursive fold, at or
   * past it `getC`'s trampoline — checked on every call, so the switch
   * happens at whatever level actually crosses it */
  private def get[A](in: In, s: Schema[A]): Either[String, A] =
    if (in.depth >= Codecs.NativeThreshold) Cont.reset(getC[A, Either[String, A]](in, s))
    else getNative(in, s)

  private type Dec[X] = Either[String, X]

  private def char(x: String): Either[String, Char] =
    if (x.length == 1) Right(x.head) else Left(s"expected one character, got ${x.length}")

  /** the fields read, assembled in declaration order: a field absent
   * from the map takes its default, then None-if-optional, then the
   * refusal by name — `Json.absent`, the one statement of that rule */
  private def assemble[A](p: Schema.SProduct[A], m: Map[String, Any]): Either[String, A] =
    p.fields.indices.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, i) =>
      acc.flatMap { xs =>
        m.get(p.fields(i)._1) match {
          case Some(v) => Right(xs :+ v)
          case None => Json.absent(p, i).map(xs :+ _)
        }
      }
    }.map(p.make)

  private def getNative[A](in: In, s: Schema[A]): Either[String, A] = s.visit(new Schema.Visit[Dec] {
    def int = in.intItem().flatMap(Numbers.int)
    def long = in.intItem()
    def double = in.doubleItem()
    def bool = in.boolItem()
    def string = in.textItem()
    def char = in.textItem().flatMap(Cbor.char)
    def bytes = in.byteStringItem()
    def bigInt = in.bigIntItem()
    def option[B](o: Schema.SOption[B]) =
      if (in.isNull) { in.skipNull(); Right(None) }
      else get(in, o.of()).map(Some(_))
    def list[B](l: Schema.SList[B]) =
      // prepended and reversed once: `xs :+ _` on a List copies per element
      inside(in) {
        in.arrayHeader().flatMap { n =>
          (0L until n).foldLeft(Right(Nil): Either[String, List[B]]) { (acc, _) =>
            acc.flatMap(xs => get(in, l.of()).map(_ :: xs))
          }.map(_.reverse)
        }
      }
    def vector[B](v: Schema.SVector[B]) =
      inside(in) {
        in.arrayHeader().flatMap { n =>
          (0L until n).foldLeft(Right(Vector.empty): Either[String, Vector[B]]) { (acc, _) =>
            acc.flatMap(xs => get(in, v.of()).map(xs :+ _))
          }
        }
      }
    def product[B](p: Schema.SProduct[B]) =
      inside(in) {
        in.mapHeader().flatMap { n =>
          (0L until n).foldLeft(Right(Map.empty[String, Any]): Either[String, Map[String, Any]]) { (acc, _) =>
            acc.flatMap { m =>
              in.textItem().flatMap { k =>
                p.fields.find(_._1 == k) match {
                  case Some((_, sc)) => field(in, sc()).map(v => m + (k -> v))
                  // a field this schema does not declare is SKIPPED, as
                  // Json.decode skips it (cbor-unknown-fields)
                  case None => in.skipItem().map(_ => m)
                }
              }
            }
          }.flatMap(assemble(p, _))
        }
      }
    def sum[B](su: Schema.SSum[B]) =
      inside(in) {
        in.mapHeader().flatMap {
          case 1 => in.textItem().flatMap { name =>
            su.cases.find(_._1 == name)
              .toRight(s"unknown case '$name' of ${su.name}")
              .flatMap { case (_, sc) => caseOf(in, sc()) }
          }
          case n => Left(s"expected a one-entry map, got $n entries")
        }
      }
    def iso[B, C](i: Schema.SIso[B, C]) = get(in, i.under()).flatMap(i.to)
  })

  /** one container's worth of nesting, on the reader's one counter */
  private def inside[X](in: In)(body: => Either[String, X]): Either[String, X] = {
    in.enter()
    val out = body
    in.leave()
    out
  }

  /** one field at its own type; the value joins the product's erased parts */
  private def field[X](in: In, sc: Schema[X]): Either[String, Any] = get(in, sc)

  /** one case at its own type, widened to the sum's */
  private def caseOf[B, X <: B](in: In, sc: Schema[X]): Either[String, B] = get(in, sc)

  private type DecC[R] = { type L[X] = Either[String, X] /> R }

  /** one container's worth of nesting, Cont-shaped: `leave()` fires
   * once the inner computation's value is ready */
  private def insideC[X, R](in: In)(body: => (Either[String, X] /> R)): Either[String, X] /> R = {
    in.enter()
    body.flatMap { v => in.leave(); Cont.Pure[Either[String, X], R](v) }
  }

  /** the trampoline: getNative case for case, each descent into a
   * nested schema a `Cont.defer` in `reset`'s loop instead of a frame */
  private def getC[A, R](in: In, s: Schema[A]): Either[String, A] /> R = s.visit(new Schema.Visit[DecC[R]#L] {
    private def now[X](e: Either[String, X]): Either[String, X] /> R = Cont.Pure(e)
    def int = now(in.intItem().flatMap(Numbers.int))
    def long = now(in.intItem())
    def double = now(in.doubleItem())
    def bool = now(in.boolItem())
    def string = now(in.textItem())
    def char = now(in.textItem().flatMap(Cbor.char))
    def bytes = now(in.byteStringItem())
    def bigInt = now(in.bigIntItem())
    def option[B](o: Schema.SOption[B]) =
      if (in.isNull) { in.skipNull(); now(Right(None)) }
      else Cont.defer(() => getC[B, R](in, o.of()))((r: Either[String, B]) => now(r.map(Some(_))))
    private def elems[B](of: Schema[B]): Either[String, List[B]] /> R =
      in.arrayHeader() match {
        case Left(e) => now(Left(e))
        case Right(n) =>
          def loop(i: Long, acc: List[B]): Either[String, List[B]] /> R =
            if (i >= n) now(Right(acc.reverse))
            else Cont.defer(() => getC[B, R](in, of)) {
              case Left(e) => now[List[B]](Left(e))
              case Right(x) => loop(i + 1, x :: acc)
            }
          loop(0, Nil)
      }
    def list[B](l: Schema.SList[B]) = insideC(in)(elems(l.of()))
    def vector[B](v: Schema.SVector[B]) = insideC(in)(elems(v.of()).map(_.map(_.toVector)))
    def product[B](p: Schema.SProduct[B]) = insideC(in) {
      in.mapHeader() match {
        case Left(e) => now[B](Left(e))
        case Right(n) =>
          def fieldC[X](sc: Schema[X]): Either[String, Any] /> R =
            getC[X, R](in, sc).map(e => e: Either[String, Any])
          def again(i: Long, m: Map[String, Any]): Either[String, Map[String, Any]] /> R = readFields(i, m)
          @scala.annotation.tailrec
          def readFields(i: Long, m: Map[String, Any]): Either[String, Map[String, Any]] /> R =
            if (i >= n) now(Right(m))
            else in.textItem() match {
              case Left(e) => now(Left(e))
              case Right(k) => p.fields.find(_._1 == k) match {
                case Some((_, sc)) =>
                  Cont.defer(() => fieldC(sc())) {
                    case Left(e) => now[Map[String, Any]](Left(e))
                    case Right(v) => again(i + 1, m + (k -> v))
                  }
                // unknown field: skipped, as getNative does — skipItem
                // trampolines on its own, so no defer here
                case None => in.skipItem() match {
                  case Left(e) => now(Left(e))
                  case Right(()) => readFields(i + 1, m)
                }
              }
            }
          readFields(0, Map.empty).map(_.flatMap(assemble(p, _)))
      }
    }
    def sum[B](su: Schema.SSum[B]) = insideC(in) {
      in.mapHeader() match {
        case Left(e) => now[B](Left(e))
        case Right(1) => in.textItem() match {
          case Left(e) => now(Left(e))
          case Right(name) => su.cases.find(_._1 == name) match {
            case None => now(Left(s"unknown case '$name' of ${su.name}"))
            case Some((_, sc)) => caseC(sc())
          }
        }
        case Right(n) => now(Left(s"expected a one-entry map, got $n entries"))
      }
    }
    /** deferred like every other descent: a hand-written case that
     * recurses directly, with no product between, is still trampolined */
    private def caseC[B, X <: B](sc: Schema[X]): Either[String, B] /> R =
      Cont.defer(() => getC[X, R](in, sc))((r: Either[String, X]) => now[B](r))
    def iso[B, C](i: Schema.SIso[B, C]) =
      Cont.defer(() => getC[C, R](in, i.under()))((r: Either[String, C]) => now(r.flatMap(i.to)))
  })

  /** bytes to value in one move; errors as values */
  def read[A](bytes: Array[Byte])(implicit s: Schema[A]): Either[String, A] =
    get(new In(bytes), s)

  /** one item at A's schema, on an already-open cursor or accumulator */
  def encodeItem[A](out: Out, a: A)(implicit s: Schema[A]): Unit = put(out, s, a)
  def decodeItem[A](in: In)(implicit s: Schema[A]): Either[String, A] = get(in, s)
}
