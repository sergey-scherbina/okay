package okay.codec

import scala.collection.mutable.ArrayBuffer
import okay.{Cont, reset, />}

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

  /** how deep a message may nest before this decoder refuses — the
   * SENDER chooses that depth, which is why a limit exists at all;
   * `Codecs.maxDepth` is the number and says why, and it is the same
   * number on the JSON wire. It used to be `Cbor.maxSkipDepth` and to
   * bound only the skip, which was the half of the problem that had
   * been noticed (input-depth-both-wires). */
  def maxDepth: Int = Codecs.maxDepth


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

    /**
     * How many items are open around the one being read — ONE budget
     * for the whole frame, spent by declared reads and by skips alike.
     *
     * Declared depth needed it as much as a skip did: a RECURSIVE
     * schema's value is nested as deeply as the SENDER nested it, and
     * MEASURED 2026-09-10 under sbt's `-Xss8m`, a 5 000-level tree was
     * a `StackOverflowError` out of `Cbor.read`, which promises an
     * Either. One counter rather than a parameter because the
     * generated decoder's containers are `Staged.cborProduct` and
     * friends, not this file's `get`: they spend the same budget, so
     * the three decoders refuse at the same depth. An abort needs no
     * `leave` — a Left ends the read.
     */
    private var open = 0
    def enter(): Boolean = if open >= Codecs.maxDepth then false else { open += 1; true }
    def leave(): Unit = open -= 1
    def tooDeep[X]: Either[String, X] = Left(s"nested deeper than ${Codecs.maxDepth}")

    /** how deep the reader is right now, counting containers — the
      * SAME number `enter`/`leave` maintain, read here for
      * `Cbor.get`'s native/trampoline decision (cbor-decode-threshold-
      * trampoline); a second counter would be a second source of
      * truth for exactly the fact this one already tracks */
    def depth: Int = open

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

    /**
     * Read one complete item and discard it — what a decoder does
     * with a field the schema does not declare (cbor-unknown-fields).
     *
     * It recurses on the depth of the INPUT, which the sender chose,
     * so it spends the reader's budget (`enter`) — without one, a
     * hundred thousand nested arrays in a field nobody declared would
     * be a stack overflow rather than a decode error, a fault where
     * this module promises a value. The budget is `open`'s, not this
     * function's own: an unknown field 200 items deep inside 200
     * declared ones is 400 levels of the same stack
     * (input-depth-both-wires).
     *
     * PAST `Codecs.NativeThreshold` this dispatches to the SAME
     * `Cont.defer` trampoline `Cbor.get` uses, for the same reason:
     * skipping an undeclared field is exactly as input-depth-driven as
     * decoding a declared one, and was the one site the two closed
     * roots left as a rescue rather than a policy
     * (depth-is-policy-not-rescue, cbor-skip-threshold-trampoline).
     * Uniformly typed (`Either[String, Unit]`), so unlike `getC` there
     * is no cross-type `R` to thread through the schema — every
     * caller's own `R` works, since `skipItem` never exposes `Cont` in
     * its signature.
     */
    def skipItem(): Either[String, Unit] =
      if depth >= Codecs.NativeThreshold then reset(skipItemInsideC[Either[String, Unit]])
      else skipItemNative()

    private def skipItemNative(): Either[String, Unit] =
      if !enter() then tooDeep
      else
        val out = skipHereNative()
        leave()
        out

    private def skipHereNative(): Either[String, Unit] =
      head().flatMap { (major, n) =>
        major match
          // 0/1: the argument WAS the integer; 7: head() consumed the
          // simple value or the float's bits with it
          case 0 | 1 | 7 => Right(())
          case 2 | 3 => take(n.toInt).map(_ => ())
          case 4 => manyNative(n)
          case 5 => manyNative(n * 2)      // a map is its pairs, flattened
          case 6 => skipItem()             // the DISPATCHER: depth may
                                            // have crossed the threshold
                                            // by the time a tag's own
                                            // item is reached
          case m => Left(s"unsupported major type $m")
      }

    private def manyNative(count: Long): Either[String, Unit] =
      var left = count
      var bad: Option[String] = None
      while bad.isEmpty && left > 0 do
        skipItem() match               // the DISPATCHER, same reason
          case Left(e) => bad = Some(e)
          case Right(()) => left -= 1
      bad.toLeft(())

    /** the trampoline: `skipHereC`'s major-6 (a tag) recursion is the
      * ONE point that descends into a fresh item, deferred through
      * `Cont`; the sibling loop for major 4/5 (`manyC`) is the same
      * shape as `Cbor.get`'s own list loops */
    private def skipItemInsideC[R]: Either[String, Unit] /> R =
      if !enter() then Cont.Pure(tooDeep)
      else skipHereC[R].flatMap { v => leave(); Cont.Pure(v) }

    private def skipHereC[R]: Either[String, Unit] /> R =
      Cont.Pure(head()).flatMap {
        case Left(e) => Cont.Pure(Left(e))
        case Right((major, n)) => major match
          case 0 | 1 | 7 => Cont.Pure(Right(()))
          case 2 | 3 => Cont.Pure(take(n.toInt).map(_ => ()))
          case 4 => manyC[R](n)
          case 5 => manyC[R](n * 2)
          case 6 => Cont.defer(() => skipItemInsideC[R])(r => Cont.Pure(r))
          case m => Cont.Pure(Left(s"unsupported major type $m"))
      }

    private def manyC[R](count: Long): Either[String, Unit] /> R =
      def loop(left: Long): Either[String, Unit] /> R =
        if left <= 0 then Cont.Pure(Right(()))
        else Cont.defer(() => skipItemInsideC[R]) {
          case Left(e) => Cont.Pure(Left(e))
          case Right(()) => loop(left - 1)
        }
      loop(count)

  /** one container's worth of nesting, on the reader's one budget —
   * the declared reads spend it exactly as a skip does */
  private def inside[X](in: In)(body: => Either[String, X]): Either[String, X] =
    if !in.enter() then in.tooDeep
    else
      val out = body
      in.leave()
      out

  /** the public entry: below the threshold, today's recursive fold,
   * unchanged; at or past it, `getC`'s trampoline — checked on EVERY
   * call, so the switch happens at whatever level actually crosses
   * it, not only at the top */
  private def get[A](in: In, s: Schema[A]): Either[String, A] =
    if in.depth >= Codecs.NativeThreshold then reset(getC[A, Either[String, A]](in, s))
    else getNative(in, s)

  private def getNative[A](in: In, s: Schema[A]): Either[String, A] = s match
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
      inside(in) { in.arrayHeader().flatMap { n =>
        (0L until n).foldLeft(Right(Nil): Either[String, List[a]]) { (acc, _) =>
          acc.flatMap(xs => get(in, l.of()).map(xs :+ _))
        }
      } }
    case vec: Schema.SVector[a] =>
      inside(in) { in.arrayHeader().flatMap { n =>
        (0L until n).foldLeft(Right(Vector.empty): Either[String, Vector[a]]) { (acc, _) =>
          acc.flatMap(xs => get(in, vec.of()).map(xs :+ _))
        }
      } }
    case p: Schema.SProduct[A] =>
      inside(in) { in.mapHeader().flatMap { n =>
        (0L until n).foldLeft(Right(Map.empty[String, Any]): Either[String, Map[String, Any]]) {
          (acc, _) =>
            acc.flatMap { m =>
              in.textItem().flatMap { k =>
                p.fields.find(_._1 == k) match
                  case Some((_, sc)) => field(in, sc()).map(v => m + (k -> v))
                  // a field this schema does not declare is SKIPPED,
                  // as Json.decode has always skipped it: one Schema,
                  // one value, one answer on either wire, and adding a
                  // field stops being a breaking change for every
                  // deployed reader (cbor-unknown-fields)
                  case None => in.skipItem().map(_ => m)
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
      } }
    case su: Schema.SSum[A] =>
      // spelled out rather than through `inside`: the case schemas are
      // `Schema[? <: A]`, so the block's type carries a wildcard the
      // by-name parameter cannot take ("not a value")
      if !in.enter() then in.tooDeep
      else
        val out: Either[String, A] = in.mapHeader().flatMap {
          case 1 => in.textItem().flatMap { name =>
            su.cases.find(_._1 == name)
              .toRight(s"unknown case '$name' of ${su.name}")
              .flatMap((_, sc) => get(in, sc()))
          }
          case n => Left(s"expected a one-entry map, got $n entries")
        }
        in.leave()
        out

  /** one field at its own type; the value joins the product's erased
   * parts (Mirror's fromProduct takes Any) */
  private def field[X](in: In, sc: Schema[X]): Either[String, Any] = get(in, sc)

  // ---------------------------------------------------------------
  // the trampoline (cbor-decode-threshold-trampoline): PAST
  // NativeThreshold, `get` runs this instead of `getNative`. Same
  // fold, same rules (unknown fields skipped, defaults, the
  // enter/leave budget for `Codecs.maxDepth`) — the ONLY difference
  // is that a descent into a NESTED schema is `Cont.defer`red rather
  // than called directly, so the trampoline forces it inside `/`'s
  // own loop, one level per iteration, at constant native stack
  // (`Cont.defer` is the exact mechanism `specs/eff-stack-safety.md`
  // proved and measured for `Eff`'s left-nested binds).
  //
  // `R` is the FINAL answer type of the whole trampolined
  // computation — fixed ONCE, at the `reset` call in `get`, to
  // `Either[String, A]` for whatever `A` was being decoded when depth
  // first crossed the threshold — and threaded unchanged through
  // every nested `getC`/`insideC`/`fieldC` call below, however many
  // different field/element types it passes through on the way. Only
  // the VALUE type (`Either[String, X]`) varies per node; that is
  // what makes `Cont.defer`'s own type (`() => Cont[A,T,R]` sharing
  // `R` with the surrounding `Cont[B,S,R]`) able to compose them.
  //
  // No cast anywhere below (no-casts-without-necessity): `Cont` is
  // INVARIANT in its value type, unlike `Either` — where `getNative`
  // widens `Either[String, X]` to `Either[String, Any]` for free
  // (covariance) at a product's field or `Either[String, C]` to
  // `Either[String, A]` for a sum's case (C <: A), the Cont-wrapped
  // equivalents need one explicit `.map` to re-state the SAME
  // widening at the Cont level — still a checked upcast, not a
  // runtime test.
  // ---------------------------------------------------------------

  /** one container's worth of nesting, Cont-shaped: `leave()` is
   * sequenced to fire once the inner computation's VALUE is ready
   * (via flatMap), not when this function returns — it returns a
   * Cont, not a value */
  private def insideC[X, R](in: In)(body: => (Either[String, X] /> R)): Either[String, X] /> R =
    if !in.enter() then Cont.Pure(in.tooDeep)
    else body.flatMap { v => in.leave(); Cont.Pure(v) }

  /** `field`'s Cont-shaped twin: one field at its own type, widened
   * to `Any` to join the product's erased parts — the SAME widening
   * `field` does via Either's covariance, restated because Cont does
   * not share it */
  private def fieldC[X, R](in: In, sc: Schema[X]): Either[String, Any] /> R =
    getC(in, sc).map(e => e: Either[String, Any])

  private def getC[X, R](in: In, s: Schema[X]): Either[String, X] /> R = s match
    case Schema.SIso(u, to, _) =>
      Cont.defer(() => getC(in, u()))(r => Cont.Pure(r.flatMap(to)))
    case Schema.SInt => Cont.Pure(in.intItem().map(_.toInt))
    case Schema.SLong => Cont.Pure(in.intItem())
    case Schema.SDouble => Cont.Pure(in.doubleItem())
    case Schema.SBool => Cont.Pure(in.boolItem())
    case Schema.SString => Cont.Pure(in.textItem())
    case Schema.SChar => Cont.Pure(in.textItem().flatMap(x =>
      if x.length == 1 then Right(x.head) else Left(s"expected one character, got ${x.length}")))
    case Schema.SBytes => Cont.Pure(in.byteStringItem())
    case Schema.SOption(of) =>
      if in.isNull then { in.skipNull(); Cont.Pure(Right(None)) }
      else Cont.defer(() => getC(in, of()))(r => Cont.Pure(r.map(Some(_))))
    case l: Schema.SList[a] =>
      insideC(in) {
        Cont.Pure(in.arrayHeader()).flatMap {
          case Left(e) => Cont.Pure(Left(e))
          case Right(n) =>
            def loop(i: Long, acc: List[a]): Either[String, List[a]] /> R =
              if i >= n then Cont.Pure(Right(acc.reverse))
              else Cont.defer(() => getC(in, l.of())) {
                case Left(e) => Cont.Pure(Left(e))
                case Right(x) => loop(i + 1, x :: acc)
              }
            loop(0, Nil)
        }
      }
    case vec: Schema.SVector[a] =>
      insideC(in) {
        Cont.Pure(in.arrayHeader()).flatMap {
          case Left(e) => Cont.Pure(Left(e))
          case Right(n) =>
            def loop(i: Long, acc: Vector[a]): Either[String, Vector[a]] /> R =
              if i >= n then Cont.Pure(Right(acc))
              else Cont.defer(() => getC(in, vec.of())) {
                case Left(e) => Cont.Pure(Left(e))
                case Right(x) => loop(i + 1, acc :+ x)
              }
            loop(0, Vector.empty)
        }
      }
    case p: Schema.SProduct[X] =>
      insideC(in) {
        Cont.Pure(in.mapHeader()).flatMap {
          case Left(e) => Cont.Pure(Left(e))
          case Right(n) =>
            def readFields(i: Long, m: Map[String, Any]): Either[String, Map[String, Any]] /> R =
              if i >= n then Cont.Pure(Right(m))
              else Cont.defer(() => Cont.Pure(in.textItem())) {
                case Left(e) => Cont.Pure(Left(e))
                case Right(k) => p.fields.find(_._1 == k) match
                  case Some((_, sc)) =>
                    Cont.defer(() => fieldC(in, sc())) {
                      case Left(e) => Cont.Pure(Left(e))
                      case Right(v) => readFields(i + 1, m + (k -> v))
                    }
                  // unknown field: skipped, same rule as getNative
                  // (cbor-unknown-fields) — no recursion into a
                  // fresh schema, so no defer needed here
                  case None => in.skipItem() match
                    case Left(e) => Cont.Pure(Left(e))
                    case Right(()) => readFields(i + 1, m)
              }
            readFields(0, Map.empty).flatMap { fieldsResult =>
              Cont.Pure(fieldsResult.flatMap { m =>
                // the same assembly as getNative: declared default,
                // then None-if-optional, then the refusal — no
                // decoding happens here, only combining what already
                // decoded, so this stays a plain (non-deferred) fold
                p.fields.zipWithIndex.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, fi) =>
                  val (f, i) = fi
                  acc.flatMap { xs =>
                    (m.get(f._1), f._2()) match
                      case (None, sc) => (p.defaults.lift(i).flatten, sc) match
                        case (Some(d), _) => Right(xs :+ d())
                        case (None, _: Schema.SOption[?]) => Right(xs :+ None)
                        case _ => Left(s"missing field '${f._1}' in ${p.name}")
                      case (found, _) =>
                        found.toRight(s"missing field '${f._1}' in ${p.name}").map(xs :+ _)
                  }
                }.map(p.make)
              })
            }
        }
      }
    case su: Schema.SSum[X] =>
      insideC(in) {
        Cont.Pure(in.mapHeader()).flatMap {
          case Left(e) => Cont.Pure(Left(e))
          case Right(1) =>
            Cont.defer(() => Cont.Pure(in.textItem())) {
              case Left(e) => Cont.Pure(Left(e))
              case Right(name) => su.cases.find(_._1 == name) match
                case None => Cont.Pure(Left(s"unknown case '$name' of ${su.name}"))
                // sc(): Schema[C] for some C <: X (su.cases' own bound) —
                // getC(in, sc()) is Either[String, C] /> R, widened to
                // Either[String, X] /> R the same way getNative's plain
                // `get(in, sc())` widens via Either's own covariance
                case Some((_, sc)) => getC(in, sc()).map(e => e: Either[String, X])
            }
          case Right(n) => Cont.Pure(Left(s"expected a one-entry map, got $n entries"))
        }
      }


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
