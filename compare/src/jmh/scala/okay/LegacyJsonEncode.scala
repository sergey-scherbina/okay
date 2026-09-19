package okay

import okay.{Cont, reset, />}
import okay.codec.{Base64, Codecs, Json, Schema}

/**
 * `Json.encode` as it stood before schema-fold stage 2 — the
 * hand-rolled `encodeIntoNative`/`encodeIntoC` pair, VERBATIM — kept
 * only here, as a benchmark lane, so the fold-based encoder is priced
 * against it in ONE JMH invocation on the same box state (the
 * before/after-in-separate-runs shape is what jmh-load-not-just-forks
 * refuses). Not a library door: nothing outside this file may call it.
 */
object LegacyJsonEncode:
  def encode[A](s: Schema[A])(a: A): String =
    val sb = new StringBuilder
    encodeInto(s, a, sb, 0)
    sb.toString

  private def encodeInto[A](s: Schema[A], a: A, sb: StringBuilder, open: Int): Unit =
    if open >= Codecs.NativeThreshold then reset(encodeIntoC[A, Unit](s, a, sb, open))
    else encodeIntoNative(s, a, sb, open)

  private def encodeIntoNative[A](s: Schema[A], a: A, sb: StringBuilder, open: Int): Unit = s match
    case Schema.SInt => sb.append(a.toString): Unit
    case Schema.SLong => sb.append(a.toString): Unit
    case Schema.SDouble => sb.append(a.toString): Unit
    case Schema.SBool => sb.append(a.toString): Unit
    case Schema.SString => sb.append('"').append(Json.escape(a)).append('"'): Unit
    case Schema.SChar => sb.append('"').append(Json.escape(a.toString)).append('"'): Unit
    case Schema.SBytes => sb.append('"').append(Base64.encode(a)).append('"'): Unit
    case Schema.SOption(of) =>
      a match
        case Some(x) => encodeInto(of(), x, sb, open + 1)
        case None => sb.append("null"): Unit
    case Schema.SList(of) =>
      sb.append('[')
      var first = true
      a.foreach { x => if !first then sb.append(','); first = false; encodeInto(of(), x, sb, open + 1) }
      sb.append(']'): Unit
    case Schema.SVector(of) =>
      sb.append('[')
      var first = true
      a.foreach { x => if !first then sb.append(','); first = false; encodeInto(of(), x, sb, open + 1) }
      sb.append(']'): Unit
    case p: Schema.SProduct[A] =>
      sb.append('{')
      var first = true
      p.eachField(a)([X] => (n: String, sc: Schema[X], x: X) => {
        if !first then sb.append(',')
        first = false
        val _ = sb.append('"').append(n).append("\":")
        encodeInto(sc, x, sb, open + 1)
      }): Unit
      sb.append('}'): Unit
    case su: Schema.SSum[A] =>
      sb.append('{')
      su.theCase(a)([X <: A] => (n: String, sc: Schema[X], x: X) => {
        val _ = sb.append('"').append(n).append("\":")
        encodeInto(sc, x, sb, open + 1)
      })
      sb.append('}'): Unit
    case Schema.SIso(u, _, from) => encodeInto(u(), from(a), sb, open)

  private def encodeIntoC[A, R](s: Schema[A], a: A, sb: StringBuilder, open: Int): Unit /> R = s match
    case Schema.SInt => sb.append(a.toString); Cont.Pure(())
    case Schema.SLong => sb.append(a.toString); Cont.Pure(())
    case Schema.SDouble => sb.append(a.toString); Cont.Pure(())
    case Schema.SBool => sb.append(a.toString); Cont.Pure(())
    case Schema.SString => val _ = sb.append('"').append(Json.escape(a)).append('"'); Cont.Pure(())
    case Schema.SChar => val _ = sb.append('"').append(Json.escape(a.toString)).append('"'); Cont.Pure(())
    case Schema.SBytes => val _ = sb.append('"').append(Base64.encode(a)).append('"'); Cont.Pure(())
    case Schema.SOption(of) => a match
      case Some(x) => Cont.defer(() => encodeIntoC(of(), x, sb, open + 1))(_ => Cont.Pure(()))
      case None => sb.append("null"); Cont.Pure(())
    case Schema.SList(of) =>
      sb.append('[')
      def loop(rest: A, first: Boolean): Unit /> R =
        if rest.isEmpty then { sb.append(']'); Cont.Pure(()) }
        else
          if !first then sb.append(',')
          Cont.defer(() => encodeIntoC(of(), rest.head, sb, open + 1))(_ => loop(rest.tail, false))
      loop(a, true)
    case Schema.SVector(of) =>
      sb.append('[')
      def loop(rest: A, first: Boolean): Unit /> R =
        if rest.isEmpty then { sb.append(']'); Cont.Pure(()) }
        else
          if !first then sb.append(',')
          Cont.defer(() => encodeIntoC(of(), rest.head, sb, open + 1))(_ => loop(rest.tail, false))
      loop(a, true)
    case p: Schema.SProduct[A] =>
      sb.append('{')
      val steps: Vector[Unit /> R] = p.eachField(a)([X] => (n: String, sc: Schema[X], x: X) =>
        Cont.defer(() => { val _ = sb.append('"').append(n).append("\":"); encodeIntoC(sc, x, sb, open + 1) })(_ => Cont.Pure(())))
      def loop(rest: Vector[Unit /> R], first: Boolean): Unit /> R =
        if rest.isEmpty then { sb.append('}'); Cont.Pure(()) }
        else
          if !first then sb.append(',')
          rest.head.flatMap(_ => loop(rest.tail, false))
      loop(steps, true)
    case su: Schema.SSum[A] =>
      sb.append('{')
      val step: Unit /> R = su.theCase(a)([X <: A] => (n: String, sc: Schema[X], x: X) =>
        Cont.defer(() => { val _ = sb.append('"').append(n).append("\":"); encodeIntoC(sc, x, sb, open + 1) })(_ => Cont.Pure(())))
      step.flatMap(_ => { sb.append('}'); Cont.Pure(()) })
    case Schema.SIso(u, _, from) => Cont.defer(() => encodeIntoC(u(), from(a), sb, open))(_ => Cont.Pure(()))

/** `Cbor.put` as it stood before schema-fold stage 2 (`putNative`/`putC`), verbatim — the same-run reference lane */
object LegacyCborPut:
  import okay.codec.Cbor.Out
  def write[A](a: A)(using s: Schema[A]): Array[Byte] =
    val out = new Out
    putAt(out, s, a, 0)
    out.toArray

  private def putAt[A](out: Out, s: Schema[A], a: A, open: Int): Unit =
    if open >= Codecs.NativeThreshold then reset(putC[A, Unit](out, s, a, open))
    else putNative(out, s, a, open)

  private def putNative[A](out: Out, s: Schema[A], a: A, open: Int): Unit = s match
    case Schema.SIso(u, _, from) => putAt(out, u(), from(a), open)
    case Schema.SInt => out.integer(a.toLong)
    case Schema.SLong => out.integer(a)
    case Schema.SDouble => out.double(a)
    case Schema.SBool => out.bool(a)
    case Schema.SString => out.text(a)
    case Schema.SChar => out.text(a.toString)
    case Schema.SBytes => out.byteString(a)
    case Schema.SOption(of) => a match
      case None => out.nul()
      case Some(x) => putAt(out, of(), x, open + 1)
    case Schema.SList(of) =>
      out.arrayHeader(a.length.toLong)
      a.foreach(putAt(out, of(), _, open + 1))
    case Schema.SVector(of) =>
      out.arrayHeader(a.length.toLong)
      a.foreach(putAt(out, of(), _, open + 1))
    case p: Schema.SProduct[A] =>
      out.mapHeader(p.fields.length.toLong)
      p.eachField(a)([X] => (n: String, sc: Schema[X], x: X) =>
        { out.text(n); putAt(out, sc, x, open + 1) }): Unit
    case su: Schema.SSum[A] =>
      out.mapHeader(1)
      su.theCase(a)([X <: A] => (n: String, sc: Schema[X], x: X) =>
        { out.text(n); putAt(out, sc, x, open + 1) })

  private def putC[A, R](out: Out, s: Schema[A], a: A, open: Int): Unit /> R = s match
    case Schema.SIso(u, _, from) =>
      Cont.defer(() => putC(out, u(), from(a), open))(_ => Cont.Pure(()))
    case Schema.SOption(of) => a match
      case None => out.nul(); Cont.Pure(())
      case Some(x) => Cont.defer(() => putC(out, of(), x, open + 1))(_ => Cont.Pure(()))
    case Schema.SList(of) =>
      out.arrayHeader(a.length.toLong)
      def loop(rest: A): Unit /> R =
        if rest.isEmpty then Cont.Pure(())
        else Cont.defer(() => putC(out, of(), rest.head, open + 1))(_ => loop(rest.tail))
      loop(a)
    case Schema.SVector(of) =>
      out.arrayHeader(a.length.toLong)
      def loop(rest: A): Unit /> R =
        if rest.isEmpty then Cont.Pure(())
        else Cont.defer(() => putC(out, of(), rest.head, open + 1))(_ => loop(rest.tail))
      loop(a)
    case p: Schema.SProduct[A] =>
      out.mapHeader(p.fields.length.toLong)
      val steps: Vector[Unit /> R] = p.eachField(a)([X] => (n: String, sc: Schema[X], x: X) =>
        Cont.defer(() => { out.text(n); putC(out, sc, x, open + 1) })(_ => Cont.Pure(())))
      def loop(rest: Vector[Unit /> R]): Unit /> R =
        if rest.isEmpty then Cont.Pure(()) else rest.head.flatMap(_ => loop(rest.tail))
      loop(steps)
    case su: Schema.SSum[A] =>
      out.mapHeader(1)
      su.theCase(a)([X <: A] => (n: String, sc: Schema[X], x: X) =>
        { out.text(n); Cont.defer(() => putC(out, sc, x, open + 1))(_ => Cont.Pure(())) })
    case Schema.SInt => out.integer(a.toLong); Cont.Pure(())
    case Schema.SLong => out.integer(a); Cont.Pure(())
    case Schema.SDouble => out.double(a); Cont.Pure(())
    case Schema.SBool => out.bool(a); Cont.Pure(())
    case Schema.SString => out.text(a); Cont.Pure(())
    case Schema.SChar => out.text(a.toString); Cont.Pure(())
    case Schema.SBytes => out.byteString(a); Cont.Pure(())
