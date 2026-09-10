package okay.codec

import okay.{Cont, reset, />}
import okay.lex.Json as JsonLex
import okay.lex.Json.K
import okay.parse.{Cst, JsonParse, Parse}

/**
 * The JSON dialect: the semantic projection of the lossless CST, and
 * the two Schema algebras — encode (a value renders to text) and
 * decode (a projected value reads back, errors as data: Either, never
 * a throw). One derivation (Schema) serves this dialect and every
 * other; the CST comes from the total lex+parse pipeline, so a
 * damaged document projects to a value with JErr leaves instead of
 * failing.
 */
enum Json:
  case JNull
  case JBool(b: Boolean)
  case JNum(n: Double)
  case JStr(s: String)
  case JArr(vs: Vector[Json])
  case JObj(fs: Vector[(String, Json)])
  case JErr(message: String)

object Json {

  /**
   * How deep a document either road reads: `Codecs.maxDepth`, which
   * says why (input-depth-both-wires). Each road refuses past it in
   * the idiom it already had for damage — the fast road is NOT SURE
   * (`JsonValue.parse` answers None), the lossless road makes the cut
   * a `JErr` in place — so `Json.parse` still answers a value and the
   * two roads still agree (TestJsonValue's law, TestInputDepth).
   *
   * A cut is not damage at a SPOT, and that is the whole rule: the
   * projection PROPAGATES it, so a container holding the cut is the
   * cut and `Json.parse` of a too-deep document answers the cut at
   * the root. Every decoder's existing JErr refusal then catches it
   * wherever it sits — including inside a field nobody declared,
   * which is how the JSON roads came to refuse what CBOR refuses
   * (cut-refuses-the-document).
   *
   * It was first written the other way, in place, and that was worse
   * than the stack overflow it replaced: `decode` skips a damaged
   * list element and reads a damaged optional as absent — right for a
   * half-arrived document — so a 256-level tree came back as a
   * 128-level one with `Right`. Three rules in three files had to
   * remember the exception; one rule here needs none of them. What is
   * given up is the partial value of a too-deep document, which no
   * caller can use: a reader cannot say what it did not descend into.
   */
  def maxDepth: Int = Codecs.maxDepth

  /** the cut the projection leaves where the document went too deep */
  private[codec] def tooDeep: Json = JErr(cutMessage)

  private[codec] val cutMessage: String = s"nested deeper than ${Codecs.maxDepth}"

  /** is this the depth cut, rather than damage the sender's document
   * actually carried? */
  def isCut(j: Json): Boolean = j match
    case JErr(m) => m == cutMessage
    case _ => false


  // ----------------------------------------------------------------
  // parse: scanner -> per-token instructions -> CST -> projection

  /**
   * The lossless layer: any string yields a CST that render puts back
   * byte-for-byte (trivia, ordering, duplicate keys, damage).
   *
   * `Parse.full` is documented as "the common case: a per-token driver
   * with no state of its own", and `JsonParse.instrs` is exactly that
   * — it says "no cross-token state" in its own comment. So a batch
   * parse needs no driver STAGE at all, and no streaming.
   *
   * It used to. Until json-cst-batch-road (2026-09-07) this fed the
   * source ONE CHARACTER AT A TIME through the effect system —
   * `Writer.tell(c).flatMap(...)` per char — into two transducer
   * stages and a LazyList. Measured on 1.68 MB: 244 ms, of which
   * merely moving the characters through `Writer` was 61-71 ms, four
   * times the entire fast value parse. JSON was the last codec on
   * that road; Xml already used `Parse.fullWith` and Yaml a hand
   * loop.
   *
   * The streaming pipeline keeps its reason to exist — incremental
   * reparse, and input that is not a String in hand — and this is the
   * same `Scan` and the same `instrs`, so there is no second grammar.
   */
  def cst(s: String): Cst[K] = Parse.full(JsonLex.scan, JsonParse.instrs)(s).tree

  /** render = the lossless law made a function */
  def render(c: Cst[K]): String = Cst.lexemes(c)

  /** print a Json VALUE back to text — the projection's other
   * direction (render is the CST's; this one is the value's) */
  def print(j: Json): String = j match
    case JNull => "null"
    case JBool(b) => b.toString
    case JNum(n) => if n == n.floor && n.abs < 1e15 then n.toLong.toString else n.toString
    case JStr(s) => "\"" + escape(s) + "\""
    case JArr(vs) => vs.map(print).mkString("[", ",", "]")
    case JObj(fs) => fs.map((k, v) => "\"" + escape(k) + "\":" + print(v)).mkString("{", ",", "}")
    case JErr(m) => "\"<error: " + escape(m) + ">\""

  /**
   * The total pipeline: any string yields a Json (JErr for damage).
   *
   * It takes the FAST road — one strict pass, no tokens, no tree —
   * and falls back to the lossless one whenever that road is not
   * sure, so damage still gets the CST's exact answer. Same values,
   * same totality; only the trivia is not kept, and nothing that
   * returns a `Json` ever wanted the trivia.
   *
   * Measured 2026-09-07 (json-parse-fast-road) on a 19.7 MB frame out
   * of okay-py: the lossless road 4.9 s, this one 62 ms — 79x, for an
   * equal value. `Codecs.readJson`, the generic decode door the whole
   * stack goes through, was paying that; so was every caller in
   * twenty-four files. A caller that genuinely wants the tree calls
   * `cst` and `value` itself.
   */
  def parse(s: String): Json = JsonValue.parse(s).getOrElse(lossless(s))

  /** the tokenize-then-project road, kept NAMED so the agreement
   * test can still compare the two and so a caller who wants the
   * CST's reading of damaged text can ask for it */
  def lossless(s: String): Json = value(cst(s))

  /**
   * RFC 7396 JSON Merge Patch, applied: an object PATCH recursively
   * merges into TARGET field by field (a target that is not itself
   * an object is treated as `{}`, per the RFC), a `null` field
   * DELETES that key, and any other value replaces it wholesale —
   * so a scalar or array patch always replaces, never merges. Any
   * non-object patch simply becomes the new value: `mergePatch(t,
   * JNum(1))` is `JNum(1)` regardless of `t`. Pure, total, and
   * self-composing only up to the caveat RFC 7396 itself has —
   * `mergePatch(mergePatch(t, p1), p2)` is not always the same value
   * as `mergePatch(t, mergePatch(p1, p2))` when `p2` deletes a key
   * that `t` carried and `p1` never mentioned (the combined patch has
   * nothing to delete, because it was never told the key existed) —
   * a caller composing patches across a boundary it does not control
   * the whole history of should apply them in order, not combine
   * them first.
   */
  def mergePatch(target: Json, patch: Json): Json = patch match
    case JObj(patchFields) =>
      val base = target match
        case JObj(fs) => fs
        case _ => Vector.empty
      val merged = patchFields.foldLeft(base) { (acc, kv) =>
        val (k, v) = kv
        val without = acc.filterNot(_._1 == k)
        v match
          case JNull => without
          case _ =>
            val orig = acc.find(_._1 == k).map(_._2).getOrElse(JNull)
            without :+ (k -> mergePatch(orig, v))
      }
      JObj(merged)
    case other => other

    /** the projection of an ALREADY PARSED tree — the door for anyone
   * holding a session (an incremental reparse, say) who should not
   * pay to parse the text a second time */
  def value(c: Cst[K]): Json =
    values(c).headOption.getOrElse(JErr("empty input"))

  /** a hex digit string, parsed as one UTF-16 code unit; anything not
   * four clean hex digits is not a code point this function can name.
   * Package-visible: JsonValue's fast path decodes the identical
   * escape and must agree with this one exactly, not just resemble it. */
  private[codec] def hex4(s: String): Option[Char] =
    if s.length == 4 && s.forall(c => c.isDigit ||
      (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
    then scala.util.Try(Integer.parseInt(s, 16).toChar).toOption
    else None

  private def unquote(lexeme: String): String =
    val from = if lexeme.startsWith("\"") then 1 else 0
    val to = if lexeme.length > from && lexeme.endsWith("\"") then lexeme.length - 1 else lexeme.length
    // the common string holds no escape at all, and then the answer
    // IS the substring: no builder, no copy beyond the one substring
    // Java gives us anyway (json-projection-alloc)
    if lexeme.indexOf('\\', from) < 0 then lexeme.substring(from, to)
    else unescape(lexeme.substring(from, to))

  private def unescape(inner: String): String =
    val b = new StringBuilder(inner.length)
    var i = 0
    while i < inner.length do
      val c = inner.charAt(i)
      if c == '\\' && i + 1 < inner.length then
        inner.charAt(i + 1) match
          case 'n' => b.append('\n'); i += 2
          case 't' => b.append('\t'); i += 2
          case 'r' => b.append('\r'); i += 2
          // a surrogate PAIR needs no special handling here: two
          // \uXXXX escapes that each decode to one UTF-16 code unit,
          // appended in order, are already a correct Scala String —
          // that is what a UTF-16 string always was
          case 'u' if i + 6 <= inner.length =>
            hex4(inner.substring(i + 2, i + 6)) match
              case Some(ch) => b.append(ch); i += 6
              // damage, not a throw: unquote returns a bare String, so
              // there is no JErr for a malformed escape to become —
              // every other case here already falls back to the
              // literal character rather than failing loudly
              case None => b.append('u'); i += 2
          case x => b.append(x); i += 2
      else { b.append(c); i += 1 }
    b.toString

  /** the semantic values among a node's children (trivia and
   * punctuation fall away; errors stay, as JErr) */
  private def values(c: Cst[K]): Vector[Json] =
    val out = Vector.newBuilder[Json]
    into(c, out, 0)
    out.result()

  /**
   * The projection, written to APPEND rather than to answer.
   *
   * It used to return a `Vector[Json]` from every node and every leaf
   * — `Vector(JNull)`, `Vector(JBool(b))` — purely to say "none, one
   * or many", with `kids.flatMap(values)` allocating an intermediate
   * at every level. That is one Vector per token on a road whose
   * whole job is to walk tokens (json-projection-alloc).
   *
   * PAST `Codecs.NativeThreshold`, dispatches to `intoC`'s
   * `Cont.defer` trampoline (json-raw-nesting-threshold-trampoline).
   * `Unit`-returning and side-effecting into `out` rather than
   * combining typed values — the one target of the four whose SHAPE
   * differs from the other three — but the mechanism is unchanged: a
   * mutable `Builder`/`var` closed over by a deferred step still
   * mutates in the SAME order once the trampoline reaches that step,
   * so `Cont.defer` composes with side effects exactly as it composes
   * with values.
   */
  private def into(c: Cst[K], out: scala.collection.mutable.Builder[Json, Vector[Json]], open: Int): Unit =
    if open >= Codecs.NativeThreshold then reset(intoC[Unit](c, out, open))
    else intoNative(c, out, open)

  private def intoNative(c: Cst[K], out: scala.collection.mutable.Builder[Json, Vector[Json]], open: Int): Unit = c match
    case Cst.Node("object", kids) =>
      if open >= maxDepth then out += tooDeep
      else
        val fs = pairs(kids, open + 1)      // the DISPATCHER
        // a container holding the cut IS the cut: depth is a property
        // of the DOCUMENT, not damage at a spot (cut-refuses-the-document)
        out += (if fs.exists((_, v) => isCut(v)) then tooDeep else JObj(fs))
    case Cst.Node("array", kids) =>
      if open >= maxDepth then out += tooDeep
      else
        val vs = Vector.newBuilder[Json]
        kids.foreach(into(_, vs, open + 1))   // the DISPATCHER
        val es = vs.result()
        out += (if es.exists(isCut) then tooDeep else JArr(es))
    // a node that is not a container is not a level: the wrappers the
    // grammar puts between them must not spend the budget
    case Cst.Node(_, kids) => kids.foreach(into(_, out, open))   // the DISPATCHER
    case Cst.Leaf(t) => t.kind match
      case K.Str => out += JStr(unquote(t.lexeme))
      case K.Num =>
        // the lexer's Num class is a superset of Java's parseable
        // doubles — a torn frame ends in "-" or "1e", and the lexer
        // rightly calls that a number-shaped lexeme. Totality is THIS
        // layer's promise too: damage becomes a JErr, never a throw.
        // Found by an NIO transport benchmark whose last line was cut
        // mid-number; five inputs crashed the "total" parser.
        t.lexeme.toDoubleOption match
          case Some(d) => out += JNum(d)
          case None => out += JErr(s"malformed number '${t.lexeme}'")
      case K.Bool => out += JBool(t.lexeme == "true")
      case K.Null => out += JNull
      case _ => ()
    case Cst.Err(t, m) => out += JErr(m + t.fold("")(x => s" at '${x.lexeme}'"))

  /** a field is a key and the value after it — read in ONE pass, in
   * place of a flatMap into a Vector and a grouped(2) that allocated
   * another Vector per field */
  private def pairs(kids: Vector[Cst[K]], open: Int): Vector[(String, Json)] =
    if open >= Codecs.NativeThreshold then reset(pairsC[Vector[(String, Json)]](kids, open))
    else pairsNative(kids, open)

  private def pairsNative(kids: Vector[Cst[K]], open: Int): Vector[(String, Json)] =
    val vs = Vector.newBuilder[Json]
    kids.foreach(into(_, vs, open))          // the DISPATCHER
    val flat = vs.result()
    val out = Vector.newBuilder[(String, Json)]
    var i = 0
    while i + 1 < flat.length do
      flat(i) match
        case JStr(k) => out += ((k, flat(i + 1)))
        case JErr(m) => out += ((s"<$m>", flat(i + 1)))
        // a key that is neither is dropped, exactly as the
        // grouped/collect pair dropped it
        case _ => ()
      i += 2
    out.result()

  // ---- the trampoline: mirrors intoNative/pairsNative exactly,
  // deferring each element/field of a SIBLING loop through
  // Cont.defer, and the ONE recursive descent (a nested container)
  // through the same mechanism ----

  private def intoC[R](c: Cst[K], out: scala.collection.mutable.Builder[Json, Vector[Json]], open: Int): Unit /> R = c match
    case Cst.Node("object", kids) =>
      if open >= maxDepth then { out += tooDeep; Cont.Pure(()) }
      else pairsC[R](kids, open + 1).flatMap { fs =>
        out += (if fs.exists((_, v) => isCut(v)) then tooDeep else JObj(fs))
        Cont.Pure(())
      }
    case Cst.Node("array", kids) =>
      if open >= maxDepth then { out += tooDeep; Cont.Pure(()) }
      else
        val vs = Vector.newBuilder[Json]
        def loop(rest: Vector[Cst[K]]): Unit /> R =
          if rest.isEmpty then Cont.Pure(())
          else Cont.defer(() => intoC[R](rest.head, vs, open + 1))(_ => loop(rest.tail))
        loop(kids).flatMap { _ =>
          val es = vs.result()
          out += (if es.exists(isCut) then tooDeep else JArr(es))
          Cont.Pure(())
        }
    case Cst.Node(_, kids) =>
      def loop(rest: Vector[Cst[K]]): Unit /> R =
        if rest.isEmpty then Cont.Pure(())
        else Cont.defer(() => intoC[R](rest.head, out, open))(_ => loop(rest.tail))
      loop(kids)
    case Cst.Leaf(t) =>
      t.kind match
        case K.Str => out += JStr(unquote(t.lexeme))
        case K.Num => t.lexeme.toDoubleOption match
          case Some(d) => out += JNum(d)
          case None => out += JErr(s"malformed number '${t.lexeme}'")
        case K.Bool => out += JBool(t.lexeme == "true")
        case K.Null => out += JNull
        case _ => ()
      Cont.Pure(())
    case Cst.Err(t, m) =>
      out += JErr(m + t.fold("")(x => s" at '${x.lexeme}'"))
      Cont.Pure(())

  private def pairsC[R](kids: Vector[Cst[K]], open: Int): Vector[(String, Json)] /> R =
    val vs = Vector.newBuilder[Json]
    def loop(rest: Vector[Cst[K]]): Unit /> R =
      if rest.isEmpty then Cont.Pure(())
      else Cont.defer(() => intoC[R](rest.head, vs, open))(_ => loop(rest.tail))
    loop(kids).flatMap { _ =>
      val flat = vs.result()
      val out = Vector.newBuilder[(String, Json)]
      var i = 0
      while i + 1 < flat.length do
        flat(i) match
          case JStr(k) => out += ((k, flat(i + 1)))
          case JErr(m) => out += ((s"<$m>", flat(i + 1)))
          case _ => ()
        i += 2
      Cont.Pure(out.result())
    }

  // ----------------------------------------------------------------
  // the two Schema algebras

  /**
   * JSON string escaping, public: a staged or hand-written encoder
   * needs the same rule.
   *
   * FIVE characters and no others — this project deliberately leaves
   * `\b`, `\f` and controls alone (see `unquote`, which calls that its
   * own choice), and `escape`/`unescape` must agree exactly rather
   * than resemble each other. TestJsonEscape pins that.
   *
   * The shape is `unquote`'s (json-escape-alloc): look first, and
   * answer the INPUT when there is nothing to do. It used to be
   * `s.flatMap { ... case c => c.toString }` — a String allocated per
   * CHARACTER — on a path `Json.print`, `Staged` and `RuntimeStaged`
   * all take, so every string through the staged doors paid it.
   * Measured over 200k strings: 13.5 ms to 4.7.
   */
  def escape(s: String): String =
    var i = 0
    while i < s.length && !needsEscape(s.charAt(i)) do i += 1
    if i == s.length then s
    else
      // java.lang's, deliberately: scala's StringBuilder has an
      // append(Any), so append(s, 0, i) silently appends the TUPLE
      // (found by TestJsonEscape, which was written first)
      val b = new java.lang.StringBuilder(s.length + 8)
      b.append(s, 0, i)
      while i < s.length do
        val c = s.charAt(i)
        c match
          case '"' => b.append("\\\"")
          case '\\' => b.append("\\\\")
          case '\n' => b.append("\\n")
          case '\t' => b.append("\\t")
          case '\r' => b.append("\\r")
          case _ => b.append(c)
        i += 1
      b.toString

  private inline def needsEscape(c: Char): Boolean =
    c == '"' || c == '\\' || c == '\n' || c == '\t' || c == '\r' 

  /** the encoding algebra: fold the schema, render the value */
  def encode[A](s: Schema[A])(a: A): String = s match
    case Schema.SInt => a.toString
    case Schema.SLong => a.toString
    case Schema.SDouble => a.toString
    case Schema.SBool => a.toString
    case Schema.SString => s"\"${escape(a)}\""
    case Schema.SChar => s"\"${escape(a.toString)}\""
    // JSON has no bytes. Base64 is what everyone means by them here,
    // and it is also what makes a dump READABLE: a thousand float
    // literals are not something anyone reads, and one opaque token
    // says "binary payload" without burying the fields that matter.
    case Schema.SBytes => s"\"${Base64.encode(a)}\""
    case Schema.SOption(of) =>
      a match
        case Some(x) => encode(of())(x)
        case None => "null"
    case Schema.SList(of) =>
      a.map(encode(of())).mkString("[", ",", "]")
    case Schema.SVector(of) =>
      a.map(encode(of())).mkString("[", ",", "]")
    case p: Schema.SProduct[A] =>
      p.eachField(a)([X] => (n: String, sc: Schema[X], x: X) => s"\"$n\":${encode(sc)(x)}")
        .mkString("{", ",", "}")
    case su: Schema.SSum[A] =>
      su.theCase(a)([X <: A] => (n: String, sc: Schema[X], x: X) => s"{\"$n\":${encode(sc)(x)}}")
    // the newtype node: A travels as B, so encode is `from` then under's
    case Schema.SIso(u, _, from) => encode(u())(from(a))

  /** the public entry, signature unchanged: dispatches on depth,
   * starting at 0. `Json.decode` has no reader object to hang a
   * counter on (it is a pure function of `Schema`/`Json`), so depth is
   * an explicit parameter rather than `Cbor.In`'s mutable `open`. Not
   * also a refusal check the way CBOR's is: a too-deep DOCUMENT is
   * already cut before `decode` ever sees it (`Json.isCut`, at the
   * parse/projection layer) — this threshold exists only so a
   * RECURSIVE schema's native call depth cannot grow with input depth,
   * independent of whether that input came through the cut at all
   * (`decode` is public and callable on any `Json` value directly). */
  def decode[A](s: Schema[A])(j: Json): Either[String, A] = decodeAt(s, j, 0)

  private def decodeAt[A](s: Schema[A], j: Json, depth: Int): Either[String, A] =
    if depth >= Codecs.NativeThreshold then reset(decodeC[A, Either[String, A]](s, j))
    else decodeNative(s, j, depth)

  /** one field at its own type; the value joins the product's erased
   * parts (Mirror's fromProduct takes Any) */
  private def field[X](sc: Schema[X], v: Json, depth: Int): Either[String, Any] = decodeAt(sc, v, depth)

  /** the decoding algebra: fold the schema, read the value back —
   * errors are values (Left), never faults */
  private def decodeNative[A](s: Schema[A], j: Json, depth: Int): Either[String, A] = (s, j) match
    case (Schema.SInt, JNum(n)) => Right(n.toInt)
    case (Schema.SLong, JNum(n)) => Right(n.toLong)
    case (Schema.SDouble, JNum(n)) => Right(n)
    case (Schema.SBool, JBool(b)) => Right(b)
    case (Schema.SString, JStr(x)) => Right(x)
    case (Schema.SChar, JStr(x)) if x.length == 1 => Right(x.head)
    case (Schema.SChar, JStr(x)) => Left(s"expected one character, got ${x.length}")
    case (Schema.SBytes, JStr(x)) => Base64.decode(x)
    case (Schema.SOption(of), JNull) => Right(None)
    case (Schema.SOption(of), v) => decodeAt(of(), v, depth + 1).map(Some(_))
    case (l: Schema.SList[a], JArr(vs)) =>
      // A truncated document leaves an "unclosed" marker where its
      // last element would be, and a damaged one leaves a JErr in
      // place of a value. Failing the whole list on either would
      // throw away the elements that DID arrive — which is the exact
      // opposite of why this stack is total. So error elements are
      // skipped here; they remain visible in the projection
      // (Json.parse) and in the tree (Cst.errors) for anyone who
      // wants to know that the document was damaged.
      vs.filterNot(_.isInstanceOf[JErr])
        .foldLeft(Right(Nil): Either[String, List[a]]) { (acc, v) =>
          acc.flatMap(xs => decodeAt(l.of(), v, depth + 1).map(xs :+ _))
        }
    case (vec: Schema.SVector[a], JArr(vs)) =>
      // the same totality rule as SList above: damaged elements are
      // skipped, the ones that arrived survive
      vs.filterNot(_.isInstanceOf[JErr])
        .foldLeft(Right(Vector.empty): Either[String, Vector[a]]) { (acc, v) =>
          acc.flatMap(xs => decodeAt(vec.of(), v, depth + 1).map(xs :+ _))
        }
    case (p: Schema.SProduct[A], JObj(fs)) =>
      val m = fs.toMap
      p.fields.zipWithIndex.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, fi) =>
        val (f, i) = fi
        // an absent (or damaged-optional) field takes, in order: its
        // DECLARED default, None-if-optional, the missing refusal
        def absent: Either[String, Any] = p.defaults.lift(i).flatten match
          case Some(d) => Right(d())
          case None => f._2() match
            case _: Schema.SOption[?] => Right(None)
            case _ => Left(s"missing field '${f._1}' in ${p.name}")
        acc.flatMap { xs =>
          (m.get(f._1), f._2()) match
            case (None, _) => absent.map(xs :+ _)
            // a damaged optional value is the same as an absent one (a
            // cut cannot reach here: it propagates to the root)
            case (Some(JErr(_)), _: Schema.SOption[?]) => absent.map(xs :+ _)
            case (found, sc) => found.toRight(s"missing field '${f._1}' in ${p.name}")
              .flatMap(field(sc, _, depth + 1)).map(xs :+ _)
        }
      }.map(p.make)
    case (su: Schema.SSum[A], JObj(Vector((name, v)))) =>
      su.cases.find(_._1 == name)
        .toRight(s"unknown case '$name' of ${su.name}")
        .flatMap((_, sc) => decodeAt(sc(), v, depth + 1))
    case (Schema.SIso(u, to, _), v) => decodeAt(u(), v, depth + 1).flatMap(to)
    case (_, JErr(m)) => Left(m)
    case (want, got) => Left(s"expected ${want.getClass.getSimpleName}, got $got")

  // ---------------------------------------------------------------
  // the trampoline (json-decode-threshold-trampoline): PAST
  // NativeThreshold, `decodeAt` runs this instead of `decodeNative`.
  // Same fold, same rules — the ONLY difference is that a descent into
  // a NESTED schema is `Cont.defer`red rather than called directly, so
  // the trampoline forces it inside `/`'s own loop, one level per
  // iteration, at constant native stack. See `Cbor.scala`'s own
  // `getC`/`insideC`/`fieldC` (cbor-decode-threshold-trampoline) for
  // the mechanism in full — this is the same design against a
  // different fold. `R` is fixed once, at `decodeAt`'s `reset` call,
  // to `Either[String, A]` for whatever `A` was being decoded when
  // depth first crossed the threshold, and threaded unchanged through
  // every nested call below.
  //
  // No cast (no-casts-without-necessity): the two widenings
  // `decodeNative` gets for free from `Either`'s covariance (a
  // product's field joining `Vector[Any]`, a sum's case narrowing to
  // its parent type) need one explicit `.map` each here, since `Cont`
  // is invariant in its value type.
  // ---------------------------------------------------------------

  /** `field`'s Cont-shaped twin, widened to `Any` the same way `field`
   * widens via Either's covariance */
  private def fieldC[X, R](sc: Schema[X], v: Json): Either[String, Any] /> R =
    decodeC(sc, v).map(e => e: Either[String, Any])

  private def decodeC[A, R](s: Schema[A], j: Json): Either[String, A] /> R = (s, j) match
    case (Schema.SInt, JNum(n)) => Cont.Pure(Right(n.toInt))
    case (Schema.SLong, JNum(n)) => Cont.Pure(Right(n.toLong))
    case (Schema.SDouble, JNum(n)) => Cont.Pure(Right(n))
    case (Schema.SBool, JBool(b)) => Cont.Pure(Right(b))
    case (Schema.SString, JStr(x)) => Cont.Pure(Right(x))
    case (Schema.SChar, JStr(x)) if x.length == 1 => Cont.Pure(Right(x.head))
    case (Schema.SChar, JStr(x)) => Cont.Pure(Left(s"expected one character, got ${x.length}"))
    case (Schema.SBytes, JStr(x)) => Cont.Pure(Base64.decode(x))
    case (Schema.SOption(of), JNull) => Cont.Pure(Right(None))
    case (Schema.SOption(of), v) =>
      Cont.defer(() => decodeC(of(), v))(r => Cont.Pure(r.map(Some(_))))
    case (l: Schema.SList[a], JArr(vs)) =>
      def loop(rest: List[Json], acc: List[a]): Either[String, List[a]] /> R = rest match
        case Nil => Cont.Pure(Right(acc.reverse))
        case v :: more => Cont.defer(() => decodeC(l.of(), v)) {
          case Left(e) => Cont.Pure(Left(e))
          case Right(x) => loop(more, x :: acc)
        }
      loop(vs.filterNot(_.isInstanceOf[JErr]).toList, Nil)
    case (vec: Schema.SVector[a], JArr(vs)) =>
      def loop(rest: List[Json], acc: Vector[a]): Either[String, Vector[a]] /> R = rest match
        case Nil => Cont.Pure(Right(acc))
        case v :: more => Cont.defer(() => decodeC(vec.of(), v)) {
          case Left(e) => Cont.Pure(Left(e))
          case Right(x) => loop(more, acc :+ x)
        }
      loop(vs.filterNot(_.isInstanceOf[JErr]).toList, Vector.empty)
    case (p: Schema.SProduct[A], JObj(fs)) =>
      val m = fs.toMap
      def loop(remaining: List[((String, () => Schema[?]), Int)], acc: Vector[Any]): Either[String, Vector[Any]] /> R =
        remaining match
          case Nil => Cont.Pure(Right(acc))
          case (f, i) :: more =>
            def absent: Either[String, Any] = p.defaults.lift(i).flatten match
              case Some(d) => Right(d())
              case None => f._2() match
                case _: Schema.SOption[?] => Right(None)
                case _ => Left(s"missing field '${f._1}' in ${p.name}")
            (m.get(f._1), f._2()) match
              case (None, _) => absent match
                case Left(e) => Cont.Pure(Left(e))
                case Right(v) => loop(more, acc :+ v)
              case (Some(JErr(_)), _: Schema.SOption[?]) => absent match
                case Left(e) => Cont.Pure(Left(e))
                case Right(v) => loop(more, acc :+ v)
              case (Some(v), sc) => Cont.defer(() => fieldC(sc, v)) {
                case Left(e) => Cont.Pure(Left(e))
                case Right(x) => loop(more, acc :+ x)
              }
      loop(p.fields.zipWithIndex.toList, Vector.empty).flatMap(r => Cont.Pure(r.map(p.make)))
    case (su: Schema.SSum[A], JObj(Vector((name, v)))) =>
      su.cases.find(_._1 == name) match
        case None => Cont.Pure(Left(s"unknown case '$name' of ${su.name}"))
        case Some((_, sc)) => decodeC(sc(), v).map(e => e: Either[String, A])
    case (Schema.SIso(u, to, _), v) =>
      Cont.defer(() => decodeC(u(), v))(r => Cont.Pure(r.flatMap(to)))
    case (_, JErr(m)) => Cont.Pure(Left(m))
    case (want, got) => Cont.Pure(Left(s"expected ${want.getClass.getSimpleName}, got $got"))

  /** text to value in one move, through the total pipeline */
  def read[A](input: String)(using s: Schema[A]): Either[String, A] =
    decode(s)(parse(input))

  /**
   * THE OTHER DOOR: text to value with no tree in between — characters
   * straight into the `Schema` (`JsonStrict`). The same answer as
   * `read` on a complete, well-formed document (TestJsonStrict holds
   * them equal), at a fraction of the cost, and `Left` on anything it
   * is not sure of: a truncated document, a damaged one, a stray or
   * trailing character. `read` still decodes those — a half-arrived
   * document projects to what did arrive, damage becomes data — and
   * that is exactly the price list in docs/benchmarks.md: choose
   * `read` for the contract, `readStrict` for the speed, and never
   * wonder which you got.
   */
  def readStrict[A](input: String)(using s: Schema[A]): Either[String, A] =
    JsonStrict.read(input)

  /** value to text in one move */
  def write[A](a: A)(using s: Schema[A]): String = encode(s)(a)
}
