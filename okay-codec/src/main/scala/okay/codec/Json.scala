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
into enum Json:
  case JNull
  case JBool(b: Boolean)
  case JNum(n: Double)
  case JStr(s: String)
  case JArr(vs: Vector[Json])
  case JObj(fs: Vector[(String, Json)])
  case JErr(message: String)

object Json {

  /**
   * OPT-IN literal transparency: `import Json.literals.given` and a
   * JSON literal may be written where a `Json` is expected.
   *
   *     import Json.literals.given
   *     obj("name" -> "ada", "age" -> 36, "ok" -> true)
   *
   * Nothing here applies unless that import is written. `Json` is
   * declared `into`, which only means "conversions to me need no
   * `import scala.language.implicitConversions`"; the conversions
   * themselves live in this object, so a file that does not ask for
   * them sees the ordinary, safe API and a plain type error.
   *
   * A STRING LITERAL CONVERTS, A `String` VALUE DOES NOT, and that is
   * the point rather than an omission. `Json.parse(s)` answers a
   * `Json`, so in a file with an unrestricted `Conversion[String,
   * Json]` a caller who passes an already-serialized document would
   * get `JStr(document)` — double encoding, silently, in the module
   * whose job is encoding. A conversion is chosen by TYPE and both
   * meanings are `String`, so it cannot read the intent. The
   * conversion below therefore accepts constant types only and tells
   * a value what to write instead.
   *
   * `Int`, `Double` and `Boolean` convert plainly: no document can
   * hide inside them. `Long` is DELIBERATELY ABSENT — `JNum` holds a
   * `Double`, so a `Long` past 2^53 would convert with silent
   * precision loss, which is the same defect one paragraph up. Write
   * `JNum(x.toDouble)` where that is what you mean.
   *
   * `===` compares a `Json` with anything this import converts, so
   * `json === "x"` reads as the comparison it is. It is sugar, not a
   * guard: `json == "x"` does not compile with or without this
   * import, because Scala 3 derives `CanEqual` for enums and refuses
   * comparison with an unrelated type.
   *
   * specs/codecs.md, "Literal transparency, opt-in".
   */
  object literals:
    import scala.compiletime.{constValueOpt, error}

    /** a string LITERAL, and nothing else */
    inline given [L <: String & Singleton] => Conversion[L, Json] =
      inline constValueOpt[L] match
        case Some(_) => (s: L) => Json.JStr(s)
        case None => error(
          "only a string LITERAL converts to Json here. For a String " +
          "value write JStr(x); if it holds a serialized document you " +
          "want Json.parse(x). See Json.literals.")

    given Conversion[Int, Json] = i => Json.JNum(i.toDouble)
    given Conversion[Double, Json] = Json.JNum(_)
    given Conversion[Boolean, Json] = Json.JBool(_)

    /** compare a Json with anything this import converts */
    extension (j: Json) def ===(that: Json): Boolean = j == that

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

  /**
   * print a Json VALUE back to text — the projection's other
   * direction (render is the CST's; this one is the value's).
   *
   * `remove-codecs-maxdepth` made every DECODE door safe at any input
   * depth but never touched this one — the write half of the same
   * pipe. A value big enough to have needed the removed cap to
   * DECODE can now exist in memory, and printing it back out was
   * still plain native recursion (encode-side-depth-safety): the same
   * `Codecs.NativeThreshold`-then-`Cont.defer` split `into`/`intoC`
   * above already carry, side-effecting into a `StringBuilder` the
   * way `into` side-effects into a `Builder`.
   */
  def print(j: Json): String =
    val sb = new StringBuilder
    printInto(j, sb, 0)
    sb.toString

  private def printInto(j: Json, sb: StringBuilder, open: Int): Unit =
    if open >= Codecs.NativeThreshold then reset(printIntoC[Unit](j, sb, open))
    else printIntoNative(j, sb, open)

  private def printIntoNative(j: Json, sb: StringBuilder, open: Int): Unit = j match
    case JArr(vs) =>
      sb.append('[')
      var first = true
      vs.foreach { v =>
        if !first then sb.append(',')
        first = false
        printInto(v, sb, open + 1)          // the DISPATCHER
      }
      sb.append(']'): Unit
    case JObj(fs) =>
      sb.append('{')
      var first = true
      fs.foreach { (k, v) =>
        if !first then sb.append(',')
        first = false
        val _ = sb.append('"').append(escape(k)).append("\":")
        printInto(v, sb, open + 1)          // the DISPATCHER
      }
      sb.append('}'): Unit
    case leaf => printLeaf(leaf, sb)

  private def printLeaf(j: Json, sb: StringBuilder): Unit = j match
    case JNull => sb.append("null"): Unit
    case JBool(b) => sb.append(b): Unit
    case JNum(n) =>
      sb.append(if n == n.floor && n.abs < 1e15 then n.toLong.toString else n.toString): Unit
    case JStr(s) => sb.append('"').append(escape(s)).append('"'): Unit
    case JErr(m) => sb.append("\"<error: ").append(escape(m)).append(">\""): Unit
    case JArr(_) | JObj(_) => () // unreachable: the caller handles containers

  // ---- the trampoline: mirrors printIntoNative exactly, deferring
  // each element/field of a sibling loop through Cont.defer, and the
  // one recursive descent (a nested container) through the same
  // mechanism, exactly as intoC does above ----

  private def printIntoC[R](j: Json, sb: StringBuilder, open: Int): Unit /> R = j match
    case JArr(vs) =>
      sb.append('[')
      def loop(rest: Vector[Json], first: Boolean): Unit /> R =
        if rest.isEmpty then { sb.append(']'); Cont.Pure(()) }
        else
          if !first then sb.append(',')
          Cont.defer(() => printIntoC[R](rest.head, sb, open + 1))(_ => loop(rest.tail, false))
      loop(vs, true)
    case JObj(fs) =>
      sb.append('{')
      def loop(rest: Vector[(String, Json)], first: Boolean): Unit /> R =
        if rest.isEmpty then { sb.append('}'); Cont.Pure(()) }
        else
          val (k, v) = rest.head
          if !first then sb.append(',')
          val _ = sb.append('"').append(escape(k)).append("\":")
          Cont.defer(() => printIntoC[R](v, sb, open + 1))(_ => loop(rest.tail, false))
      loop(fs, true)
    case leaf =>
      printLeaf(leaf, sb)
      Cont.Pure(())

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
   *
   * Recurses on the PATCH's own depth, not a schema's — the same
   * exposure `print`/`into` have (encode-side-depth-safety): the same
   * `Codecs.NativeThreshold`-then-`Cont.defer` split, folding the
   * patch's fields the way `pairsC` above folds a CST's.
   */
  def mergePatch(target: Json, patch: Json): Json = mergePatchAt(target, patch, 0)

  private def mergePatchAt(target: Json, patch: Json, open: Int): Json =
    if open >= Codecs.NativeThreshold then reset(mergePatchC[Json](target, patch, open))
    else mergePatchNative(target, patch, open)

  private def mergePatchNative(target: Json, patch: Json, open: Int): Json = patch match
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
            without :+ (k -> mergePatchAt(orig, v, open + 1))     // the DISPATCHER
      }
      JObj(merged)
    case other => other

  private def mergePatchC[R](target: Json, patch: Json, open: Int): Json /> R = patch match
    case JObj(patchFields) =>
      val base = target match
        case JObj(fs) => fs
        case _ => Vector.empty
      def loop(rest: Vector[(String, Json)], acc: Vector[(String, Json)]): Vector[(String, Json)] /> R =
        if rest.isEmpty then Cont.Pure(acc)
        else
          val (k, v) = rest.head
          val without = acc.filterNot(_._1 == k)
          v match
            case JNull => loop(rest.tail, without)
            case _ =>
              val orig = acc.find(_._1 == k).map(_._2).getOrElse(JNull)
              Cont.defer(() => mergePatchC[R](orig, v, open + 1))(merged => loop(rest.tail, without :+ (k -> merged)))
      loop(patchFields, base).flatMap(merged => Cont.Pure(JObj(merged)))
    case other => Cont.Pure(other)

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
      out += JObj(pairs(kids, open + 1))      // the DISPATCHER
    case Cst.Node("array", kids) =>
      val vs = Vector.newBuilder[Json]
      kids.foreach(into(_, vs, open + 1))     // the DISPATCHER
      out += JArr(vs.result())
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
      pairsC[R](kids, open + 1).flatMap { fs => out += JObj(fs); Cont.Pure(()) }
    case Cst.Node("array", kids) =>
      val vs = Vector.newBuilder[Json]
      def loop(rest: Vector[Cst[K]]): Unit /> R =
        if rest.isEmpty then Cont.Pure(())
        else Cont.defer(() => intoC[R](rest.head, vs, open + 1))(_ => loop(rest.tail))
      loop(kids).flatMap { _ => out += JArr(vs.result()); Cont.Pure(()) }
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

  /**
   * The encoding algebra, as a fold (specs/schema-fold.md, stage 2):
   * `Schema.fold` with `Enc` below, the value walk on `Schema.Step`.
   * No `match` on the GADT here and no depth logic — the
   * `NativeThreshold`-then-`Cont.defer` split lives in `Step.child`,
   * once, where five doors used to each carry a copy (this one's was
   * `encodeIntoNative`/`encodeIntoC`, now deleted). The fold is
   * memoised per schema by identity (`Schema.Folded`): built once,
   * reused per value.
   */
  def encode[A](s: Schema[A])(a: A): String =
    val sb = new StringBuilder
    Schema.Step.walk(encoder(s), sb, a)
    sb.toString

  private type Enc[A] = Schema.Step[StringBuilder, A, Unit]
  private val encoder = Schema.Folded[Enc](new Schema.Algebra[Enc]:
    import Schema.Step
    def int = Step.leaf((sb, a: Int) => sb.append(a.toString): Unit)
    def long = Step.leaf((sb, a: Long) => sb.append(a.toString): Unit)
    def double = Step.leaf((sb, a: Double) => sb.append(a.toString): Unit)
    def bool = Step.leaf((sb, a: Boolean) => sb.append(a.toString): Unit)
    def string = Step.leaf((sb, a: String) => sb.append('"').append(escape(a)).append('"'): Unit)
    def char = Step.leaf((sb, a: Char) => sb.append('"').append(escape(a.toString)).append('"'): Unit)
    // JSON has no bytes. Base64 is what everyone means by them here,
    // and it is also what makes a dump READABLE: a thousand float
    // literals are not something anyone reads, and one opaque token
    // says "binary payload" without burying the fields that matter.
    def bytes = Step.leaf((sb, a: Array[Byte]) => sb.append('"').append(Base64.encode(a)).append('"'): Unit)
    def option[A](o: Schema.SOption[A], of: () => Enc[A]) = Step.option(sb => sb.append("null"): Unit, of)
    def list[A](l: Schema.SList[A], of: () => Enc[A]) = Step.elems[StringBuilder, List[A], A, Unit, Unit](
      (sb, _) => sb.append('['): Unit, identity, of,
      (sb, i) => if i > 0 then sb.append(','): Unit,
      (_, _) => (), (sb, _, _) => sb.append(']'): Unit)
    def vector[A](v: Schema.SVector[A], of: () => Enc[A]) = Step.elems[StringBuilder, Vector[A], A, Unit, Unit](
      (sb, _) => sb.append('['): Unit, identity, of,
      (sb, i) => if i > 0 then sb.append(','): Unit,
      (_, _) => (), (sb, _, _) => sb.append(']'): Unit)
    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Enc, Any])]) =
      Step.fields[StringBuilder, A, Unit, Unit](
        (sb, _) => sb.append('{'): Unit, p.parts, fields,
        (sb, i, n) => { if i > 0 then sb.append(','); val _ = sb.append('"').append(n).append("\":") },
        (_, _) => (), (sb, _, _) => sb.append('}'): Unit)
    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Enc, A])]) =
      Step.one[StringBuilder, A, Unit, Unit](
        (sb, _) => sb.append('{'): Unit, su.caseOf, cases,
        (sb, n) => { val _ = sb.append('"').append(n).append("\":") },
        (sb, _, _, _) => sb.append('}'): Unit)
    // the newtype node: A travels as B, so encode is `from` then under's
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Enc[B]) = Step.via(iso.from, under)
    def ref[A](name: String) =
      throw IllegalStateException(s"a lazy carrier never meets a back edge, got one at $name")
  )

  /** the public entry, signature unchanged: dispatches on depth,
   * starting at 0. `Json.decode` has no reader object to hang a
   * counter on (it is a pure function of `Schema`/`Json`), so depth is
   * an explicit parameter rather than `Cbor.In`'s mutable `open`. This
   * counter drives ONLY the native/trampoline switch (`decode` never
   * refused a depth, before or after remove-codecs-maxdepth — the
   * refusal removed was upstream, at the parse/projection layer). */
  def decode[A](s: Schema[A])(j: Json): Either[String, A] = decodeAt(s, j, 0)

  private def decodeAt[A](s: Schema[A], j: Json, depth: Int): Either[String, A] =
    if depth >= Codecs.NativeThreshold then reset(decodeC[A, Either[String, A]](s, j))
    else decodeNative(s, j, depth)

  /** one field at its own type; the value joins the product's erased
   * parts (Mirror's fromProduct takes Any) */
  private def field[X](sc: Schema[X], v: Json, depth: Int): Either[String, Any] = decodeAt(sc, v, depth)

  /** the decoding algebra: fold the schema, read the value back —
   * errors are values (Left), never faults */
  /** the node's kind, for a refusal — `getClass.getSimpleName` was
    * used here and is EMPTY for an enum's singleton cases, so a wrong
    * scalar read "expected , got JStr(x)" (found by Validate's law
    * against decode, schema-fold stage 3) */
  private def kindOf(s: Schema[?]): String = s match
    case Schema.SInt => "SInt"
    case Schema.SLong => "SLong"
    case Schema.SDouble => "SDouble"
    case Schema.SBool => "SBool"
    case Schema.SString => "SString"
    case Schema.SChar => "SChar"
    case Schema.SBytes => "SBytes"
    case _: Schema.SOption[?] => "SOption"
    case _: Schema.SList[?] => "SList"
    case _: Schema.SVector[?] => "SVector"
    case p: Schema.SProduct[?] => p.name
    case su: Schema.SSum[?] => su.name
    case _: Schema.SIso[?, ?] => "SIso"

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
            // a damaged optional value is the same as an absent one
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
    case (want, got) => Left(s"expected ${kindOf(want)}, got $got")

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
    case (want, got) => Cont.Pure(Left(s"expected ${kindOf(want)}, got $got"))

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
