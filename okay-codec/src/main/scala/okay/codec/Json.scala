package okay.codec

import okay.{!, %, Writer, through, pure}
import okay.toLazyList
import okay.lex.Scan
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

  // ----------------------------------------------------------------
  // parse: chars -> scanner -> driver -> CST -> projection

  private def chars(s: String, i: Int = 0): Unit ! Writer % Char =
    if i >= s.length then pure(())
    else Writer.tell(s.charAt(i)).flatMap(_ => chars(s, i + 1))

  /** the lossless layer: any string yields a CST that render puts
   * back byte-for-byte (trivia, ordering, duplicate keys, damage) */
  def cst(s: String): Cst[K] = Parse.toCst(
    through(through(chars(s))(Scan.stage(JsonLex.scan)))(JsonParse.driver).toLazyList)

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

  /** the total pipeline: any string yields a Json (JErr for damage) */
  def parse(s: String): Json = value(cst(s))

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

  /** the same Json by the fast road (JsonValue: one strict pass, no
   * tokens, no tree) when the text is well formed, and by the
   * lossless road otherwise — so damage still gets the CST's exact
   * answer. Same values, same totality; only the trivia is not kept */
  def parseValue(s: String): Json = JsonValue.parse(s).getOrElse(parse(s))

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
    val inner = lexeme.stripPrefix("\"").stripSuffix("\"")
    val b = new StringBuilder
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
  private def values(c: Cst[K]): Vector[Json] = c match
    case Cst.Node("object", kids) => Vector(JObj(pairs(kids)))
    case Cst.Node("array", kids) => Vector(JArr(kids.flatMap(values)))
    case Cst.Node(_, kids) => kids.flatMap(values)
    case Cst.Leaf(t) => t.kind match
      case K.Str => Vector(JStr(unquote(t.lexeme)))
      case K.Num =>
        // the lexer's Num class is a superset of Java's parseable
        // doubles — a torn frame ends in "-" or "1e", and the lexer
        // rightly calls that a number-shaped lexeme. Totality is THIS
        // layer's promise too: damage becomes a JErr, never a throw.
        // Found by an NIO transport benchmark whose last line was cut
        // mid-number; five inputs crashed the "total" parser.
        t.lexeme.toDoubleOption match
          case Some(d) => Vector(JNum(d))
          case None => Vector(JErr(s"malformed number '${t.lexeme}'"))
      case K.Bool => Vector(JBool(t.lexeme == "true"))
      case K.Null => Vector(JNull)
      case _ => Vector.empty
    case Cst.Err(t, m) => Vector(JErr(m + t.fold("")(x => s" at '${x.lexeme}'")))

  private def pairs(kids: Vector[Cst[K]]): Vector[(String, Json)] =
    val vs = kids.flatMap(values)
    vs.grouped(2).collect {
      case Vector(JStr(k), v) => (k, v)
      case Vector(JErr(m), v) => (s"<$m>", v)
    }.toVector

  // ----------------------------------------------------------------
  // the two Schema algebras

  /** JSON string escaping, public: a staged or hand-written encoder needs the same rule */
  def escape(s: String): String =
    s.flatMap {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case c => c.toString
    }

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

  /** one field at its own type; the value joins the product's erased
   * parts (Mirror's fromProduct takes Any) */
  private def field[X](sc: Schema[X], v: Json): Either[String, Any] = decode(sc)(v)

  /** the decoding algebra: fold the schema, read the value back —
   * errors are values (Left), never faults */
  def decode[A](s: Schema[A])(j: Json): Either[String, A] = (s, j) match
    case (Schema.SInt, JNum(n)) => Right(n.toInt)
    case (Schema.SLong, JNum(n)) => Right(n.toLong)
    case (Schema.SDouble, JNum(n)) => Right(n)
    case (Schema.SBool, JBool(b)) => Right(b)
    case (Schema.SString, JStr(x)) => Right(x)
    case (Schema.SChar, JStr(x)) if x.length == 1 => Right(x.head)
    case (Schema.SChar, JStr(x)) => Left(s"expected one character, got ${x.length}")
    case (Schema.SBytes, JStr(x)) => Base64.decode(x)
    case (Schema.SOption(of), JNull) => Right(None)
    case (Schema.SOption(of), v) => decode(of())(v).map(Some(_))
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
          acc.flatMap(xs => decode(l.of())(v).map(xs :+ _))
        }
    case (vec: Schema.SVector[a], JArr(vs)) =>
      // the same totality rule as SList above: damaged elements are
      // skipped, the ones that arrived survive
      vs.filterNot(_.isInstanceOf[JErr])
        .foldLeft(Right(Vector.empty): Either[String, Vector[a]]) { (acc, v) =>
          acc.flatMap(xs => decode(vec.of())(v).map(xs :+ _))
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
              .flatMap(field(sc, _)).map(xs :+ _)
        }
      }.map(p.make)
    case (su: Schema.SSum[A], JObj(Vector((name, v)))) =>
      su.cases.find(_._1 == name)
        .toRight(s"unknown case '$name' of ${su.name}")
        .flatMap((_, sc) => decode(sc())(v))
    case (Schema.SIso(u, to, _), v) => decode(u())(v).flatMap(to)
    case (_, JErr(m)) => Left(m)
    case (want, got) => Left(s"expected ${want.getClass.getSimpleName}, got $got")

  /** text to value in one move, through the total pipeline */
  def read[A](input: String)(using s: Schema[A]): Either[String, A] =
    decode(s)(parse(input))

  /** value to text in one move */
  def write[A](a: A)(using s: Schema[A]): String = encode(s)(a)
}
