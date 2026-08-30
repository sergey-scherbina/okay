package okay.codec

import okay.{!, %, Writer, through, pure}
import okay.given
import okay.toLazyList
import okay.lex.{Channel, Scan, Token}
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

  /** the projection of an ALREADY PARSED tree — the door for anyone
   * holding a session (an incremental reparse, say) who should not
   * pay to parse the text a second time */
  def value(c: Cst[K]): Json =
    values(c).headOption.getOrElse(JErr("empty input"))

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
      case K.Num => Vector(JNum(t.lexeme.toDouble))
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

  private def escape(s: String): String =
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
    case Schema.SString => s"\"${escape(a.asInstanceOf[String])}\""
    case Schema.SOption(of) =>
      a.asInstanceOf[Option[Any]] match
        case Some(x) => encode(of().asInstanceOf[Schema[Any]])(x)
        case None => "null"
    case Schema.SList(of) =>
      a.asInstanceOf[List[Any]]
        .map(encode(of().asInstanceOf[Schema[Any]])).mkString("[", ",", "]")
    case p: Schema.SProduct[A] =>
      p.parts(a).zip(p.fields).map { (v, f) =>
        s"\"${f._1}\":${encode(f._2().asInstanceOf[Schema[Any]])(v)}"
      }.mkString("{", ",", "}")
    case su: Schema.SSum[A] =>
      val i = su.caseOf(a)
      val (name, sc) = su.cases(i)
      s"{\"$name\":${encode(sc().asInstanceOf[Schema[Any]])(a)}}"

  /** the decoding algebra: fold the schema, read the value back —
   * errors are values (Left), never faults */
  def decode[A](s: Schema[A])(j: Json): Either[String, A] = (s, j) match
    case (Schema.SInt, JNum(n)) => Right(n.toInt.asInstanceOf[A])
    case (Schema.SLong, JNum(n)) => Right(n.toLong.asInstanceOf[A])
    case (Schema.SDouble, JNum(n)) => Right(n.asInstanceOf[A])
    case (Schema.SBool, JBool(b)) => Right(b.asInstanceOf[A])
    case (Schema.SString, JStr(x)) => Right(x.asInstanceOf[A])
    case (Schema.SOption(of), JNull) => Right(None.asInstanceOf[A])
    case (Schema.SOption(of), v) =>
      decode(of().asInstanceOf[Schema[Any]])(v).map(Some(_).asInstanceOf[A])
    case (Schema.SList(of), JArr(vs)) =>
      // A truncated document leaves an "unclosed" marker where its
      // last element would be, and a damaged one leaves a JErr in
      // place of a value. Failing the whole list on either would
      // throw away the elements that DID arrive — which is the exact
      // opposite of why this stack is total. So error elements are
      // skipped here; they remain visible in the projection
      // (Json.parse) and in the tree (Cst.errors) for anyone who
      // wants to know that the document was damaged.
      vs.filterNot(_.isInstanceOf[JErr])
        .foldLeft(Right(List.empty[Any]): Either[String, List[Any]]) { (acc, v) =>
          acc.flatMap(xs => decode(of().asInstanceOf[Schema[Any]])(v).map(xs :+ _))
        }.map(_.asInstanceOf[A])
    case (p: Schema.SProduct[A], JObj(fs)) =>
      val m = fs.toMap
      p.fields.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, f) =>
        acc.flatMap { xs =>
          (m.get(f._1), f._2()) match
            case (None, _: Schema.SOption[?]) => Right(xs :+ None)   // absent optional
            // a damaged optional value is the same as an absent one
            case (Some(JErr(_)), _: Schema.SOption[?]) => Right(xs :+ None)
            case (found, sc) => found.toRight(s"missing field '${f._1}' in ${p.name}")
              .flatMap(decode(sc.asInstanceOf[Schema[Any]])).map(xs :+ _)
        }
      }.map(p.make)
    case (su: Schema.SSum[A], JObj(Vector((name, v)))) =>
      su.cases.find(_._1 == name)
        .toRight(s"unknown case '$name' of ${su.name}")
        .flatMap((_, sc) => decode(sc().asInstanceOf[Schema[Any]])(v).map(_.asInstanceOf[A]))
    case (_, JErr(m)) => Left(m)
    case (want, got) => Left(s"expected ${want.getClass.getSimpleName}, got $got")

  /** text to value in one move, through the total pipeline */
  def read[A](input: String)(using s: Schema[A]): Either[String, A] =
    decode(s)(parse(input))

  /** value to text in one move */
  def write[A](a: A)(using s: Schema[A]): String = encode(s)(a)
}
