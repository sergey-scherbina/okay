package okay2.codec

import scala.language.experimental.macros
import scala.language.implicitConversions
import okay2.{Cont, reset, />}
import okay2.lex.{Json => JsonLex}
import okay2.lex.Json.K
import okay2.parse.{Cst, JsonParse, Parse}

/** a JSON value; damage the lossless parser kept is `JErr`, in place */
sealed trait Json

/**
 * JSON over a `Schema` (okay-codec's Json.scala): the value, its
 * printer, two parsers (the fast strict VALUE parser, `JsonValue`, and
 * the lossless CST under it for everything the fast one is not sure
 * of), RFC 7386 merge patch, and encode/decode by folding a schema.
 * Every recursive walk here is native to `Codecs.NativeThreshold` open
 * containers and a `Cont.defer` trampoline past it.
 */
object Json {
  case object JNull extends Json
  final case class JBool(b: Boolean) extends Json
  final case class JNum(n: Double) extends Json
  final case class JStr(s: String) extends Json
  final case class JArr(vs: Vector[Json]) extends Json
  final case class JObj(fs: Vector[(String, Json)]) extends Json
  final case class JErr(message: String) extends Json

  /**
   * Opt-in literal conversions: `import Json.literals._` and
   * `JObj(Vector("a" -> 1))` reads as written. A String converts only
   * as a LITERAL (a macro reads the tree); a String VALUE is refused,
   * because it is as likely to be a serialized document, which
   * `Json.parse` reads and a silent `JStr` would double-encode.
   */
  object literals {
    implicit def fromString(s: String): Json = macro SchemaMacro.literal
    // EXACT types, by `=:=`: a view `Double => Json` applies to a Long
    // too in Scala 2 (numeric widening makes it applicable), and
    // 9007199254740993L would become a rounded JNum without a word —
    // measured (TestJsonLiterals, "Long does not convert"). Same for a
    // Char reaching `Int => Json`.
    implicit def fromInt[I](i: I)(implicit exact: I =:= Int): Json = JNum(exact(i).toDouble)
    implicit def fromDouble[D](d: D)(implicit exact: D =:= Double): Json = JNum(exact(d))
    implicit def fromBoolean(b: Boolean): Json = JBool(b)
    implicit final class JsonEq(private val j: Json) extends AnyVal {
      def ===(that: Json): Boolean = j == that
    }
  }

  // ---- the lossless layer ----

  /** the lossless CST: every token, trivia and damage included */
  def cst(s: String): Cst[K] = Parse.full(JsonLex.scan, JsonParse.instrs)(s).tree

  /** the CST back to its text, byte for byte */
  def render(c: Cst[K]): String = Cst.lexemes(c)

  /** compact JSON */
  def print(j: Json): String = {
    val sb = new StringBuilder
    printInto(j, sb, 0)
    sb.toString
  }

  private def printInto(j: Json, sb: StringBuilder, open: Int): Unit =
    if (open >= Codecs.NativeThreshold) reset(printIntoC[Unit](j, sb, open))
    else printIntoNative(j, sb, open)

  private def printIntoNative(j: Json, sb: StringBuilder, open: Int): Unit = j match {
    case JArr(vs) =>
      sb.append('[')
      var first = true
      vs.foreach { v =>
        if (!first) sb.append(',')
        first = false
        printInto(v, sb, open + 1)
      }
      sb.append(']'): Unit
    case JObj(fs) =>
      sb.append('{')
      var first = true
      fs.foreach { case (k, v) =>
        if (!first) sb.append(',')
        first = false
        sb.append('"').append(escape(k)).append("\":")
        printInto(v, sb, open + 1)
      }
      sb.append('}'): Unit
    case leaf => printLeaf(leaf, sb)
  }

  private def printLeaf(j: Json, sb: StringBuilder): Unit = j match {
    case JNull => sb.append("null"): Unit
    case JBool(b) => sb.append(b): Unit
    case JNum(n) =>
      sb.append(if (n == n.floor && n.abs < 1e15) n.toLong.toString else n.toString): Unit
    case JStr(s) => sb.append('"').append(escape(s)).append('"'): Unit
    case JErr(m) => sb.append("\"<error: ").append(escape(m)).append(">\""): Unit
    case JArr(_) | JObj(_) => () // unreachable: the caller handles containers
  }

  private def printIntoC[R](j: Json, sb: StringBuilder, open: Int): Unit /> R = j match {
    case JArr(vs) =>
      sb.append('[')
      def loop(rest: Vector[Json], first: Boolean): Unit /> R =
        if (rest.isEmpty) { sb.append(']'); Cont.Pure(()) }
        else {
          if (!first) sb.append(',')
          Cont.defer(() => printIntoC[R](rest.head, sb, open + 1))((_: Unit) => loop(rest.tail, false))
        }
      loop(vs, true)
    case JObj(fs) =>
      sb.append('{')
      def loop(rest: Vector[(String, Json)], first: Boolean): Unit /> R =
        if (rest.isEmpty) { sb.append('}'); Cont.Pure(()) }
        else {
          val (k, v) = rest.head
          if (!first) sb.append(',')
          sb.append('"').append(escape(k)).append("\":")
          Cont.defer(() => printIntoC[R](v, sb, open + 1))((_: Unit) => loop(rest.tail, false))
        }
      loop(fs, true)
    case leaf =>
      printLeaf(leaf, sb)
      Cont.Pure(())
  }

  /** the value: the strict fast parser where it is sure, the lossless
   * projection (damage as `JErr`, in place) where it is not */
  def parse(s: String): Json = JsonValue.parse(s).getOrElse(lossless(s))

  /** the value through the CST, whatever the input */
  def lossless(s: String): Json = value(cst(s))

  /** RFC 7396 JSON Merge Patch: an object patch merges key by key, a
   * `null` deletes, anything else replaces */
  def mergePatch(target: Json, patch: Json): Json = mergePatchAt(target, patch, 0)

  private def mergePatchAt(target: Json, patch: Json, open: Int): Json =
    if (open >= Codecs.NativeThreshold) reset(mergePatchC[Json](target, patch, open))
    else mergePatchNative(target, patch, open)

  private def mergePatchNative(target: Json, patch: Json, open: Int): Json = patch match {
    case JObj(patchFields) =>
      val base = target match {
        case JObj(fs) => fs
        case _ => Vector.empty
      }
      val merged = patchFields.foldLeft(base) { (acc, kv) =>
        val (k, v) = kv
        val without = acc.filterNot(_._1 == k)
        v match {
          case JNull => without
          case _ =>
            val orig = acc.find(_._1 == k).map(_._2).getOrElse(JNull)
            without :+ (k -> mergePatchAt(orig, v, open + 1))
        }
      }
      JObj(merged)
    case other => other
  }

  private def mergePatchC[R](target: Json, patch: Json, open: Int): Json /> R = patch match {
    case JObj(patchFields) =>
      val base = target match {
        case JObj(fs) => fs
        case _ => Vector.empty
      }
      def loop(rest: Vector[(String, Json)], acc: Vector[(String, Json)]): Vector[(String, Json)] /> R =
        if (rest.isEmpty) Cont.Pure(acc)
        else {
          val (k, v) = rest.head
          val without = acc.filterNot(_._1 == k)
          v match {
            case JNull => loop(rest.tail, without)
            case _ =>
              val orig = acc.find(_._1 == k).map(_._2).getOrElse(JNull)
              Cont.defer(() => mergePatchC[R](orig, v, open + 1))((merged: Json) => loop(rest.tail, without :+ (k -> merged)))
          }
        }
      loop(patchFields, base).flatMap(merged => Cont.Pure[Json, R](JObj(merged)))
    case other => Cont.Pure(other)
  }

  /** the CST's projection: the first value in it, damage as `JErr` */
  def value(c: Cst[K]): Json =
    values(c).headOption.getOrElse(JErr("empty input"))

  private[codec] def hex4(s: String): Option[Char] =
    if (s.length == 4 && s.forall(c => c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')))
      scala.util.Try(Integer.parseInt(s, 16).toChar).toOption
    else None

  private def unquote(lexeme: String): String = {
    val from = if (lexeme.startsWith("\"")) 1 else 0
    val to = if (lexeme.length > from && lexeme.endsWith("\"")) lexeme.length - 1 else lexeme.length
    if (lexeme.indexOf('\\', from) < 0) lexeme.substring(from, to)
    else unescape(lexeme.substring(from, to))
  }

  /** `\n` `\t` `\r` are the control characters, `\uXXXX` one UTF-16
   * code unit, any OTHER escaped character is itself — the reading
   * `JsonValue` and `JsonStrict` share */
  private def unescape(inner: String): String = {
    val b = new StringBuilder(inner.length)
    var i = 0
    while (i < inner.length) {
      val c = inner.charAt(i)
      if (c == '\\' && i + 1 < inner.length) {
        inner.charAt(i + 1) match {
          case 'n' => b.append('\n'); i += 2
          case 't' => b.append('\t'); i += 2
          case 'r' => b.append('\r'); i += 2
          case 'u' if i + 6 <= inner.length =>
            hex4(inner.substring(i + 2, i + 6)) match {
              case Some(ch) => b.append(ch); i += 6
              case None => b.append('u'); i += 2
            }
          case x => b.append(x); i += 2
        }
      } else { b.append(c); i += 1 }
    }
    b.toString
  }

  private type Out = scala.collection.mutable.Builder[Json, Vector[Json]]

  private def values(c: Cst[K]): Vector[Json] = {
    val out = Vector.newBuilder[Json]
    into(c, out, 0)
    out.result()
  }

  private def into(c: Cst[K], out: Out, open: Int): Unit =
    if (open >= Codecs.NativeThreshold) reset(intoC[Unit](c, out, open))
    else intoNative(c, out, open)

  private def leaf(t: okay2.lex.Token[K], out: Out): Unit = t.kind match {
    case K.Str => out += JStr(unquote(t.lexeme)): Unit
    case K.Num =>
      t.lexeme.toDoubleOption match {
        case Some(d) => out += JNum(d): Unit
        case None => out += JErr(s"malformed number '${t.lexeme}'"): Unit
      }
    case K.Bool => out += JBool(t.lexeme == "true"): Unit
    case K.Null => out += JNull: Unit
    case _ => ()
  }

  private def err(t: Option[okay2.lex.Token[K]], m: String): Json =
    JErr(m + t.fold("")(x => s" at '${x.lexeme}'"))

  private def intoNative(c: Cst[K], out: Out, open: Int): Unit = c match {
    case Cst.Node("object", kids) =>
      out += JObj(pairs(kids, open + 1)): Unit
    case Cst.Node("array", kids) =>
      val vs = Vector.newBuilder[Json]
      kids.foreach(into(_, vs, open + 1))
      out += JArr(vs.result()): Unit
    case Cst.Node(_, kids) => kids.foreach(into(_, out, open))
    case Cst.Leaf(t) => leaf(t, out)
    case Cst.Err(t, m) => out += err(t, m): Unit
  }

  /** an object's children flattened, then paired key-value; a damaged
   * key is `<message>`, so the value is not lost with it */
  private def paired(flat: Vector[Json]): Vector[(String, Json)] = {
    val out = Vector.newBuilder[(String, Json)]
    var i = 0
    while (i + 1 < flat.length) {
      flat(i) match {
        case JStr(k) => out += ((k, flat(i + 1)))
        case JErr(m) => out += ((s"<$m>", flat(i + 1)))
        case _ => ()
      }
      i += 2
    }
    out.result()
  }

  private def pairs(kids: Vector[Cst[K]], open: Int): Vector[(String, Json)] =
    if (open >= Codecs.NativeThreshold) reset(pairsC[Vector[(String, Json)]](kids, open))
    else {
      val vs = Vector.newBuilder[Json]
      kids.foreach(into(_, vs, open))
      paired(vs.result())
    }

  private def intoC[R](c: Cst[K], out: Out, open: Int): Unit /> R = c match {
    case Cst.Node("object", kids) =>
      pairsC[R](kids, open + 1).flatMap { fs => out += JObj(fs); Cont.Pure[Unit, R](()) }
    case Cst.Node("array", kids) =>
      val vs = Vector.newBuilder[Json]
      def loop(rest: Vector[Cst[K]]): Unit /> R =
        if (rest.isEmpty) Cont.Pure(())
        else Cont.defer(() => intoC[R](rest.head, vs, open + 1))((_: Unit) => loop(rest.tail))
      loop(kids).flatMap { _ => out += JArr(vs.result()); Cont.Pure[Unit, R](()) }
    case Cst.Node(_, kids) =>
      def loop(rest: Vector[Cst[K]]): Unit /> R =
        if (rest.isEmpty) Cont.Pure(())
        else Cont.defer(() => intoC[R](rest.head, out, open))((_: Unit) => loop(rest.tail))
      loop(kids)
    case Cst.Leaf(t) =>
      leaf(t, out)
      Cont.Pure(())
    case Cst.Err(t, m) =>
      out += err(t, m)
      Cont.Pure(())
  }

  private def pairsC[R](kids: Vector[Cst[K]], open: Int): Vector[(String, Json)] /> R = {
    val vs = Vector.newBuilder[Json]
    def loop(rest: Vector[Cst[K]]): Unit /> R =
      if (rest.isEmpty) Cont.Pure(())
      else Cont.defer(() => intoC[R](rest.head, vs, open))((_: Unit) => loop(rest.tail))
    loop(kids).flatMap(_ => Cont.Pure[Vector[(String, Json)], R](paired(vs.result())))
  }

  /** the five characters a JSON string must escape here; the rest
   * passes through */
  def escape(s: String): String = {
    var i = 0
    while (i < s.length && !needsEscape(s.charAt(i))) i += 1
    if (i == s.length) s
    else {
      val b = new java.lang.StringBuilder(s.length + 8)
      b.append(s, 0, i)
      while (i < s.length) {
        s.charAt(i) match {
          case '"' => b.append("\\\"")
          case '\\' => b.append("\\\\")
          case '\n' => b.append("\\n")
          case '\t' => b.append("\\t")
          case '\r' => b.append("\\r")
          case c => b.append(c)
        }
        i += 1
      }
      b.toString
    }
  }

  @inline private def needsEscape(c: Char): Boolean =
    c == '"' || c == '\\' || c == '\n' || c == '\t' || c == '\r'

  // ---- encode: the schema folded once into a Step, walked per value ----

  def encode[A](s: Schema[A])(a: A): String = {
    val sb = new StringBuilder
    Schema.Step.walk(encoder(s), sb, a)
    sb.toString
  }

  private type Enc[A] = Schema.Step[StringBuilder, A, Unit]
  private val encoder = new Schema.Folded[Enc](new Schema.Algebra[Enc] {
    import Schema.Step
    def int = Step.leaf((sb: StringBuilder, a: Int) => sb.append(a.toString): Unit)
    def long = Step.leaf((sb: StringBuilder, a: Long) => sb.append(a.toString): Unit)
    def double = Step.leaf((sb: StringBuilder, a: Double) => sb.append(a.toString): Unit)
    def bool = Step.leaf((sb: StringBuilder, a: Boolean) => sb.append(a.toString): Unit)
    def string = Step.leaf((sb: StringBuilder, a: String) => sb.append('"').append(escape(a)).append('"'): Unit)
    def char = Step.leaf((sb: StringBuilder, a: Char) => sb.append('"').append(escape(a.toString)).append('"'): Unit)
    def bytes = Step.leaf((sb: StringBuilder, a: Array[Byte]) => sb.append('"').append(Base64.encode(a)).append('"'): Unit)
    def bigInt = Step.leaf((sb: StringBuilder, a: BigInt) => sb.append('"').append(a.toString).append('"'): Unit)
    def option[A](o: Schema.SOption[A], of: () => Enc[A]) = Step.option((sb: StringBuilder) => sb.append("null"): Unit, of)
    private def seq[C, A](items: C => Iterable[A], of: () => Enc[A]): Enc[C] = Step.elems[StringBuilder, C, A, Unit, Unit](
      (sb, _) => sb.append('['): Unit, items, of,
      (sb, i) => if (i > 0) sb.append(','): Unit,
      (_, _) => (), (sb, _, _) => sb.append(']'): Unit)
    def list[A](l: Schema.SList[A], of: () => Enc[A]) = seq[List[A], A](identity, of)
    def vector[A](v: Schema.SVector[A], of: () => Enc[A]) = seq[Vector[A], A](identity, of)
    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Enc, Any])]) =
      Step.fields[StringBuilder, A, Unit, Unit, Enc](
        (sb, _) => sb.append('{'): Unit, p.parts, fields,
        (sb, i, n) => { if (i > 0) sb.append(','); sb.append('"').append(n).append("\":"): Unit },
        (_, _) => (), (sb, _, _) => sb.append('}'): Unit)
    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Enc, A])]) =
      Step.one[StringBuilder, A, Unit, Unit, Enc](
        (sb, _) => sb.append('{'): Unit, su.caseOf, cases,
        (sb, n) => sb.append('"').append(n).append("\":"): Unit,
        (sb, _, _, _) => sb.append('}'): Unit)
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Enc[B]) = Step.via(iso.from, under)
    def ref[A](name: String) =
      throw new IllegalStateException(s"a lazy carrier never meets a back edge, got one at $name")
  })

  // ---- decode: total over the projection; errors are values ----

  def decode[A](s: Schema[A])(j: Json): Either[String, A] = decodeAt(s, j, 0)

  private def decodeAt[A](s: Schema[A], j: Json, depth: Int): Either[String, A] =
    if (depth >= Codecs.NativeThreshold) reset(decodeC[A, Either[String, A]](s, j))
    else decodeNative(s, j, depth)

  private[codec] def kindOf(s: Schema[_]): String = s match {
    case p: Schema.SProduct[_] => p.name
    case su: Schema.SSum[_] => su.name
    case _: Schema.SOption[_] => "SOption"
    case _: Schema.SList[_] => "SList"
    case _: Schema.SVector[_] => "SVector"
    case _: Schema.SIso[_, _] => "SIso"
    case leaf => leaf.toString // the case objects' own names
  }

  private def mismatch[A](want: Schema[_], got: Json): Either[String, A] = got match {
    case JErr(m) => Left(m)
    case _ => Left(s"expected ${kindOf(want)}, got $got")
  }

  /** a product's field absent from the input: its declared default,
   * then None if it is optional, then the refusal by name */
  private[codec] def absent[A](p: Schema.SProduct[A], i: Int): Either[String, Any] = {
    val (name, sc) = p.fields(i)
    p.defaults.lift(i).flatten match {
      case Some(d) => Right(d())
      case None => sc() match {
        case _: Schema.SOption[_] => Right(None)
        case _ => Left(s"missing field '$name' in ${p.name}")
      }
    }
  }

  private def isOption(s: Schema[_]): Boolean = s.isInstanceOf[Schema.SOption[_]]

  private type Dec[X] = Either[String, X]

  private def decodeNative[A](s: Schema[A], j: Json, depth: Int): Either[String, A] = s.visit(new Schema.Visit[Dec] {
    def int = j match { case JNum(n) => Numbers.int(n); case g => mismatch(s, g) }
    def long = j match { case JNum(n) => Numbers.long(n); case g => mismatch(s, g) }
    def double = j match { case JNum(n) => Right(n); case g => mismatch(s, g) }
    def bool = j match { case JBool(b) => Right(b); case g => mismatch(s, g) }
    def string = j match { case JStr(x) => Right(x); case g => mismatch(s, g) }
    def char = j match {
      case JStr(x) if x.length == 1 => Right(x.head)
      case JStr(x) => Left(s"expected one character, got ${x.length}")
      case g => mismatch(s, g)
    }
    def bytes = j match { case JStr(x) => Base64.decode(x); case g => mismatch(s, g) }
    def bigInt = j match {
      case JStr(x) => BigInts.fromDigits(x)
      case JNum(n) => BigInts.fromNumber(n)
      case g => mismatch(s, g)
    }
    def option[B](o: Schema.SOption[B]) = j match {
      case JNull => Right(None)
      case v => decodeAt(o.of(), v, depth + 1).map(Some(_))
    }
    def list[B](l: Schema.SList[B]) = j match {
      case JArr(vs) => elems(l.of(), vs).map(_.toList)
      case g => mismatch(s, g)
    }
    def vector[B](v: Schema.SVector[B]) = j match {
      case JArr(vs) => elems(v.of(), vs)
      case g => mismatch(s, g)
    }
    private def elems[B](of: Schema[B], vs: Vector[Json]): Either[String, Vector[B]] =
      vs.filterNot(_.isInstanceOf[JErr]).foldLeft(Right(Vector.empty): Either[String, Vector[B]]) { (acc, v) =>
        acc.flatMap(xs => decodeAt(of, v, depth + 1).map(xs :+ _))
      }
    def product[B](p: Schema.SProduct[B]) = j match {
      case JObj(fs) =>
        val m = fs.toMap
        p.fields.indices.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, i) =>
          val (name, sc) = p.fields(i)
          acc.flatMap { xs =>
            m.get(name) match {
              case None => absent(p, i).map(xs :+ _)
              case Some(JErr(_)) if isOption(sc()) => absent(p, i).map(xs :+ _)
              case Some(v) => decodeAt(sc(), v, depth + 1).map(xs :+ _)
            }
          }
        }.map(p.make)
      case g => mismatch(s, g)
    }
    def sum[B](su: Schema.SSum[B]) = j match {
      case JObj(Vector((name, v))) =>
        su.cases.find(_._1 == name)
          .toRight(s"unknown case '$name' of ${su.name}")
          .flatMap { case (_, sc) => decodeAt(sc(), v, depth + 1) }
      case g => mismatch(s, g)
    }
    def iso[B, C](i: Schema.SIso[B, C]) = decodeAt(i.under(), j, depth + 1).flatMap(i.to)
  })

  private type DecC[R] = { type L[X] = Either[String, X] /> R }

  /** the trampoline: decodeNative case for case, each descent a
   * `Cont.defer` in `reset`'s loop instead of a JVM frame */
  private def decodeC[A, R](s: Schema[A], j: Json): Either[String, A] /> R = s.visit(new Schema.Visit[DecC[R]#L] {
    private def now[X](e: Either[String, X]): Either[String, X] /> R = Cont.Pure(e)
    def int = now(j match { case JNum(n) => Numbers.int(n); case g => mismatch(s, g) })
    def long = now(j match { case JNum(n) => Numbers.long(n); case g => mismatch(s, g) })
    def double = now(j match { case JNum(n) => Right(n); case g => mismatch(s, g) })
    def bool = now(j match { case JBool(b) => Right(b); case g => mismatch(s, g) })
    def string = now(j match { case JStr(x) => Right(x); case g => mismatch(s, g) })
    def char = now(j match {
      case JStr(x) if x.length == 1 => Right(x.head)
      case JStr(x) => Left(s"expected one character, got ${x.length}")
      case g => mismatch(s, g)
    })
    def bytes = now(j match { case JStr(x) => Base64.decode(x); case g => mismatch(s, g) })
    def bigInt = now(j match {
      case JStr(x) => BigInts.fromDigits(x)
      case JNum(n) => BigInts.fromNumber(n)
      case g => mismatch(s, g)
    })
    def option[B](o: Schema.SOption[B]) = j match {
      case JNull => now(Right(None))
      case v => Cont.defer(() => decodeC[B, R](o.of(), v))((r: Either[String, B]) => now(r.map(Some(_))))
    }
    private def elems[B](of: Schema[B], vs: Vector[Json]): Either[String, Vector[B]] /> R = {
      def loop(rest: List[Json], acc: Vector[B]): Either[String, Vector[B]] /> R = rest match {
        case Nil => now(Right(acc))
        case v :: more => Cont.defer(() => decodeC[B, R](of, v)) {
          case Left(e) => now(Left(e))
          case Right(x) => loop(more, acc :+ x)
        }
      }
      loop(vs.filterNot(_.isInstanceOf[JErr]).toList, Vector.empty)
    }
    def list[B](l: Schema.SList[B]) = j match {
      case JArr(vs) => elems(l.of(), vs).map(_.map(_.toList))
      case g => now(mismatch(s, g))
    }
    def vector[B](v: Schema.SVector[B]) = j match {
      case JArr(vs) => elems(v.of(), vs)
      case g => now(mismatch(s, g))
    }
    def product[B](p: Schema.SProduct[B]) = j match {
      case JObj(fs) =>
        val m = fs.toMap
        def field[X](sc: Schema[X], v: Json): Either[String, Any] /> R =
          decodeC[X, R](sc, v).map(e => e: Either[String, Any])
        def loop(i: Int, acc: Vector[Any]): Either[String, Vector[Any]] /> R =
          if (i >= p.fields.length) now(Right(acc))
          else {
            val (name, sc) = p.fields(i)
            def fallback = absent(p, i) match {
              case Left(e) => now[Vector[Any]](Left(e))
              case Right(v) => loop(i + 1, acc :+ v)
            }
            m.get(name) match {
              case None => fallback
              case Some(JErr(_)) if isOption(sc()) => fallback
              case Some(v) => Cont.defer(() => field(sc(), v)) {
                case Left(e) => now[Vector[Any]](Left(e))
                case Right(x) => loop(i + 1, acc :+ x)
              }
            }
          }
        loop(0, Vector.empty).map(_.map(p.make))
      case g => now(mismatch(s, g))
    }
    def sum[B](su: Schema.SSum[B]) = j match {
      case JObj(Vector((name, v))) =>
        su.cases.find(_._1 == name) match {
          case None => now(Left(s"unknown case '$name' of ${su.name}"))
          case Some((_, sc)) => caseC(sc(), v)
        }
      case g => now(mismatch(s, g))
    }
    private def caseC[B, X <: B](sc: Schema[X], v: Json): Either[String, B] /> R =
      decodeC[X, R](sc, v).map(e => e: Either[String, B])
    def iso[B, C](i: Schema.SIso[B, C]) =
      Cont.defer(() => decodeC[C, R](i.under(), j))((r: Either[String, C]) => now(r.flatMap(i.to)))
  })

  /** parse, then decode */
  def read[A](input: String)(implicit s: Schema[A]): Either[String, A] =
    decode(s)(parse(input))

  /** characters straight into the schema, no tree: `Left` on anything
   * not complete and well-formed (see `JsonStrict`) */
  def readStrict[A](input: String)(implicit s: Schema[A]): Either[String, A] =
    JsonStrict.read(input)

  def write[A](a: A)(implicit s: Schema[A]): String = encode(s)(a)
}
