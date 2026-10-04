package okay2.codec

import okay2.{Cont, />}
import scala.collection.mutable
import scala.annotation.tailrec

/** an EDN value (okay-codec's `Edn` enum) */
sealed trait Edn

/**
 * EDN — Clojure's extensible data notation (https://github.com/edn-format/edn)
 * — as a codec beside JSON and CBOR (okay-codec's Edn.scala).
 *
 * EDN says what JSON cannot: a KEYWORD is not a string, a SET is not a
 * vector, a 64-bit integer is exact (and `123N` is a big one), a
 * character is not a one-letter string, and a value can carry a TAG.
 * So a `Schema` maps onto it without the JSON compromises:
 *
 *  - a product is a map with keyword keys — `{:name "okay" :age 3}`;
 *  - a sum is a TAGGED value, the tag namespaced by the sum's name —
 *    `#Shape/Circle {:r 1.0}` (EDN reserves un-namespaced tags);
 *  - `SLong` is exact to 64 bits, `SBigInt` is `123N`, `SChar` is `\c`;
 *  - `SList` is a list `(…)`, `SVector` a vector `[…]` (either is read
 *    for either); bytes are `#okay/bytes "base64"`; `None` is `nil`.
 *
 * Text is read and printed with an EXPLICIT stack, so nesting depth is
 * bounded by memory; a `Schema` is encoded by the `Step` walk and
 * decoded natively up to `Codecs.NativeThreshold`, then on the `Cont`
 * road.
 */
object Edn {
  case object ENil extends Edn
  final case class EBool(b: Boolean) extends Edn
  final case class ELong(n: Long) extends Edn
  /** an integer beyond 64 bits, or written with `N` */
  final case class EBig(n: BigInt) extends Edn
  final case class EDouble(d: Double) extends Edn
  /** a decimal written with `M` */
  final case class EDec(d: BigDecimal) extends Edn
  final case class EStr(s: String) extends Edn
  final case class EChar(c: Char) extends Edn
  final case class EKeyword(ns: Option[String], name: String) extends Edn
  final case class ESymbol(ns: Option[String], name: String) extends Edn
  final case class EList(items: Vector[Edn]) extends Edn
  final case class EVector(items: Vector[Edn]) extends Edn
  final case class EMap(entries: Vector[(Edn, Edn)]) extends Edn
  final case class ESet(items: Vector[Edn]) extends Edn
  final case class ETagged(ns: Option[String], name: String, value: Edn) extends Edn

  // ================================================================ read

  /** EDN text as one value; `Left` names the first thing wrong, where */
  def parse(text: String): Either[String, Edn] = {
    val r = new Reader(text)
    r.value() match {
      case Left(e) => Left(e)
      case Right(None) => Left("edn: empty input")
      case Right(Some(v)) =>
        r.skip()
        if (r.at < text.length) Left(s"edn: trailing input at ${r.at}: '${text.substring(r.at, math.min(text.length, r.at + 20))}'")
        else Right(v)
    }
  }

  /** every top-level value in the text, in order */
  def parseAll(text: String): Either[String, Vector[Edn]] = {
    val r = new Reader(text)
    val out = Vector.newBuilder[Edn]
    var err: Option[String] = None
    var go = true
    while (go && err.isEmpty) {
      r.value() match {
        case Left(e) => err = Some(e)
        case Right(None) => go = false
        case Right(Some(v)) => out += v
      }
    }
    err.toLeft(out.result())
  }

  /** a collection being filled; `Tagged` and `Discard` take ONE value */
  private sealed trait Frame
  private object Frame {
    final case class InList(items: mutable.ArrayBuffer[Edn]) extends Frame
    final case class InVector(items: mutable.ArrayBuffer[Edn]) extends Frame
    final case class InMap(items: mutable.ArrayBuffer[Edn]) extends Frame
    final case class InSet(items: mutable.ArrayBuffer[Edn]) extends Frame
    final case class Tagged(ns: Option[String], name: String) extends Frame
    case object Discard extends Frame
  }

  private val IntPattern = "([+-]?[0-9]+)(N?)".r
  private val FloatPattern = "([+-]?[0-9]+(?:\\.[0-9]*)?(?:[eE][+-]?[0-9]+)?)(M?)".r

  private def split(s: String): (Option[String], String) = {
    val slash = s.indexOf('/')
    if (slash > 0 && slash < s.length - 1) (Some(s.substring(0, slash)), s.substring(slash + 1))
    else (None, s)
  }

  private final class Reader(text: String) {
    var at = 0

    private def isWs(c: Char) = c.isWhitespace || c == ','
    private def isDelim(c: Char) = isWs(c) || "()[]{}\";".indexOf(c.toInt) >= 0
    private def isSymbolChar(c: Char) =
      c.isLetterOrDigit || ".*+!-_?$%&=<>/:#'".indexOf(c.toInt) >= 0

    def skip(): Unit = {
      var go = true
      while (go && at < text.length) {
        val c = text.charAt(at)
        if (isWs(c)) at += 1
        else if (c == ';') {
          while (at < text.length && text.charAt(at) != '\n') at += 1
        } else go = false
      }
    }

    private def fail(msg: String): Left[String, Nothing] = Left(s"edn: $msg at $at")

    /** the next complete value, or None at the end of input. Iterative:
     * a stack of open collections, so nesting depth costs heap */
    def value(): Either[String, Option[Edn]] = {
      val stack = mutable.ArrayBuffer.empty[Frame]
      var result: Option[Edn] = None
      var err: Option[String] = None

      /* a finished value goes to the innermost open frame, or out */
      def complete(v0: Edn): Unit = {
        var v: Option[Edn] = Some(v0)
        while (v.isDefined && err.isEmpty) {
          if (stack.isEmpty) { result = v; v = None }
          else stack.last match {
            case Frame.InList(b) => b += v.get; v = None
            case Frame.InVector(b) => b += v.get; v = None
            case Frame.InMap(b) => b += v.get; v = None
            case Frame.InSet(b) => b += v.get; v = None
            case Frame.Tagged(ns, name) =>
              stack.remove(stack.length - 1): Unit
              v = Some(ETagged(ns, name, v.get))
            case Frame.Discard =>
              stack.remove(stack.length - 1): Unit
              v = None
          }
        }
      }

      def close(c: Char): Unit =
        if (stack.isEmpty) err = Some(s"edn: unexpected '$c' at $at")
        else {
          val top = stack.remove(stack.length - 1)
          (top, c) match {
            case (Frame.InList(b), ')') => complete(EList(b.toVector))
            case (Frame.InVector(b), ']') => complete(EVector(b.toVector))
            case (Frame.InSet(b), '}') => complete(ESet(b.toVector))
            case (Frame.InMap(b), '}') =>
              if (b.length % 2 != 0) err = Some(s"edn: a map with an odd number of forms, closed at $at")
              else complete(EMap(b.grouped(2).map(p => (p(0), p(1))).toVector))
            case _ => err = Some(s"edn: '$c' closes nothing open here (at $at)")
          }
        }

      while (result.isEmpty && err.isEmpty && (stack.nonEmpty || { skip(); at < text.length })) {
        skip()
        if (at >= text.length) err = Some(s"edn: input ended inside an open form")
        else {
          val c = text.charAt(at)
          c match {
            case '(' => at += 1; stack += Frame.InList(mutable.ArrayBuffer.empty)
            case '[' => at += 1; stack += Frame.InVector(mutable.ArrayBuffer.empty)
            case '{' => at += 1; stack += Frame.InMap(mutable.ArrayBuffer.empty)
            case ')' | ']' | '}' => at += 1; close(c)
            case '"' => string().fold(e => err = Some(e), complete)
            case '\\' => char().fold(e => err = Some(e), complete)
            case '#' => dispatch(stack).fold(e => err = Some(e), _.foreach(complete))
            case _ => atom().fold(e => err = Some(e), complete)
          }
        }
      }
      err.toLeft(result)
    }

    /** after '#': a set, a discard, a symbolic value, or a tag */
    private def dispatch(stack: mutable.ArrayBuffer[Frame]): Either[String, Option[Edn]] = {
      at += 1
      if (at >= text.length) fail("'#' at the end of input")
      else text.charAt(at) match {
        case '{' => at += 1; stack += Frame.InSet(mutable.ArrayBuffer.empty); Right(None)
        case '_' => at += 1; stack += Frame.Discard; Right(None)
        case '#' =>
          at += 1
          val start = at
          while (at < text.length && !isDelim(text.charAt(at))) at += 1
          text.substring(start, at) match {
            case "Inf" => Right(Some(EDouble(Double.PositiveInfinity)))
            case "-Inf" => Right(Some(EDouble(Double.NegativeInfinity)))
            case "NaN" => Right(Some(EDouble(Double.NaN)))
            case other => fail(s"unknown symbolic value ##$other")
          }
        case c if c.isLetter =>
          val start = at
          while (at < text.length && !isDelim(text.charAt(at))) at += 1
          val (ns, name) = split(text.substring(start, at))
          stack += Frame.Tagged(ns, name)
          Right(None)
        case c => fail(s"'#$c' is not EDN")
      }
    }

    private def string(): Either[String, Edn] = {
      val sb = new StringBuilder
      at += 1
      var err: Option[String] = None
      var done = false
      while (!done && err.isEmpty) {
        if (at >= text.length) err = Some(s"edn: an unterminated string")
        else {
          val c = text.charAt(at)
          at += 1
          if (c == '"') done = true
          else if (c == '\\') {
            if (at >= text.length) err = Some("edn: an unterminated escape")
            else {
              val e = text.charAt(at)
              at += 1
              e match {
                case 't' => sb.append('\t')
                case 'r' => sb.append('\r')
                case 'n' => sb.append('\n')
                case 'b' => sb.append('\b')
                case 'f' => sb.append('\f')
                case '\\' => sb.append('\\')
                case '"' => sb.append('"')
                case 'u' if at + 4 <= text.length =>
                  scala.util.Try(Integer.parseInt(text.substring(at, at + 4), 16)).toOption match {
                    case Some(code) => sb.append(code.toChar); at += 4
                    case None => err = Some(s"edn: a bad \\u escape at $at")
                  }
                case other => err = Some(s"edn: unknown escape \\$other at $at")
              }
            }
          } else sb.append(c): Unit
        }
      }
      err.toLeft(EStr(sb.toString))
    }

    private def char(): Either[String, Edn] = {
      at += 1
      val start = at
      if (at >= text.length) fail("a '\\' at the end of input")
      else {
        at += 1
        while (at < text.length && !isDelim(text.charAt(at))) at += 1
        text.substring(start, at) match {
          case s if s.length == 1 => Right(EChar(s.head))
          case "newline" => Right(EChar('\n'))
          case "return" => Right(EChar('\r'))
          case "space" => Right(EChar(' '))
          case "tab" => Right(EChar('\t'))
          case "formfeed" => Right(EChar('\f'))
          case "backspace" => Right(EChar('\b'))
          case s if s.startsWith("u") && s.length == 5 =>
            scala.util.Try(Integer.parseInt(s.substring(1), 16).toChar).toOption
              .map(EChar(_)).toRight(s"edn: a bad character \\$s")
          case s => Left(s"edn: unknown character \\$s")
        }
      }
    }

    /** a number, keyword, symbol, nil, true or false */
    private def atom(): Either[String, Edn] = {
      val start = at
      while (at < text.length && !isDelim(text.charAt(at))) at += 1
      val tok = text.substring(start, at)
      if (tok.isEmpty) fail(s"unexpected '${text.charAt(at)}'")
      else tok match {
        case "nil" => Right(ENil)
        case "true" => Right(EBool(true))
        case "false" => Right(EBool(false))
        case IntPattern(digits, n) =>
          val big = BigInt(digits.stripPrefix("+"))
          if (n.nonEmpty || !big.isValidLong) Right(EBig(big)) else Right(ELong(big.toLong))
        case FloatPattern(digits, m) =>
          if (m.nonEmpty) Right(EDec(BigDecimal(digits))) else Right(EDouble(digits.toDouble))
        case k if k.startsWith(":") && k.length > 1 =>
          val (ns, name) = split(k.substring(1))
          Right(EKeyword(ns, name))
        case s if s.forall(isSymbolChar) && !s.head.isDigit =>
          val (ns, name) = split(s)
          Right(ESymbol(ns, name))
        case s => Left(s"edn: '$s' is not an EDN value (at $start)")
      }
    }
  }

  // =============================================================== print

  /** a value as EDN text; iterative, so depth costs heap */
  def show(e: Edn): String = {
    val sb = new StringBuilder
    var work: List[Either[String, Edn]] = List(Right(e))
    def seq(open: String, close: String, items: Vector[Edn]): Unit = {
      sb.append(open)
      var w: List[Either[String, Edn]] = Left(close) :: work
      var i = items.length - 1
      while (i >= 0) {
        w = Right(items(i)) :: w
        if (i > 0) w = Left(" ") :: w
        i -= 1
      }
      work = w
    }
    while (work.nonEmpty) {
      val next = work.head
      work = work.tail
      next match {
        case Left(s) => sb.append(s): Unit
        case Right(v) => v match {
          case ENil => sb.append("nil"): Unit
          case EBool(b) => sb.append(b): Unit
          case ELong(n) => sb.append(n): Unit
          case EBig(n) => sb.append(n).append('N'): Unit
          case EDouble(d) => sb.append(double(d)): Unit
          case EDec(d) => sb.append(d.bigDecimal.toPlainString).append('M'): Unit
          case EStr(s) => sb.append(string(s)): Unit
          case EChar(c) => sb.append(char(c)): Unit
          case EKeyword(ns, n) => sb.append(':').append(qualified(ns, n)): Unit
          case ESymbol(ns, n) => sb.append(qualified(ns, n)): Unit
          case EList(xs) => seq("(", ")", xs)
          case EVector(xs) => seq("[", "]", xs)
          case ESet(xs) => seq("#{", "}", xs)
          case EMap(kvs) => seq("{", "}", kvs.flatMap { case (k, x) => Vector(k, x) })
          case ETagged(ns, n, value) =>
            sb.append('#').append(qualified(ns, n)).append(' ')
            work = Right(value) :: work
        }
      }
    }
    sb.toString
  }

  private def qualified(ns: Option[String], name: String): String = ns.fold(name)(n => s"$n/$name")

  private def double(d: Double): String =
    if (d.isNaN) "##NaN"
    else if (d.isPosInfinity) "##Inf"
    else if (d.isNegInfinity) "##-Inf"
    else {
      val s = d.toString
      // EDN reads `1` as an integer: a double must look like one
      if (s.exists(c => c == '.' || c == 'E' || c == 'e')) s else s + ".0"
    }

  private def string(s: String): String = {
    val sb = new StringBuilder("\"")
    s.foreach {
      case '"' => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case '\b' => sb.append("\\b")
      case '\f' => sb.append("\\f")
      case c if c < ' ' => sb.append(f"\\u${c.toInt}%04x")
      case c => sb.append(c)
    }
    sb.append('"').toString
  }

  private def char(c: Char): String = c match {
    case '\n' => "\\newline"
    case '\r' => "\\return"
    case ' ' => "\\space"
    case '\t' => "\\tab"
    case '\f' => "\\formfeed"
    case '\b' => "\\backspace"
    case c if c < ' ' || c.isWhitespace || "()[]{}\";,".indexOf(c.toInt) >= 0 => f"\\u${c.toInt}%04x"
    case c => s"\\$c"
  }

  // ============================================================ encode

  /** a value as EDN text, through its Schema — stack-safe (the Step walk) */
  def write[A](a: A)(implicit s: Schema[A]): String = encode(s)(a)

  def encode[A](s: Schema[A])(a: A): String = {
    val sb = new StringBuilder
    Schema.Step.walk(encoder(s), sb, a)
    sb.toString
  }

  private type Enc[A] = Schema.Step[StringBuilder, A, Unit]
  private val encoder = new Schema.Folded[Enc](new Schema.Algebra[Enc] {
    import Schema.Step
    def int = Step.leaf((sb: StringBuilder, a: Int) => sb.append(a): Unit)
    def long = Step.leaf((sb: StringBuilder, a: Long) => sb.append(a): Unit)
    def double = Step.leaf((sb: StringBuilder, a: Double) => sb.append(Edn.double(a)): Unit)
    def bool = Step.leaf((sb: StringBuilder, a: Boolean) => sb.append(a): Unit)
    def string = Step.leaf((sb: StringBuilder, a: String) => sb.append(Edn.string(a)): Unit)
    def char = Step.leaf((sb: StringBuilder, a: Char) => sb.append(Edn.char(a)): Unit)
    def bytes = Step.leaf((sb: StringBuilder, a: Array[Byte]) => sb.append("#okay/bytes ").append(Edn.string(Base64.encode(a))): Unit)
    def bigInt = Step.leaf((sb: StringBuilder, a: BigInt) => sb.append(a).append('N'): Unit)
    def option[A](o: Schema.SOption[A], of: () => Enc[A]) = Step.option((sb: StringBuilder) => sb.append("nil"): Unit, of)
    private def seq[C, A](open: Char, close: Char, items: C => Iterable[A], of: () => Enc[A]): Enc[C] =
      Step.elems[StringBuilder, C, A, Unit, Unit](
        (sb, _) => sb.append(open): Unit, items, of,
        (sb, i) => if (i > 0) sb.append(' '): Unit,
        (_, _) => (), (sb, _, _) => sb.append(close): Unit)
    def list[A](l: Schema.SList[A], of: () => Enc[A]) = seq[List[A], A]('(', ')', identity, of)
    def vector[A](v: Schema.SVector[A], of: () => Enc[A]) = seq[Vector[A], A]('[', ']', identity, of)
    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Enc, Any])]) =
      Step.fields[StringBuilder, A, Unit, Unit, Enc](
        (sb, _) => sb.append('{'): Unit, p.parts, fields,
        (sb, i, n) => { if (i > 0) sb.append(' '); sb.append(':').append(n).append(' '): Unit },
        (_, _) => (), (sb, _, _) => sb.append('}'): Unit)
    // a sum is a TAGGED value, namespaced by the sum (EDN reserves bare tags)
    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Enc, A])]) =
      Step.one[StringBuilder, A, Unit, Unit, Enc](
        (_, _) => (), su.caseOf, cases,
        (sb, n) => sb.append('#').append(su.name).append('/').append(n).append(' '): Unit,
        (_, _, _, _) => ())
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Enc[B]) = Step.via(iso.from, under)
    def ref[A](name: String) =
      throw new IllegalStateException(s"a lazy carrier never meets a back edge, got one at $name")
  })

  // ============================================================ decode

  /** EDN text as a value of the Schema */
  def read[A](text: String)(implicit s: Schema[A]): Either[String, A] =
    parse(text).flatMap(decode(s))

  def decode[A](s: Schema[A])(e: Edn): Either[String, A] = decodeAt(s, e, 0)

  private def decodeAt[A](s: Schema[A], e: Edn, depth: Int): Either[String, A] =
    if (depth >= Codecs.NativeThreshold) Cont.reset(decodeC[A, Either[String, A]](s, e))
    else decodeNative(s, e, depth)

  private def kindOf(s: Schema[_]): String = s match {
    case p: Schema.SProduct[_] => p.name
    case su: Schema.SSum[_] => su.name
    case _: Schema.SOption[_] => "SOption"
    case _: Schema.SList[_] => "SList"
    case _: Schema.SVector[_] => "SVector"
    case _: Schema.SIso[_, _] => "SIso"
    case leaf => leaf.toString // the case objects' own names
  }

  private def mismatch[A](want: Schema[_], got: Edn): Either[String, A] =
    Left(s"expected ${kindOf(want)}, got ${show(got).take(60)}")

  /** a product's fields by name: keyword keys (and, leniently, strings) */
  private def entriesOf(kvs: Vector[(Edn, Edn)]): Map[String, Edn] =
    kvs.collect {
      case (EKeyword(None, k), v) => k -> v
      case (EStr(k), v) => k -> v
    }.toMap

  private def items(e: Edn): Option[Vector[Edn]] = e match {
    case EList(xs) => Some(xs)
    case EVector(xs) => Some(xs)
    case _ => None
  }

  /** the case a tag names: `#Sum/Case`, or a bare `#Case` */
  private def caseTagged[A](su: Schema.SSum[A], ns: Option[String], name: String) =
    if (ns.forall(_ == su.name)) su.cases.find(_._1 == name) else None

  /** a field the map does not carry: its default, None for an option, or
   * named as missing */
  private def absent(p: Schema.SProduct[_], i: Int): Either[String, Any] = {
    val (name, sc) = p.fields(i)
    p.defaults.lift(i).flatten match {
      case Some(d) => Right(d())
      case None => sc() match {
        case _: Schema.SOption[_] => Right(None)
        case _ => Left(s"missing field :$name in ${p.name}")
      }
    }
  }

  private type Dec[X] = Either[String, X]

  /** the leaves, shared by both roads */
  private def leaves(s: Schema[_], e: Edn): Schema.Visit[Dec] = new Schema.Visit[Dec] {
    def int = e match {
      case ELong(n) => if (n.isValidInt) Right(n.toInt) else Left(s"$n does not fit an Int")
      case g => mismatch(s, g)
    }
    def long = e match {
      case ELong(n) => Right(n)
      case EBig(n) => Left(s"${n}N does not fit a Long")
      case g => mismatch(s, g)
    }
    def double = e match {
      case EDouble(d) => Right(d)
      case ELong(n) => Right(n.toDouble)
      case EDec(d) => Right(d.toDouble)
      case g => mismatch(s, g)
    }
    def bool = e match { case EBool(b) => Right(b); case g => mismatch(s, g) }
    def string = e match { case EStr(x) => Right(x); case g => mismatch(s, g) }
    def char = e match { case EChar(c) => Right(c); case g => mismatch(s, g) }
    def bytes = e match {
      case ETagged(Some("okay"), "bytes", EStr(x)) => Base64.decode(x)
      case g => mismatch(s, g)
    }
    def bigInt = e match {
      case EBig(n) => Right(n)
      case ELong(n) => Right(BigInt(n))
      case g => mismatch(s, g)
    }
    // the containers are each road's own; these answers are never asked
    def option[A](o: Schema.SOption[A]) = mismatch(s, e)
    def list[A](l: Schema.SList[A]) = mismatch(s, e)
    def vector[A](v: Schema.SVector[A]) = mismatch(s, e)
    def product[A](p: Schema.SProduct[A]) = mismatch(s, e)
    def sum[A](su: Schema.SSum[A]) = mismatch(s, e)
    def iso[A, B](i: Schema.SIso[A, B]) = mismatch(s, e)
  }

  private def decodeNative[A](s: Schema[A], e: Edn, depth: Int): Either[String, A] = s.visit(new Schema.Visit[Dec] {
    private val leaf = leaves(s, e)
    def int = leaf.int
    def long = leaf.long
    def double = leaf.double
    def bool = leaf.bool
    def string = leaf.string
    def char = leaf.char
    def bytes = leaf.bytes
    def bigInt = leaf.bigInt
    def option[B](o: Schema.SOption[B]) = e match {
      case ENil => Right(None)
      case v => decodeAt(o.of(), v, depth + 1).map(Some(_))
    }
    def list[B](l: Schema.SList[B]) = items(e) match {
      case Some(xs) =>
        xs.foldLeft(Right(Nil): Either[String, List[B]]) { (acc, x) =>
          acc.flatMap(ys => decodeAt(l.of(), x, depth + 1).map(_ :: ys))
        }.map(_.reverse)
      case None => mismatch(s, e)
    }
    def vector[B](v: Schema.SVector[B]) = items(e) match {
      case Some(xs) =>
        xs.foldLeft(Right(Vector.empty): Either[String, Vector[B]]) { (acc, x) =>
          acc.flatMap(ys => decodeAt(v.of(), x, depth + 1).map(ys :+ _))
        }
      case None => mismatch(s, e)
    }
    def product[B](p: Schema.SProduct[B]) = e match {
      case EMap(kvs) =>
        val m = entriesOf(kvs)
        p.fields.indices.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, i) =>
          val (name, sc) = p.fields(i)
          acc.flatMap { xs =>
            m.get(name) match {
              case Some(v) => field(sc(), v, depth + 1).map(xs :+ _)
              case None => absent(p, i).map(xs :+ _)
            }
          }
        }.map(p.make)
      case g => mismatch(s, g)
    }
    def sum[B](su: Schema.SSum[B]) = e match {
      case ETagged(ns, name, v) =>
        caseTagged(su, ns, name)
          .toRight(s"unknown case '${qualified(ns, name)}' of ${su.name}")
          .flatMap { case (_, sc) => decodeAt(sc(), v, depth + 1) }
      case g => mismatch(s, g)
    }
    def iso[B, C](i: Schema.SIso[B, C]) = decodeAt(i.under(), e, depth + 1).flatMap(i.to)
  })

  private def field[X](sc: Schema[X], v: Edn, depth: Int): Either[String, Any] = decodeAt(sc, v, depth)

  private type DecC[R] = { type L[X] = Either[String, X] /> R }

  /** the road past `NativeThreshold`: the same decisions, each child a
   * `Cont.defer`, so depth costs heap */
  private def decodeC[A, R](s: Schema[A], e: Edn): Either[String, A] /> R = s.visit(new Schema.Visit[DecC[R]#L] {
    private val leaf = leaves(s, e)
    private def now[X](r: Either[String, X]): Either[String, X] /> R = Cont.Pure(r)
    def int = now(leaf.int)
    def long = now(leaf.long)
    def double = now(leaf.double)
    def bool = now(leaf.bool)
    def string = now(leaf.string)
    def char = now(leaf.char)
    def bytes = now(leaf.bytes)
    def bigInt = now(leaf.bigInt)
    def option[B](o: Schema.SOption[B]) = e match {
      case ENil => now(Right(None))
      case v => Cont.defer(() => decodeC[B, R](o.of(), v))((r: Either[String, B]) => now(r.map(Some(_))))
    }
    private def elems[B](of: Schema[B], xs: Vector[Edn]): Either[String, List[B]] /> R = {
      def loop(rest: List[Edn], acc: List[B]): Either[String, List[B]] /> R = rest match {
        case Nil => now(Right(acc.reverse))
        case x :: more => Cont.defer(() => decodeC[B, R](of, x)) {
          case Left(err) => now[List[B]](Left(err))
          case Right(y) => loop(more, y :: acc)
        }
      }
      loop(xs.toList, Nil)
    }
    def list[B](l: Schema.SList[B]) = items(e) match {
      case Some(xs) => elems(l.of(), xs)
      case None => now(mismatch(s, e))
    }
    def vector[B](v: Schema.SVector[B]) = items(e) match {
      case Some(xs) => elems(v.of(), xs).map(_.map(_.toVector))
      case None => now(mismatch(s, e))
    }
    def product[B](p: Schema.SProduct[B]) = e match {
      case EMap(kvs) =>
        val m = entriesOf(kvs)
        def fieldC[X](sc: Schema[X], v: Edn): Either[String, Any] /> R =
          Cont.defer(() => decodeC[X, R](sc, v))((r: Either[String, X]) => now[Any](r))
        // the field decoded inside Cont.defer continues from its
        // continuation, a call that cannot be a jump; `again` takes it
        def again(i: Int, acc: Vector[Any]): Either[String, Vector[Any]] /> R = loop(i, acc)
        @tailrec def loop(i: Int, acc: Vector[Any]): Either[String, Vector[Any]] /> R =
          if (i >= p.fields.length) now(Right(acc))
          else {
            val (name, sc) = p.fields(i)
            m.get(name) match {
              case None => absent(p, i) match {
                case Left(err) => now(Left(err))
                case Right(v) => loop(i + 1, acc :+ v)
              }
              case Some(v) => Cont.defer(() => fieldC(sc(), v)) {
                case Left(err) => now[Vector[Any]](Left(err))
                case Right(x) => again(i + 1, acc :+ x)
              }
            }
          }
        loop(0, Vector.empty).map(_.map(p.make))
      case g => now(mismatch(s, g))
    }
    def sum[B](su: Schema.SSum[B]) = e match {
      case ETagged(ns, name, v) => caseTagged(su, ns, name) match {
        case None => now(Left(s"unknown case '${qualified(ns, name)}' of ${su.name}"))
        case Some((_, sc)) => caseC(sc(), v)
      }
      case g => now(mismatch(s, g))
    }
    private def caseC[B, X <: B](sc: Schema[X], v: Edn): Either[String, B] /> R =
      Cont.defer(() => decodeC[X, R](sc, v))((r: Either[String, X]) => now[B](r))
    def iso[B, C](i: Schema.SIso[B, C]) =
      Cont.defer(() => decodeC[C, R](i.under(), e))((r: Either[String, C]) => now(r.flatMap(i.to)))
  })
}
