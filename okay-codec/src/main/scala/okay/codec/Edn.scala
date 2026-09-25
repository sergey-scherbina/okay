package okay.codec

import okay.{Cont, reset, />}
import scala.collection.mutable
import scala.annotation.tailrec

/**
 * EDN — Clojure's extensible data notation (https://github.com/edn-format/edn)
 * — as an okay codec beside JSON and CBOR (specs/codecs.md, edn-codec).
 *
 * EDN says what JSON cannot: a KEYWORD is not a string, a SET is not a
 * vector, a 64-bit integer is exact (and `123N` is a big one), a
 * character is not a one-letter string, and a value can carry a TAG
 * naming what it is. So a `Schema` maps onto it without the JSON
 * compromises:
 *
 *  - a product is a map with keyword keys — `{:name "okay" :age 3}`;
 *  - a sum is a TAGGED value, the tag namespaced by the sum's name —
 *    `#Shape/Circle {:r 1.0}` (EDN reserves un-namespaced tags);
 *  - `SLong` is exact to 64 bits, `SBigInt` is `123N`, `SChar` is `\c`;
 *  - `SList` is a list `(…)`, `SVector` a vector `[…]` (either is read for
 *    either); bytes are `#okay/bytes "base64"`; `None` is `nil`.
 *
 * Text is read and printed with an EXPLICIT stack, so nesting depth is
 * bounded by memory on every platform; a `Schema` is encoded by the
 * `Step` algebra JSON's encoder uses and decoded natively up to
 * `Codecs.NativeThreshold`, then on the `Cont` road — JSON's two roads,
 * so a deep document decodes on the default stack.
 */
enum Edn:
  case ENil
  case EBool(b: Boolean)
  case ELong(n: Long)
  /** an integer beyond 64 bits, or written with `N` */
  case EBig(n: BigInt)
  case EDouble(d: Double)
  /** a decimal written with `M` */
  case EDec(d: BigDecimal)
  case EStr(s: String)
  case EChar(c: Char)
  case EKeyword(ns: Option[String], name: String)
  case ESymbol(ns: Option[String], name: String)
  case EList(items: Vector[Edn])
  case EVector(items: Vector[Edn])
  case EMap(entries: Vector[(Edn, Edn)])
  case ESet(items: Vector[Edn])
  case ETagged(ns: Option[String], name: String, value: Edn)

object Edn {

  // ================================================================ read

  /** EDN text as one value; `Left` names the first thing wrong, where */
  def parse(text: String): Either[String, Edn] =
    val r = Reader(text)
    r.value() match
      case Left(e) => Left(e)
      case Right(None) => Left("edn: empty input")
      case Right(Some(v)) =>
        r.skip()
        if r.at < text.length then Left(s"edn: trailing input at ${r.at}: '${text.substring(r.at, math.min(text.length, r.at + 20))}'")
        else Right(v)

  /** every top-level value in the text, in order */
  def parseAll(text: String): Either[String, Vector[Edn]] =
    val r = Reader(text)
    val out = Vector.newBuilder[Edn]
    var err: Option[String] = None
    var go = true
    while go && err.isEmpty do
      r.value() match
        case Left(e) => err = Some(e)
        case Right(None) => go = false
        case Right(Some(v)) => out += v
    err.toLeft(out.result())

  private final class Reader(text: String):
    var at = 0

    /** a collection being filled; `Tagged` and `Discard` take ONE value */
    private enum Frame:
      case InList(items: mutable.ArrayBuffer[Edn])
      case InVector(items: mutable.ArrayBuffer[Edn])
      case InMap(items: mutable.ArrayBuffer[Edn])
      case InSet(items: mutable.ArrayBuffer[Edn])
      case Tagged(ns: Option[String], name: String)
      case Discard

    private def isWs(c: Char) = c.isWhitespace || c == ','
    private def isDelim(c: Char) = isWs(c) || "()[]{}\";".indexOf(c) >= 0
    private def isSymbolChar(c: Char) =
      c.isLetterOrDigit || ".*+!-_?$%&=<>/:#'".indexOf(c) >= 0

    def skip(): Unit =
      var go = true
      while go && at < text.length do
        val c = text.charAt(at)
        if isWs(c) then at += 1
        else if c == ';' then
          while at < text.length && text.charAt(at) != '\n' do at += 1
        else go = false

    private def fail(msg: String): Left[String, Nothing] = Left(s"edn: $msg at $at")

    /**
     * The next complete value, or None at the end of input. Iterative: a
     * stack of open collections, so nesting depth costs heap, not stack.
     */
    def value(): Either[String, Option[Edn]] =
      val stack = mutable.ArrayBuffer.empty[Frame]
      var result: Option[Edn] = None
      var err: Option[String] = None

      /** a finished value goes to the innermost open frame, or out */
      def complete(v0: Edn): Unit =
        var v: Option[Edn] = Some(v0)
        while v.isDefined && err.isEmpty do
          if stack.isEmpty then { result = v; v = None }
          else stack.last match
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

      def close(c: Char): Unit =
        if stack.isEmpty then err = Some(s"edn: unexpected '$c' at $at")
        else
          val top = stack.remove(stack.length - 1)
          (top, c) match
            case (Frame.InList(b), ')') => complete(EList(b.toVector))
            case (Frame.InVector(b), ']') => complete(EVector(b.toVector))
            case (Frame.InSet(b), '}') => complete(ESet(b.toVector))
            case (Frame.InMap(b), '}') =>
              if b.length % 2 != 0 then err = Some(s"edn: a map with an odd number of forms, closed at $at")
              else complete(EMap(b.grouped(2).map(p => (p(0), p(1))).toVector))
            case _ => err = Some(s"edn: '$c' closes nothing open here (at $at)")

      while result.isEmpty && err.isEmpty && (stack.nonEmpty || { skip(); at < text.length }) do
        skip()
        if at >= text.length then err = Some(s"edn: input ended inside an open form")
        else
          val c = text.charAt(at)
          c match
            case '(' => at += 1; stack += Frame.InList(mutable.ArrayBuffer.empty)
            case '[' => at += 1; stack += Frame.InVector(mutable.ArrayBuffer.empty)
            case '{' => at += 1; stack += Frame.InMap(mutable.ArrayBuffer.empty)
            case ')' | ']' | '}' => at += 1; close(c)
            case '"' => string().fold(e => err = Some(e), complete)
            case '\\' => char().fold(e => err = Some(e), complete)
            case '#' => dispatch(stack).fold(e => err = Some(e), _.foreach(complete))
            case _ => atom().fold(e => err = Some(e), complete)
      err.toLeft(result)

    /** after '#': a set, a discard, a symbolic value, or a tag */
    private def dispatch(stack: mutable.ArrayBuffer[Frame]): Either[String, Option[Edn]] =
      at += 1
      if at >= text.length then fail("'#' at the end of input")
      else text.charAt(at) match
        case '{' => at += 1; stack += Frame.InSet(mutable.ArrayBuffer.empty); Right(None)
        case '_' => at += 1; stack += Frame.Discard; Right(None)
        case '#' =>
          at += 1
          val start = at
          while at < text.length && !isDelim(text.charAt(at)) do at += 1
          text.substring(start, at) match
            case "Inf" => Right(Some(EDouble(Double.PositiveInfinity)))
            case "-Inf" => Right(Some(EDouble(Double.NegativeInfinity)))
            case "NaN" => Right(Some(EDouble(Double.NaN)))
            case other => fail(s"unknown symbolic value ##$other")
        case c if c.isLetter =>
          val start = at
          while at < text.length && !isDelim(text.charAt(at)) do at += 1
          val (ns, name) = split(text.substring(start, at))
          stack += Frame.Tagged(ns, name)
          Right(None)
        case c => fail(s"'#$c' is not EDN")

    private def split(s: String): (Option[String], String) =
      val slash = s.indexOf('/')
      if slash > 0 && slash < s.length - 1 then (Some(s.substring(0, slash)), s.substring(slash + 1))
      else (None, s)

    private def string(): Either[String, Edn] =
      val sb = new StringBuilder
      at += 1
      var err: Option[String] = None
      var done = false
      while !done && err.isEmpty do
        if at >= text.length then err = Some(s"edn: an unterminated string")
        else
          val c = text.charAt(at)
          at += 1
          if c == '"' then done = true
          else if c == '\\' then
            if at >= text.length then err = Some("edn: an unterminated escape")
            else
              val e = text.charAt(at)
              at += 1
              val _ = e match
                case 't' => sb.append('\t')
                case 'r' => sb.append('\r')
                case 'n' => sb.append('\n')
                case 'b' => sb.append('\b')
                case 'f' => sb.append('\f')
                case '\\' => sb.append('\\')
                case '"' => sb.append('"')
                case 'u' if at + 4 <= text.length =>
                  scala.util.Try(Integer.parseInt(text.substring(at, at + 4), 16)).toOption match
                    case Some(code) => sb.append(code.toChar); at += 4
                    case None => err = Some(s"edn: a bad \\u escape at $at")
                case other => err = Some(s"edn: unknown escape \\$other at $at")
          else sb.append(c): Unit
      err.toLeft(EStr(sb.toString))

    private def char(): Either[String, Edn] =
      at += 1
      val start = at
      if at >= text.length then fail("a '\\' at the end of input")
      else
        at += 1
        while at < text.length && !isDelim(text.charAt(at)) do at += 1
        text.substring(start, at) match
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

    private val IntPattern = "([+-]?[0-9]+)(N?)".r
    private val FloatPattern = "([+-]?[0-9]+(?:\\.[0-9]*)?(?:[eE][+-]?[0-9]+)?)(M?)".r

    /** a number, keyword, symbol, nil, true or false */
    private def atom(): Either[String, Edn] =
      val start = at
      while at < text.length && !isDelim(text.charAt(at)) do at += 1
      val tok = text.substring(start, at)
      if tok.isEmpty then fail(s"unexpected '${text.charAt(at)}'")
      else tok match
        case "nil" => Right(ENil)
        case "true" => Right(EBool(true))
        case "false" => Right(EBool(false))
        case IntPattern(digits, n) =>
          val big = BigInt(digits.stripPrefix("+"))
          if n.nonEmpty || !big.isValidLong then Right(EBig(big)) else Right(ELong(big.toLong))
        case FloatPattern(digits, m) =>
          if m.nonEmpty then Right(EDec(BigDecimal(digits))) else Right(EDouble(digits.toDouble))
        case k if k.startsWith(":") && k.length > 1 =>
          val (ns, name) = split(k.substring(1))
          Right(EKeyword(ns, name))
        case s if s.forall(isSymbolChar) && !s.head.isDigit =>
          val (ns, name) = split(s)
          Right(ESymbol(ns, name))
        case s => Left(s"edn: '$s' is not an EDN value (at $start)")

  // =============================================================== print

  /** a value as EDN text; iterative, so depth costs heap */
  def show(e: Edn): String =
    val sb = new StringBuilder
    val work = mutable.Stack.empty[Either[String, Edn]]
    work.push(Right(e))
    def seq(open: String, close: String, items: Vector[Edn]): Unit =
      sb.append(open)
      work.push(Left(close))
      items.indices.reverse.foreach { i =>
        work.push(Right(items(i)))
        if i > 0 then work.push(Left(" "))
      }
    while work.nonEmpty do
      work.pop() match
        case Left(s) => sb.append(s): Unit
        case Right(v) => v match
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
          case EMap(kvs) => seq("{", "}", kvs.flatMap((k, v) => Vector(k, v)))
          case ETagged(ns, n, value) =>
            val _ = sb.append('#').append(qualified(ns, n)).append(' ')
            work.push(Right(value))
    sb.toString

  private def qualified(ns: Option[String], name: String): String = ns.fold(name)(n => s"$n/$name")

  private def double(d: Double): String =
    if d.isNaN then "##NaN"
    else if d.isPosInfinity then "##Inf"
    else if d.isNegInfinity then "##-Inf"
    else
      val s = d.toString
      // EDN reads `1` as an integer: a double must look like one
      if s.exists(c => c == '.' || c == 'E' || c == 'e') then s else s + ".0"

  private def string(s: String): String =
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

  private def char(c: Char): String = c match
    case '\n' => "\\newline"
    case '\r' => "\\return"
    case ' ' => "\\space"
    case '\t' => "\\tab"
    case '\f' => "\\formfeed"
    case '\b' => "\\backspace"
    case c if c < ' ' || c.isWhitespace || "()[]{}\";,".indexOf(c) >= 0 => f"\\u${c.toInt}%04x"
    case c => s"\\$c"

  // ============================================================ encode

  /** a value as EDN text, through its Schema — stack-safe (the Step walk) */
  def write[A](a: A)(using s: Schema[A]): String = encode(s)(a)

  def encode[A](s: Schema[A])(a: A): String =
    val sb = new StringBuilder
    Schema.Step.walk(encoder(s), sb, a)
    sb.toString

  private type Enc[A] = Schema.Step[StringBuilder, A, Unit]
  private val encoder = Schema.Folded[Enc](new Schema.Algebra[Enc]:
    import Schema.Step
    def int = Step.leaf((sb, a: Int) => sb.append(a): Unit)
    def long = Step.leaf((sb, a: Long) => sb.append(a): Unit)
    def double = Step.leaf((sb, a: Double) => sb.append(Edn.double(a)): Unit)
    def bool = Step.leaf((sb, a: Boolean) => sb.append(a): Unit)
    def string = Step.leaf((sb, a: String) => sb.append(Edn.string(a)): Unit)
    def char = Step.leaf((sb, a: Char) => sb.append(Edn.char(a)): Unit)
    def bytes = Step.leaf((sb, a: Array[Byte]) => sb.append("#okay/bytes ").append(Edn.string(Base64.encode(a))): Unit)
    def bigInt = Step.leaf((sb, a: BigInt) => sb.append(a).append('N'): Unit)
    def option[A](o: Schema.SOption[A], of: () => Enc[A]) = Step.option(sb => sb.append("nil"): Unit, of)
    def list[A](l: Schema.SList[A], of: () => Enc[A]) = Step.elems[StringBuilder, List[A], A, Unit, Unit](
      (sb, _) => sb.append('('): Unit, identity, of,
      (sb, i) => if i > 0 then sb.append(' '): Unit,
      (_, _) => (), (sb, _, _) => sb.append(')'): Unit)
    def vector[A](v: Schema.SVector[A], of: () => Enc[A]) = Step.elems[StringBuilder, Vector[A], A, Unit, Unit](
      (sb, _) => sb.append('['): Unit, identity, of,
      (sb, i) => if i > 0 then sb.append(' '): Unit,
      (_, _) => (), (sb, _, _) => sb.append(']'): Unit)
    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Enc, Any])]) =
      Step.fields[StringBuilder, A, Unit, Unit](
        (sb, _) => sb.append('{'): Unit, p.parts, fields,
        (sb, i, n) => { if i > 0 then sb.append(' '); val _ = sb.append(':').append(n).append(' ') },
        (_, _) => (), (sb, _, _) => sb.append('}'): Unit)
    // a sum is a TAGGED value, namespaced by the sum (EDN reserves bare tags)
    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Enc, A])]) =
      Step.one[StringBuilder, A, Unit, Unit](
        (_, _) => (), su.caseOf, cases,
        (sb, n) => { val _ = sb.append('#').append(su.name).append('/').append(n).append(' ') },
        (_, _, _, _) => ())
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Enc[B]) = Step.via(iso.from, under)
    def ref[A](name: String) =
      throw IllegalStateException(s"a lazy carrier never meets a back edge, got one at $name")
  )

  // ============================================================ decode

  /** EDN text as a value of the Schema */
  def read[A](text: String)(using s: Schema[A]): Either[String, A] =
    parse(text).flatMap(decode(s))

  def decode[A](s: Schema[A])(e: Edn): Either[String, A] = decodeAt(s, e, 0)

  private def decodeAt[A](s: Schema[A], e: Edn, depth: Int): Either[String, A] =
    if depth >= Codecs.NativeThreshold then reset(decodeC[A, Either[String, A]](s, e))
    else decodeNative(s, e, depth)

  private def kindOf(s: Schema[?]): String = s match
    case Schema.SInt => "SInt"
    case Schema.SLong => "SLong"
    case Schema.SDouble => "SDouble"
    case Schema.SBool => "SBool"
    case Schema.SString => "SString"
    case Schema.SChar => "SChar"
    case Schema.SBytes => "SBytes"
    case Schema.SBigInt => "SBigInt"
    case _: Schema.SOption[?] => "SOption"
    case _: Schema.SList[?] => "SList"
    case _: Schema.SVector[?] => "SVector"
    case p: Schema.SProduct[?] => p.name
    case su: Schema.SSum[?] => su.name
    case _: Schema.SIso[?, ?] => "SIso"

  /** the leaves, shared by both roads: None when the node is not a leaf */
  private def leaf[A](s: Schema[A], e: Edn): Option[Either[String, A]] = (s, e) match
    case (Schema.SInt, ELong(n)) =>
      Some(if n.isValidInt then Right(n.toInt) else Left(s"$n does not fit an Int"))
    case (Schema.SLong, ELong(n)) => Some(Right(n))
    case (Schema.SLong, EBig(n)) => Some(Left(s"${n}N does not fit a Long"))
    case (Schema.SDouble, EDouble(d)) => Some(Right(d))
    case (Schema.SDouble, ELong(n)) => Some(Right(n.toDouble))
    case (Schema.SDouble, EDec(d)) => Some(Right(d.toDouble))
    case (Schema.SBool, EBool(b)) => Some(Right(b))
    case (Schema.SString, EStr(x)) => Some(Right(x))
    case (Schema.SChar, EChar(c)) => Some(Right(c))
    case (Schema.SBytes, ETagged(Some("okay"), "bytes", EStr(x))) => Some(Base64.decode(x))
    case (Schema.SBigInt, EBig(n)) => Some(Right(n))
    case (Schema.SBigInt, ELong(n)) => Some(Right(BigInt(n)))
    case _ => None

  /** a product's fields by name: keyword keys (and, leniently, strings) */
  private def entriesOf(kvs: Vector[(Edn, Edn)]): Map[String, Edn] =
    kvs.collect {
      case (EKeyword(None, k), v) => k -> v
      case (EStr(k), v) => k -> v
    }.toMap

  private def items(e: Edn): Option[Vector[Edn]] = e match
    case EList(xs) => Some(xs)
    case EVector(xs) => Some(xs)
    case _ => None

  /** the case a tag names: `#Sum/Case`, or a bare `#Case` */
  private def caseTagged[A](su: Schema.SSum[A], ns: Option[String], name: String) =
    if ns.forall(_ == su.name) then su.cases.find(_._1 == name) else None

  private def decodeNative[A](s: Schema[A], e: Edn, depth: Int): Either[String, A] = leaf(s, e) match
    case Some(r) => r
    case None => (s, e) match
      case (Schema.SOption(_), ENil) => Right(None)
      case (Schema.SOption(of), v) => decodeAt(of(), v, depth + 1).map(Some(_))
      case (l: Schema.SList[a], v) if items(v).isDefined =>
        items(v).get.foldLeft(Right(Nil): Either[String, List[a]]) { (acc, x) =>
          acc.flatMap(xs => decodeAt(l.of(), x, depth + 1).map(_ :: xs))
        }.map(_.reverse)
      case (vec: Schema.SVector[a], v) if items(v).isDefined =>
        items(v).get.foldLeft(Right(Vector.empty): Either[String, Vector[a]]) { (acc, x) =>
          acc.flatMap(xs => decodeAt(vec.of(), x, depth + 1).map(xs :+ _))
        }
      case (p: Schema.SProduct[A], EMap(kvs)) =>
        val m = entriesOf(kvs)
        p.fields.zipWithIndex.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, fi) =>
          val ((name, sc), i) = fi
          acc.flatMap { xs =>
            m.get(name) match
              case Some(v) => decodeField(sc(), v, depth + 1).map(xs :+ _)
              case None => absent(p, name, sc, i).map(xs :+ _)
          }
        }.map(p.make)
      case (su: Schema.SSum[A], ETagged(ns, name, v)) =>
        caseTagged(su, ns, name)
          .toRight(s"unknown case '${qualified(ns, name)}' of ${su.name}")
          .flatMap((_, sc) => decodeAt(sc(), v, depth + 1))
      case (Schema.SIso(u, to, _), v) => decodeAt(u(), v, depth + 1).flatMap(to)
      case (want, got) => Left(s"expected ${kindOf(want)}, got ${show(got).take(60)}")

  private def decodeField[X](sc: Schema[X], v: Edn, depth: Int): Either[String, Any] = decodeAt(sc, v, depth)

  /** a field the map does not carry: its default, None for an option, or
   * named as missing */
  private def absent(p: Schema.SProduct[?], name: String, sc: () => Schema[?], i: Int): Either[String, Any] =
    p.defaults.lift(i).flatten match
      case Some(d) => Right(d())
      case None => sc() match
        case _: Schema.SOption[?] => Right(None)
        case _ => Left(s"missing field :$name in ${p.name}")

  /** the road past `NativeThreshold`: the same decisions, each child a
   * `Cont.defer`, so depth costs heap (Json's decodeC, over EDN) */
  private def decodeC[A, R](s: Schema[A], e: Edn): Either[String, A] /> R = leaf(s, e) match
    case Some(r) => Cont.Pure(r)
    case None => (s, e) match
      case (Schema.SOption(_), ENil) => Cont.Pure(Right(None))
      case (Schema.SOption(of), v) =>
        Cont.defer(() => decodeC(of(), v))(r => Cont.Pure(r.map(Some(_))))
      case (l: Schema.SList[a], v) if items(v).isDefined =>
        def loop(rest: List[Edn], acc: List[a]): Either[String, List[a]] /> R = rest match
          case Nil => Cont.Pure(Right(acc.reverse))
          case x :: more => Cont.defer(() => decodeC(l.of(), x)) {
            case Left(err) => Cont.Pure(Left(err))
            case Right(y) => loop(more, y :: acc)
          }
        loop(items(v).get.toList, Nil)
      case (vec: Schema.SVector[a], v) if items(v).isDefined =>
        def loop(rest: List[Edn], acc: Vector[a]): Either[String, Vector[a]] /> R = rest match
          case Nil => Cont.Pure(Right(acc))
          case x :: more => Cont.defer(() => decodeC(vec.of(), x)) {
            case Left(err) => Cont.Pure(Left(err))
            case Right(y) => loop(more, acc :+ y)
          }
        loop(items(v).get.toList, Vector.empty)
      case (p: Schema.SProduct[A], EMap(kvs)) =>
        val m = entriesOf(kvs)
        // the field decoded inside Cont.defer continues from its continuation,
        // a call that cannot be a jump; `again` takes it, so this stays a loop
        def again(remaining: List[((String, () => Schema[?]), Int)], acc: Vector[Any]): Either[String, Vector[Any]] /> R = loop(remaining, acc)
        @tailrec def loop(remaining: List[((String, () => Schema[?]), Int)], acc: Vector[Any]): Either[String, Vector[Any]] /> R =
          remaining match
            case Nil => Cont.Pure(Right(acc))
            case ((name, sc), i) :: more => m.get(name) match
              case None => absent(p, name, sc, i) match
                case Left(err) => Cont.Pure(Left(err))
                case Right(v) => loop(more, acc :+ v)
              case Some(v) => Cont.defer(() => fieldC(sc(), v)) {
                case Left(err) => Cont.Pure(Left(err))
                case Right(x) => again(more, acc :+ x)
              }
        loop(p.fields.zipWithIndex.toList, Vector.empty).flatMap(r => Cont.Pure(r.map(p.make)))
      case (su: Schema.SSum[A], ETagged(ns, name, v)) =>
        caseTagged(su, ns, name) match
          case None => Cont.Pure(Left(s"unknown case '${qualified(ns, name)}' of ${su.name}"))
          case Some((_, sc)) => Cont.defer(() => decodeC(sc(), v))(r => Cont.Pure(r: Either[String, A]))
      case (Schema.SIso(u, to, _), v) =>
        Cont.defer(() => decodeC(u(), v))(r => Cont.Pure(r.flatMap(to)))
      case (want, got) => Cont.Pure(Left(s"expected ${kindOf(want)}, got ${show(got).take(60)}"))

  private def fieldC[X, R](sc: Schema[X], v: Edn): Either[String, Any] /> R =
    Cont.defer(() => decodeC(sc, v))(r => Cont.Pure(r: Either[String, Any]))
}
