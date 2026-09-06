package okay.codec

/**
 * The STRICT JSON reader: characters straight into a `Schema`, no
 * tokens, no CST, no `Json` tree — the second door beside the
 * lossless one, and named for what it gives up.
 *
 * `Json.read` is the lossless road: a scanner, a CST that keeps every
 * trivia token, a projection, and only then the `Schema` fold. It
 * buys byte-for-byte losslessness, damage-as-data, and a HALF-ARRIVED
 * document that still decodes (the LLM case) — and it reads 10.3us
 * where circe reads 0.623 (docs/benchmarks.md, the codec price list).
 * A caller decoding a complete, well-formed document wants none of
 * that and pays for all of it. This reader is for that caller.
 *
 * WHAT IT IS. `JsonValue.Parser`'s strict recursive descent — an
 * index over the string, a slice for a plain string, `parseDouble` on
 * a number's slice — driving `Cbor.get`'s walk over the `Schema`: a
 * product reads its fields by name into the erased parts, a sum is
 * the one-entry object `Json.encode` writes, an option is `null` or
 * the value, lists and vectors are arrays, iso decodes under and
 * maps. The three scanning primitives are copied from `JsonValue`
 * rather than shared: that file's contract is "the same VALUE the
 * projection gives", this one's is "the same ANSWER the lossless
 * decoder gives", and coupling them would let either drift the other.
 *
 * THE LAW (TestJsonStrict): for every `a`,
 * `readStrict(write(a)) == read(write(a))` — the same answer as the
 * lossless road on well-formed input, including the product rules
 * (unknown fields ignored; an absent field takes its declared default,
 * then None-if-optional, then the refusal). And on a truncated or
 * damaged document this reader answers `Left` where the lossless one
 * still projects — that is the trade, and it is a test, not a
 * footnote. It accepts ONLY what it is sure of, as `JsonValue.parse`
 * answers `None`: a stray character, a trailing value, a raw control
 * character in a string, an unterminated anything — all `Left`.
 */
object JsonStrict {

  def read[A](input: String)(using s: Schema[A]): Either[String, A] =
    val r = new Reader(input)
    r.skipWs()
    r.get(s).flatMap { a =>
      r.skipWs()
      if r.at == input.length then Right(a)
      else Left(s"trailing input at ${r.at}")
    }

  /**
   * PUBLIC, though it is an implementation detail, for the same
   * reason `Cbor.In` is: the staged strict reader (`Staged.strict`)
   * is generated code spliced into the CALLER's compilation unit,
   * and a package-private member reached from a quote becomes an
   * "unstable inline accessor" there -- "access from wrong staging
   * level" at compile time. What is public is the scanning surface
   * the generated code calls; `get` is the interpreted door's walk.
   */
  final class Reader(s: String) {
    var at = 0
    private val n = s.length

    def skipWs(): Unit =
      while at < n && { val c = s.charAt(at); c == ' ' || c == '\n' || c == '\r' || c == '\t' } do at += 1

    def peek: Int = if at < n then s.charAt(at).toInt else -1

    def fail[X](what: String): Either[String, X] =
      Left(s"$what at $at" + (if at < n then s" ('${s.charAt(at)}')" else " (end of input)"))

    /** the walk: `Cbor.get`'s shape, `Json.decode`'s rules */
    def get[A](sc: Schema[A]): Either[String, A] = sc match
      case Schema.SIso(u, to, _) => get(u()).flatMap(to)
      case Schema.SInt => number().map(_.toInt)
      case Schema.SLong => number().map(_.toLong)
      case Schema.SDouble => number()
      case Schema.SBool => bool()
      case Schema.SString => string()
      case Schema.SChar => string().flatMap(x =>
        if x.length == 1 then Right(x.head) else Left(s"expected one character, got ${x.length}"))
      case Schema.SBytes => string().flatMap(Base64.decode)
      case Schema.SOption(of) =>
        if lit("null") then Right(None) else get(of()).map(Some(_))
      case l: Schema.SList[a] => array[a](l.of()).map(_.toList)
      case v: Schema.SVector[a] => array[a](v.of())
      case p: Schema.SProduct[A] => product(p)
      case su: Schema.SSum[A] => sum(su)

    def bool(): Either[String, Boolean] =
      if lit("true") then Right(true)
      else if lit("false") then Right(false)
      else fail("expected true or false")

    def lit(word: String): Boolean =
      if s.startsWith(word, at) then { at += word.length; true } else false

    def expect(c: Char): Either[String, Unit] =
      if at < n && s.charAt(at) == c then { at += 1; Right(()) } else fail(s"expected '$c'")

    /** `[` v (`,` v)* `]`, each element at the element schema */
    def array[X](of: Schema[X]): Either[String, Vector[X]] =
      expect('[').flatMap { _ =>
        skipWs()
        if peek == ']' then { at += 1; Right(Vector.empty) }
        else
          val b = Vector.newBuilder[X]
          var err: String | Null = null
          var done = false
          while err == null && !done do
            skipWs()
            get(of) match
              case Left(e) => err = e
              case Right(x) =>
                b += x
                skipWs()
                peek match
                  case ',' => at += 1
                  case ']' => at += 1; done = true
                  case _ => err = fail[Unit]("expected ',' or ']'").swap.getOrElse("")
          val e = err
          if e != null then Left(e) else Right(b.result())
      }

    /**
     * `{` "k" `:` v (`,` "k" `:` v)* `}`. A declared field is read at
     * its own schema; an UNDECLARED one is skipped, as `Json.decode`
     * ignores it. Then the product is assembled exactly as the
     * lossless decoder assembles it: an absent field takes its
     * declared default, then None-if-optional, then the refusal.
     */
    private def product[A](p: Schema.SProduct[A]): Either[String, A] =
      expect('{').flatMap { _ =>
        skipWs()
        var found = Map.empty[String, Any]
        var err: String | Null = null
        if peek == '}' then at += 1
        else
          var done = false
          while err == null && !done do
            skipWs()
            string() match
              case Left(e) => err = e
              case Right(k) =>
                skipWs()
                expect(':') match
                  case Left(e) => err = e
                  case Right(_) =>
                    skipWs()
                    val r: Either[String, Unit] = p.fields.find(_._1 == k) match
                      case Some((_, sc)) => field(sc()).map(v => { found = found + (k -> v); () })
                      case None => skipValue()
                    r match
                      case Left(e) => err = e
                      case Right(_) =>
                        skipWs()
                        peek match
                          case ',' => at += 1
                          case '}' => at += 1; done = true
                          case _ => err = fail[Unit]("expected ',' or '}'").swap.getOrElse("")
        val e = err
        if e != null then Left(e)
        else
          p.fields.zipWithIndex.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, fi) =>
            val (f, i) = fi
            acc.flatMap { xs =>
              found.get(f._1) match
                case Some(v) => Right(xs :+ v)
                case None => p.defaults.lift(i).flatten match
                  case Some(d) => Right(xs :+ d())
                  case None => f._2() match
                    case _: Schema.SOption[?] => Right(xs :+ None)
                    case _ => Left(s"missing field '${f._1}' in ${p.name}")
            }
          }.map(p.make)
      }

    /** one field at its own type; the value joins the product's erased
     * parts, as in `Cbor.field` */
    private def field[X](sc: Schema[X]): Either[String, Any] = get(sc)

    /** the one-entry object `Json.encode` writes: `{"Case": value}` */
    private def sum[A](su: Schema.SSum[A]): Either[String, A] =
      expect('{').flatMap { _ =>
        skipWs()
        string().flatMap { name =>
          skipWs()
          expect(':').flatMap { _ =>
            skipWs()
            su.cases.find(_._1 == name)
              .toRight(s"unknown case '$name' of ${su.name}")
              .flatMap((_, sc) => get(sc()))
              .flatMap { v => skipWs(); expect('}').map(_ => v) }
          }
        }
      }

    /** skip one value of any shape — an undeclared field's — without
     * building it: strings by their own scan, everything else by
     * bracket depth */
    def skipValue(): Either[String, Unit] =
      peek match
        case '"' => string().map(_ => ())
        case '{' | '[' =>
          var depth = 0
          var err: String | Null = null
          var done = false
          while err == null && !done do
            if at >= n then err = "unterminated value"
            else s.charAt(at) match
              case '{' | '[' => depth += 1; at += 1
              case '}' | ']' => depth -= 1; at += 1; if depth == 0 then done = true
              case '"' => string() match { case Left(e) => err = e; case Right(_) => () }
              case _ => at += 1
          val e = err
          if e != null then Left(e) else Right(())
        case _ =>
          // a literal or a number: everything up to a delimiter
          val start = at
          while at < n && { val c = s.charAt(at); c != ',' && c != '}' && c != ']' && c != ' ' && c != '\n' && c != '\r' && c != '\t' } do at += 1
          if at == start then fail("expected a value") else Right(())

    /**
     * A string's content — `JsonValue.str`, verbatim: the slice when
     * no escape appears, the builder otherwise; `\n \t \r`, `\uXXXX`
     * as one UTF-16 code unit, any other escaped character is itself;
     * a raw control character is not ours.
     */
    def string(): Either[String, String] =
      if at >= n || s.charAt(at) != '"' then fail("expected a string")
      else
        at += 1
        val start = at
        var i = at
        var plain = true
        var closed = false
        while !closed && i < n do
          val c = s.charAt(i)
          if c == '"' then closed = true
          else if c == '\\' then { plain = false; i += 2 }
          else if c < ' ' then { i = n }
          else i += 1
        if !closed || i > n then fail("unterminated string")
        else if plain then { at = i + 1; Right(s.substring(start, i)) }
        else
          val b = new java.lang.StringBuilder(i - start)
          var j = start
          while j < i do
            val c = s.charAt(j)
            if c == '\\' && j + 1 < i then
              s.charAt(j + 1) match
                case 'n' => b.append('\n'): Unit; j += 2
                case 't' => b.append('\t'): Unit; j += 2
                case 'r' => b.append('\r'): Unit; j += 2
                case 'u' if j + 6 <= i =>
                  Json.hex4(s.substring(j + 2, j + 6)) match
                    case Some(ch) => b.append(ch): Unit; j += 6
                    case None => b.append('u'): Unit; j += 2
                case x => b.append(x): Unit; j += 2
            else { b.append(c): Unit; j += 1 }
          at = i + 1
          Right(b.toString)

    /** RFC 8259 number, `-? int frac? exp?`, as `JsonValue.num` reads
     * it; the value is `parseDouble`'s, which is what `JNum` holds and
     * what `Json.decode` truncates for `SInt`/`SLong` */
    def number(): Either[String, Double] =
      val start = at
      var i = at
      if i < n && s.charAt(i) == '-' then i += 1
      if i >= n then return fail("expected a number")
      if s.charAt(i) == '0' then i += 1
      else if s.charAt(i) >= '1' && s.charAt(i) <= '9' then
        while i < n && s.charAt(i) >= '0' && s.charAt(i) <= '9' do i += 1
      else return fail("expected a number")
      if i < n && s.charAt(i) == '.' then
        i += 1
        val fracStart = i
        while i < n && s.charAt(i) >= '0' && s.charAt(i) <= '9' do i += 1
        if i == fracStart then return fail("expected digits after '.'")
      if i < n && (s.charAt(i) == 'e' || s.charAt(i) == 'E') then
        i += 1
        if i < n && (s.charAt(i) == '+' || s.charAt(i) == '-') then i += 1
        val expStart = i
        while i < n && s.charAt(i) >= '0' && s.charAt(i) <= '9' do i += 1
        if i == expStart then return fail("expected digits in the exponent")
      at = i
      try Right(java.lang.Double.parseDouble(s.substring(start, i)))
      catch case _: NumberFormatException => fail("not a number")
  }
}
