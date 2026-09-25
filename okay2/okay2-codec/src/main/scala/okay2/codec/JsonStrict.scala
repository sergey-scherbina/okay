package okay2.codec

import okay2.{Cont, reset, />}

/**
 * The STRICT JSON reader (okay-codec's JsonStrict.scala): characters
 * straight into a `Schema`, no tokens, no CST, no `Json` tree — the
 * second door beside the lossless one, named for what it gives up.
 *
 * THE LAW (TestJsonStrict): for every `a`,
 * `readStrict(write(a)) == read(write(a))` — the lossless road's answer
 * on well-formed input, including the product rules (unknown fields
 * ignored; an absent field takes its default, then None-if-optional,
 * then the refusal). On a truncated or damaged document this reader
 * answers `Left` where the lossless one still projects: that is the
 * trade, and it is a test.
 */
object JsonStrict {

  def read[A](input: String)(implicit s: Schema[A]): Either[String, A] = {
    val r = new Reader(input)
    r.skipWs()
    r.get(s).flatMap { a =>
      r.skipWs()
      if (r.at == input.length) Right(a)
      else Left(s"trailing input at ${r.at}")
    }
  }

  final class Reader(s: String) {
    var at = 0
    private val n = s.length

    /** open containers, for the native/trampoline switch */
    private var open = 0

    def skipWs(): Unit =
      while (at < n && { val c = s.charAt(at); c == ' ' || c == '\n' || c == '\r' || c == '\t' }) at += 1

    def peek: Int = if (at < n) s.charAt(at).toInt else -1

    def fail[X](what: String): Either[String, X] =
      Left(s"$what at $at" + (if (at < n) s" ('${s.charAt(at)}')" else " (end of input)"))

    /** BOUNDED (specs/stack-safety.md): a container descent counts one
     * `open`, and past Codecs.NativeThreshold the reader continues on
     * Cont; an Option or iso step between two containers is bounded by
     * the schema, which must cross a product or sum to recurse */
    def get[A](sc: Schema[A]): Either[String, A] =
      if (open >= Codecs.NativeThreshold) reset(getC[A, Either[String, A]](sc))
      else getNative(sc)

    private def one(x: String): Either[String, Char] =
      if (x.length == 1) Right(x.head) else Left(s"expected one character, got ${x.length}")

    private type Dec[X] = Either[String, X]

    private def getNative[A](sc: Schema[A]): Either[String, A] = sc.visit(new Schema.Visit[Dec] {
      def int = number().flatMap(Numbers.int)
      def long = number().flatMap(Numbers.long)
      def double = number()
      def bool = Reader.this.bool()
      def string = Reader.this.string()
      def char = Reader.this.string().flatMap(one)
      def bytes = Reader.this.string().flatMap(Base64.decode)
      def bigInt = Reader.this.bigInt()
      def option[B](o: Schema.SOption[B]) = if (lit("null")) Right(None) else get(o.of()).map(Some(_))
      def list[B](l: Schema.SList[B]) = inside(arrayHere(l.of())).map(_.toList)
      def vector[B](v: Schema.SVector[B]) = inside(arrayHere(v.of()))
      def product[B](p: Schema.SProduct[B]) = inside(productHere(p))
      def sum[B](su: Schema.SSum[B]) = inside(sumHere(su))
      def iso[B, C](i: Schema.SIso[B, C]) = get(i.under()).flatMap(i.to)
    })

    private def inside[X](body: => Either[String, X]): Either[String, X] = {
      open += 1
      val out = body
      open -= 1
      out
    }

    def bool(): Either[String, Boolean] =
      if (lit("true")) Right(true)
      else if (lit("false")) Right(false)
      else fail("expected true or false")

    def lit(word: String): Boolean =
      if (s.startsWith(word, at)) { at += word.length; true } else false

    def expect(c: Char): Either[String, Unit] =
      if (at < n && s.charAt(at) == c) { at += 1; Right(()) } else fail(s"expected '$c'")

    private def arrayHere[X](of: Schema[X]): Either[String, Vector[X]] =
      expect('[').flatMap { _ =>
        skipWs()
        if (peek == ']') { at += 1; Right(Vector.empty) }
        else {
          val b = Vector.newBuilder[X]
          var err: String = null
          var done = false
          while (err == null && !done) {
            skipWs()
            get(of) match {
              case Left(e) => err = e
              case Right(x) =>
                b += x
                skipWs()
                peek match {
                  case ',' => at += 1
                  case ']' => at += 1; done = true
                  case _ => err = fail[Unit]("expected ',' or ']'").swap.getOrElse("")
                }
            }
          }
          if (err != null) Left(err) else Right(b.result())
        }
      }

    /** the product from the fields found: each by name, else its
     * default, else None if optional, else the refusal */
    private def assemble[A](p: Schema.SProduct[A], found: Map[String, Any]): Either[String, A] =
      p.fields.indices.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, i) =>
        acc.flatMap { xs =>
          found.get(p.fields(i)._1) match {
            case Some(v) => Right(xs :+ v)
            case None => Json.absent(p, i).map(xs :+ _)
          }
        }
      }.map(p.make)

    private def productHere[A](p: Schema.SProduct[A]): Either[String, A] =
      expect('{').flatMap { _ =>
        skipWs()
        var found = Map.empty[String, Any]
        var err: String = null
        if (peek == '}') at += 1
        else {
          var done = false
          while (err == null && !done) {
            skipWs()
            string() match {
              case Left(e) => err = e
              case Right(k) =>
                skipWs()
                expect(':') match {
                  case Left(e) => err = e
                  case Right(_) =>
                    skipWs()
                    val r: Either[String, Unit] = p.fields.find(_._1 == k) match {
                      case Some((_, sc)) => get(sc()).map(v => { found = found + (k -> v); () })
                      case None => skipValue()
                    }
                    r match {
                      case Left(e) => err = e
                      case Right(_) =>
                        skipWs()
                        peek match {
                          case ',' => at += 1
                          case '}' => at += 1; done = true
                          case _ => err = fail[Unit]("expected ',' or '}'").swap.getOrElse("")
                        }
                    }
                }
            }
          }
        }
        if (err != null) Left(err) else assemble(p, found)
      }

    private def caseHead[A](su: Schema.SSum[A]): Either[String, () => Schema[_ <: A]] =
      expect('{').flatMap { _ =>
        skipWs()
        string().flatMap { name =>
          skipWs()
          expect(':').flatMap { _ =>
            skipWs()
            su.cases.find(_._1 == name).map(_._2).toRight(s"unknown case '$name' of ${su.name}")
          }
        }
      }

    private def sumHere[A](su: Schema.SSum[A]): Either[String, A] =
      caseHead(su).flatMap(sc => get(sc())).flatMap { v => skipWs(); expect('}').map(_ => v) }

    /** past one value of any shape, checking only that it ends */
    def skipValue(): Either[String, Unit] =
      peek match {
        case '"' => string().map(_ => ())
        case '{' | '[' =>
          var depth = 0
          var err: String = null
          var done = false
          while (err == null && !done) {
            if (at >= n) err = "unterminated value"
            else s.charAt(at) match {
              case '{' | '[' => depth += 1; at += 1
              case '}' | ']' => depth -= 1; at += 1; if (depth == 0) done = true
              case '"' => string() match { case Left(e) => err = e; case Right(_) => () }
              case _ => at += 1
            }
          }
          if (err != null) Left(err) else Right(())
        case _ =>
          val start = at
          while (at < n && { val c = s.charAt(at); c != ',' && c != '}' && c != ']' && c != ' ' && c != '\n' && c != '\r' && c != '\t' }) at += 1
          if (at == start) fail("expected a value") else Right(())
      }

    def bigInt(): Either[String, BigInt] =
      if (peek == '"') string().flatMap(BigInts.fromDigits)
      else number().flatMap(BigInts.fromNumber)

    def string(): Either[String, String] =
      if (at >= n || s.charAt(at) != '"') fail("expected a string")
      else {
        at += 1
        val start = at
        var i = at
        var plain = true
        var closed = false
        while (!closed && i < n) {
          val c = s.charAt(i)
          if (c == '"') closed = true
          else if (c == '\\') { plain = false; i += 2 }
          else if (c < ' ') i = n
          else i += 1
        }
        if (!closed || i > n) fail("unterminated string")
        else if (plain) { at = i + 1; Right(s.substring(start, i)) }
        else {
          val b = new java.lang.StringBuilder(i - start)
          var j = start
          while (j < i) {
            val c = s.charAt(j)
            if (c == '\\' && j + 1 < i) {
              s.charAt(j + 1) match {
                case 'n' => b.append('\n'); j += 2
                case 't' => b.append('\t'); j += 2
                case 'r' => b.append('\r'); j += 2
                case 'u' if j + 6 <= i =>
                  Json.hex4(s.substring(j + 2, j + 6)) match {
                    case Some(ch) => b.append(ch); j += 6
                    case None => b.append('u'); j += 2
                  }
                case x => b.append(x); j += 2
              }
            } else { b.append(c); j += 1 }
          }
          at = i + 1
          Right(b.toString)
        }
      }

    // ---- the trampoline: getNative case for case, each descent a
    // Cont.defer inside reset's loop ----

    private type DecC[R] = { type L[X] = Either[String, X] /> R }

    private def insideC[X, R](body: => (Either[String, X] /> R)): Either[String, X] /> R = {
      open += 1
      body.flatMap { v => open -= 1; Cont.Pure[Either[String, X], R](v) }
    }

    private def getC[A, R](sc: Schema[A]): Either[String, A] /> R = sc.visit(new Schema.Visit[DecC[R]#L] {
      private def now[X](e: Either[String, X]): Either[String, X] /> R = Cont.Pure(e)
      def int = now(number().flatMap(Numbers.int))
      def long = now(number().flatMap(Numbers.long))
      def double = now(number())
      def bool = now(Reader.this.bool())
      def string = now(Reader.this.string())
      def char = now(Reader.this.string().flatMap(one))
      def bytes = now(Reader.this.string().flatMap(Base64.decode))
      def bigInt = now(Reader.this.bigInt())
      def option[B](o: Schema.SOption[B]) =
        if (lit("null")) now(Right(None))
        else Cont.defer(() => getC[B, R](o.of()))((r: Either[String, B]) => now(r.map(Some(_))))
      def list[B](l: Schema.SList[B]) = insideC[Vector[B], R](arrayHereC[B, R](l.of())).map(_.map(_.toList))
      def vector[B](v: Schema.SVector[B]) = insideC[Vector[B], R](arrayHereC[B, R](v.of()))
      def product[B](p: Schema.SProduct[B]) = insideC[B, R](productHereC[B, R](p))
      def sum[B](su: Schema.SSum[B]) = insideC[B, R](sumHereC[B, R](su))
      def iso[B, C](i: Schema.SIso[B, C]) =
        Cont.defer(() => getC[C, R](i.under()))((r: Either[String, C]) => now(r.flatMap(i.to)))
    })

    private def arrayHereC[X, R](of: Schema[X]): Either[String, Vector[X]] /> R =
      expect('[') match {
        case Left(e) => Cont.Pure(Left(e))
        case Right(_) =>
          skipWs()
          if (peek == ']') { at += 1; Cont.Pure(Right(Vector.empty)) }
          else {
            def loop(acc: Vector[X]): Either[String, Vector[X]] /> R = {
              skipWs()
              Cont.defer(() => getC[X, R](of)) {
                case Left(e) => Cont.Pure[Either[String, Vector[X]], R](Left(e))
                case Right(x) =>
                  val acc2 = acc :+ x
                  skipWs()
                  peek match {
                    case ',' => at += 1; loop(acc2)
                    case ']' => at += 1; Cont.Pure[Either[String, Vector[X]], R](Right(acc2))
                    case _ => Cont.Pure[Either[String, Vector[X]], R](fail[Vector[X]]("expected ',' or ']'"))
                  }
              }
            }
            loop(Vector.empty)
          }
      }

    private def productHereC[A, R](p: Schema.SProduct[A]): Either[String, A] /> R =
      expect('{') match {
        case Left(e) => Cont.Pure(Left(e))
        case Right(_) =>
          skipWs()
          if (peek == '}') { at += 1; Cont.Pure(assemble(p, Map.empty)) }
          else {
            // local, so R is in scope and X is inferred from the field's
            // schema: no cast from `Schema[_]`
            def fieldC[X](sc: Schema[X]): Either[String, Any] /> R =
              getC[X, R](sc).map(e => e: Either[String, Any])
            def afterField(found: Map[String, Any]): Either[String, A] /> R = {
              skipWs()
              peek match {
                case ',' => at += 1; loop(found)
                case '}' => at += 1; Cont.Pure(assemble(p, found))
                case _ => Cont.Pure(fail[A]("expected ',' or '}'"))
              }
            }
            // Fields up to the next KNOWN one, by ITERATION. A known field
            // descends through Cont.defer, so its continuation runs in the
            // trampoline's loop; an unknown one is skipped in place, and its
            // separator is read here rather than by a call back into `loop`,
            // which cost a frame per skipped field and overflowed on an
            // object with many of them (stack-safety-json, 2026-09-25)
            def loop(found: Map[String, Any]): Either[String, A] /> R = {
              // an abstract Cont.Rep cannot hold null: the answer, once found
              var out: Option[Either[String, A] /> R] = None
              while (out.isEmpty) {
                skipWs()
                string() match {
                  case Left(e) => out = Some(Cont.Pure(Left(e)))
                  case Right(k) =>
                    skipWs()
                    expect(':') match {
                      case Left(e) => out = Some(Cont.Pure(Left(e)))
                      case Right(_) =>
                        skipWs()
                        p.fields.find(_._1 == k) match {
                          case Some((_, sc)) =>
                            out = Some(Cont.defer(() => fieldC(sc())) {
                              case Left(e) => Cont.Pure[Either[String, A], R](Left(e))
                              case Right(v) => afterField(found + (k -> v))
                            })
                          case None => skipValue() match {
                            case Left(e) => out = Some(Cont.Pure(Left(e)))
                            case Right(_) =>
                              skipWs()
                              peek match {
                                case ',' => at += 1
                                case '}' => at += 1; out = Some(Cont.Pure(assemble(p, found)))
                                case _ => out = Some(Cont.Pure(fail[A]("expected ',' or '}'")))
                              }
                          }
                        }
                    }
                }
              }
              out.get
            }
            loop(Map.empty)
          }
      }

    private def sumHereC[A, R](su: Schema.SSum[A]): Either[String, A] /> R = {
      def caseValueC[X <: A](sc: Schema[X]): Either[String, A] /> R =
        getC[X, R](sc).map(e => e: Either[String, A])
      caseHead(su) match {
        case Left(e) => Cont.Pure(Left(e))
        case Right(sc) =>
          Cont.defer(() => caseValueC(sc())) {
            case Left(e) => Cont.Pure[Either[String, A], R](Left(e))
            case Right(v) =>
              skipWs()
              Cont.Pure[Either[String, A], R](expect('}').map(_ => v))
          }
      }
    }

    /** RFC 8259 number; the value is parseDouble's */
    def number(): Either[String, Double] = {
      val start = at
      var i = at
      def digits(): Unit = while (i < n && s.charAt(i) >= '0' && s.charAt(i) <= '9') i += 1
      if (i < n && s.charAt(i) == '-') i += 1
      val int: Either[String, Unit] =
        if (i >= n) fail("expected a number")
        else if (s.charAt(i) == '0') { i += 1; Right(()) }
        else if (s.charAt(i) >= '1' && s.charAt(i) <= '9') { digits(); Right(()) }
        else fail("expected a number")
      val frac = int.flatMap { _ =>
        if (i < n && s.charAt(i) == '.') {
          i += 1
          val fs = i
          digits()
          if (i == fs) fail("expected digits after '.'") else Right(())
        } else Right(())
      }
      val exp = frac.flatMap { _ =>
        if (i < n && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) {
          i += 1
          if (i < n && (s.charAt(i) == '+' || s.charAt(i) == '-')) i += 1
          val es = i
          digits()
          if (i == es) fail("expected digits in the exponent") else Right(())
        } else Right(())
      }
      exp.flatMap { _ =>
        at = i
        try Right(java.lang.Double.parseDouble(s.substring(start, i)))
        catch { case _: NumberFormatException => fail("not a number") }
      }
    }
  }
}
