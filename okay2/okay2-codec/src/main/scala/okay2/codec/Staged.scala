package okay2.codec

import scala.language.experimental.macros

/**
 * The STAGED fold mode (okay-codec's Staged.scala): the same algebra as
 * `Json.encode`/`decode`, `Cbor.write`/`read` and `JsonStrict.read`,
 * folded over the type's shape at COMPILE time, emitting straight-line
 * field access — appends into one StringBuilder (or CBOR items into one
 * `Cbor.Out`), a lookup per field, the constructor call.
 *
 * Scala 3 reads the type through a `Mirror` inside a quote; here a
 * blackbox macro (`StagedMacro`) reads the class itself, as
 * `Schema.derived` does (`SchemaMacro`): a case class is a product, a
 * case object an empty one, a sealed trait or abstract class a sum.
 *
 * The rule that keeps the staged mode FAITHFUL to its fold is Scala 3's:
 * structure is staged exactly where the run-time schema has the class's
 * shape, checked ONCE at construction (`productShape`/`sumShape`, a val
 * per product or sum the codec meets). Each staged node is `if (ok)
 * <straight-line code> else <the fold with that schema>`, so an iso, a
 * hand-written instance, Char, bytes, BigInt and a type met again
 * inside itself all travel exactly as the fold sends them. Decode keeps
 * each fold's totality rules verbatim: an absent field takes its
 * declared default, then None if optional, then the missing refusal.
 */
object Staged {

  /** the staged JSON codec for A: `val c = Staged.json[Order]` */
  def json[A]: JsonCodec[A] = macro StagedMacro.json[A]

  /** the staged CBOR codec for A: `val c = Staged.cbor[Order]` */
  def cbor[A]: CborCodec[A] = macro StagedMacro.cbor[A]

  /** the strict JSON reader for A, generated once at the call site */
  def strict[A]: StrictJsonCodec[A] = macro StagedMacro.strict[A]

  // ---- the construction-time shape checks, shared by every format ----

  /** does this schema have the class's shape (the names, in order)? */
  def productShape(s: Schema[_], names: List[String]): Boolean = s match {
    case p: Schema.SProduct[_] => p.fields.map(_._1).toList == names
    case _ => false
  }
  def sumShape(s: Schema[_], names: List[String]): Boolean = s match {
    case su: Schema.SSum[_] => su.cases.map(_._1).toList == names
    case _ => false
  }

  // ---- run-time helpers the generated JSON code calls ----
  // each leaf answers what the fold answers, and anything else goes to
  // the fold itself, so even a refusal's words are the fold's

  def jInt(j: Json): Either[String, Int] = j match {
    case Json.JNum(n) => Numbers.int(n)
    case got => Json.decode(Schema.SInt)(got)
  }
  def jLong(j: Json): Either[String, Long] = j match {
    case Json.JNum(n) => Numbers.long(n)
    case got => Json.decode(Schema.SLong)(got)
  }
  def jDouble(j: Json): Either[String, Double] = j match {
    case Json.JNum(n) => Right(n)
    case got => Json.decode(Schema.SDouble)(got)
  }
  def jBool(j: Json): Either[String, Boolean] = j match {
    case Json.JBool(b) => Right(b)
    case got => Json.decode(Schema.SBool)(got)
  }
  def jString(j: Json): Either[String, String] = j match {
    case Json.JStr(s) => Right(s)
    case got => Json.decode(Schema.SString)(got)
  }
  def jOption[X](j: Json)(f: Json => Either[String, X]): Either[String, Option[X]] = j match {
    case Json.JNull => Right(None)
    case v => f(v).map(Some(_))
  }
  def jList[X](j: Json, s: Schema[List[X]])(f: Json => Either[String, X]): Either[String, List[X]] = j match {
    case Json.JArr(vs) => elems(vs)(f)
    case got => Json.decode(s)(got)
  }
  def jVector[X](j: Json, s: Schema[Vector[X]])(f: Json => Either[String, X]): Either[String, Vector[X]] = j match {
    case Json.JArr(vs) => elemsV(vs)(f)
    case got => Json.decode(s)(got)
  }
  /** an object's fields, or the fold's answer for any other shape */
  def jObject[T](j: Json, s: Schema[T])(f: Vector[(String, Json)] => Either[String, T]): Either[String, T] = j match {
    case Json.JObj(fs) => f(fs)
    case got => Json.decode(s)(got)
  }
  /** a one-entry object's case name and value, or the fold's answer */
  def jSum[T](j: Json, s: Schema[T])(f: (String, Json) => Either[String, T]): Either[String, T] = j match {
    case Json.JObj(Vector((name, v))) => f(name, v)
    case got => Json.decode(s)(got)
  }
  /** one field: absent (or, for an optional, damaged) takes `absent` */
  def jField[X](fs: Vector[(String, Json)], name: String, optional: Boolean, absent: => Either[String, X])
               (f: Json => Either[String, X]): Either[String, X] =
    lookup(fs, name) match {
      case None => absent
      case Some(Json.JErr(_)) if optional => absent
      case Some(v) => f(v)
    }

  /** a JSON string literal, escaped as the fold escapes it */
  def jText(sb: StringBuilder, s: String): Unit = sb.append('"').append(Json.escape(s)).append('"'): Unit

  /** the field of that name, as the fold's `fs.toMap.get` sees it:
   * toMap keeps the LAST duplicate, so does this */
  def lookup(fs: Vector[(String, Json)], name: String): Option[Json] = {
    var i = 0
    var found: Option[Json] = None
    while (i < fs.length) {
      if (fs(i)._1 == name) found = Some(fs(i)._2)
      i += 1
    }
    found
  }

  /** the fold's list rule: damaged elements are skipped, the rest
   * decode in order, the first Left ends it */
  def elems[X](vs: Vector[Json])(f: Json => Either[String, X]): Either[String, List[X]] = {
    val b = List.newBuilder[X]
    var i = 0
    var failed: Option[String] = None
    while (failed.isEmpty && i < vs.length) {
      vs(i) match {
        case Json.JErr(_) => ()
        case v => f(v) match {
          case Right(x) => b += x
          case Left(e) => failed = Some(e)
        }
      }
      i += 1
    }
    failed.toLeft(b.result())
  }

  def elemsV[X](vs: Vector[Json])(f: Json => Either[String, X]): Either[String, Vector[X]] =
    elems(vs)(f).map(_.toVector)

  // ---- run-time helpers the generated CBOR code calls ----

  /** CBOR's list rule: a sequential read of the declared count, the
   * first Left ends it; a container spends the reader's nesting count */
  def cborElems[X](in: Cbor.In, n: Long)(f: Cbor.In => Either[String, X]): Either[String, List[X]] = {
    in.enter()
    val b = List.newBuilder[X]
    var i = 0L
    var failed: Option[String] = None
    while (failed.isEmpty && i < n) {
      f(in) match {
        case Right(x) => b += x
        case Left(e) => failed = Some(e)
      }
      i += 1
    }
    in.leave()
    failed.toLeft(b.result())
  }

  def cborElemsV[X](in: Cbor.In, n: Long)(f: Cbor.In => Either[String, X]): Either[String, Vector[X]] =
    cborElems(in, n)(f).map(_.toVector)

  def cOption[X](in: Cbor.In)(f: Cbor.In => Either[String, X]): Either[String, Option[X]] =
    if (in.isNull) { in.skipNull(); Right(None) }
    else f(in).map(Some(_))

  /** the one-entry map a sum is written as, by its case name */
  def cborSum[T](in: Cbor.In)(byName: String => Either[String, T]): Either[String, T] =
    in.mapHeader().flatMap {
      case 1 => in.textItem().flatMap(byName)
      case n => Left(s"expected a one-entry map, got $n entries")
    }

  def sOption[X](r: JsonStrict.Reader)(f: JsonStrict.Reader => Either[String, X]): Either[String, Option[X]] =
    if (r.lit("null")) Right(None) else f(r).map(Some(_))

  /** fills absent slots as the fold does — declared default, then
   * None-if-optional, then the refusal — and makes the value */
  private def finish[T](slots: Array[Any], filled: Array[Boolean], absents: Array[Either[String, Any]],
                        make: Array[Any] => T): Either[String, T] = {
    var j = 0
    var missing: Option[String] = None
    while (missing.isEmpty && j < slots.length) {
      if (!filled(j)) absents(j) match {
        case Right(d) => slots(j) = d
        case Left(e) => missing = Some(e)
      }
      j += 1
    }
    missing.toLeft(()).map(_ => make(slots))
  }

  /** a CBOR map carries no field ORDER guarantee, so decode reads `n`
   * (key, value) pairs by NAME, each value at its own staged reader,
   * an unknown field skipped as the fold skips it */
  def cborProduct[T](in: Cbor.In, n: Long, names: Array[String],
                     readers: Array[Cbor.In => Either[String, Any]],
                     absents: Array[Either[String, Any]],
                     make: Array[Any] => T): Either[String, T] = {
    in.enter()
    val slots = new Array[Any](names.length)
    val filled = new Array[Boolean](names.length)
    var i = 0L
    var err: Option[String] = None
    while (err.isEmpty && i < n) {
      in.textItem() match {
        case Left(e) => err = Some(e)
        case Right(k) =>
          val idx = names.indexOf(k)
          if (idx < 0) in.skipItem() match {
            case Left(e) => err = Some(e)
            case Right(()) => ()
          } else readers(idx)(in) match {
            case Left(e) => err = Some(e)
            case Right(v) => slots(idx) = v; filled(idx) = true
          }
      }
      i += 1
    }
    in.leave()
    err match {
      case Some(e) => Left(e)
      case None => finish(slots, filled, absents, make)
    }
  }

  // ---- run-time helpers the generated STRICT JSON code calls ----

  /** `[` v (`,` v)* `]`, each element at its own staged reader */
  def strictElems[X](r: JsonStrict.Reader)(f: JsonStrict.Reader => Either[String, X]): Either[String, List[X]] = {
    r.enter()
    val out = r.expect('[').flatMap { _ =>
      r.skipWs()
      if (r.peek == ']') { r.at += 1; Right(Nil) }
      else {
        val b = List.newBuilder[X]
        var err: Option[String] = None
        var done = false
        while (err.isEmpty && !done) {
          r.skipWs()
          f(r) match {
            case Left(e) => err = Some(e)
            case Right(x) =>
              b += x
              r.skipWs()
              r.peek match {
                case ',' => r.at += 1
                case ']' => r.at += 1; done = true
                case _ => err = r.fail[Unit]("expected ',' or ']'").swap.toOption
              }
          }
        }
        err.toLeft(b.result())
      }
    }
    r.leave()
    out
  }

  def strictElemsV[X](r: JsonStrict.Reader)(f: JsonStrict.Reader => Either[String, X]): Either[String, Vector[X]] =
    strictElems(r)(f).map(_.toVector)

  /** `{` "k" `:` v ... `}` by NAME into slots — an undeclared field is
   * SKIPPED — then absences filled as the fold fills them */
  def strictProduct[T](r: JsonStrict.Reader, names: Array[String],
                       readers: Array[JsonStrict.Reader => Either[String, Any]],
                       absents: Array[Either[String, Any]],
                       make: Array[Any] => T): Either[String, T] = {
    r.enter()
    val out = r.expect('{').flatMap { _ =>
      val slots = new Array[Any](names.length)
      val filled = new Array[Boolean](names.length)
      var err: Option[String] = None
      r.skipWs()
      if (r.peek == '}') r.at += 1
      else {
        var done = false
        while (err.isEmpty && !done) {
          r.skipWs()
          r.string() match {
            case Left(e) => err = Some(e)
            case Right(k) =>
              r.skipWs()
              r.expect(':') match {
                case Left(e) => err = Some(e)
                case Right(_) =>
                  r.skipWs()
                  val idx = names.indexOf(k)
                  val got: Either[String, Unit] =
                    if (idx < 0) r.skipValue()
                    else readers(idx)(r).map { v => slots(idx) = v; filled(idx) = true }
                  got match {
                    case Left(e) => err = Some(e)
                    case Right(_) =>
                      r.skipWs()
                      r.peek match {
                        case ',' => r.at += 1
                        case '}' => r.at += 1; done = true
                        case _ => err = r.fail[Unit]("expected ',' or '}'").swap.toOption
                      }
                  }
              }
          }
        }
      }
      err match {
        case Some(e) => Left(e)
        case None => finish(slots, filled, absents, make)
      }
    }
    r.leave()
    out
  }

  /** the one-entry object a sum is written as: `{"Case": value}` */
  def strictSum[T](r: JsonStrict.Reader)(byName: String => Either[String, T]): Either[String, T] = {
    r.enter()
    val out = r.expect('{').flatMap { _ =>
      r.skipWs()
      r.string().flatMap { name =>
        r.skipWs()
        r.expect(':').flatMap { _ =>
          r.skipWs()
          byName(name).flatMap { v => r.skipWs(); r.expect('}').map(_ => v) }
        }
      }
    }
    r.leave()
    out
  }
}
