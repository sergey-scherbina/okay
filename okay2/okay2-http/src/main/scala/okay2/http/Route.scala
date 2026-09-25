package okay2.http

import java.nio.charset.StandardCharsets.UTF_8
import scala.language.experimental.macros
import okay2.Optic
import okay2.codec.Json

/**
 * A route: one declaration, three interpreters (okay-http's Route.scala,
 * specs/optics-outside.md stage 1).
 *
 *  - MATCH: `unapply`, so a server writes `case Get(userPost(id, slug))`
 *    and gets an `Int` and a `String`;
 *  - BUILD: `url`, the path from the SAME declaration the server matches,
 *    so `unapply(url(a)) == Some(a)` is a law about a program;
 *  - DESCRIBE: `describe`, `params`, `queries`, with no request at all.
 *
 * `A` is what the route captures, in its outside form (`Captures`):
 * `Unit`, one value, or a tuple. A url is a path and THEN a query: `:?`
 * answers a `Queried`, which has no `/`, so a segment after a query
 * parameter does not compile.
 */
sealed trait Routed[A] {

  val segments: Vector[Route.Seg]
  val queries: Vector[Route.Q]
  private[http] val decode: (Vector[String], Route.Params) => Option[A]
  private[http] val encode: A => (Vector[String], Vector[(String, String)])

  /** MATCH, and the extractor */
  def unapply(url: String): Option[A] =
    Route.segmentsOf(url).filter(_.length == segments.length).flatMap(ss => Route.paramsOf(url).flatMap(decode(ss, _)))

  /** BUILD: the query written in DECLARATION order, the canonical url
   * among the many a route accepts, which is why this is a prism and not
   * an iso */
  def url(a: A): String = {
    val (segs, qp) = encode(a)
    val path = segs.map(Route.encodeSeg).mkString("/", "/", "")
    if (qp.isEmpty) path
    else path + qp.map { case (k, v) => Route.encodeSeg(k) + "=" + Route.encodeSeg(v) }.mkString("?", "&", "")
  }

  /** DESCRIBE, the path: `/users/{id}/posts/{slug}` */
  def describe: String =
    segments.map {
      case Route.Seg.Lit(v) => v
      case v: Route.Seg.Var => "{" + v.name + "}"
    }.mkString("/", "/", "")

  /** the whole thing, for a human: `/search?q={q}&page={page}` */
  def describeFull: String =
    if (queries.isEmpty) describe
    else describe + queries.map(q => q.name + "={" + q.name + "}").mkString("?", "&", "")

  /** the path parameters, in order */
  def params: Vector[Route.Seg.Var] = segments.collect { case v: Route.Seg.Var => v }

  /** the description as ONE value: what a renderer is given */
  def described: Route.Described = Route.Described(describe, params, queries)

  /** a REQUEST HEADER, declared: the result is not a `Routed`, because a
   * header cannot join `A` without breaking `unapply(url(a)) == Some(a)` */
  def :@[B](q: Query[B]): Headed[A, B] =
    new Headed[A, B](this, q.declared.map(Route.headerOf), q.declared.map(_.name), q.decode)

  def :@[T](n: Route.Named[T]): Headed[A, T] = this :@ Query[T](n.name)(n.param)

  /** what this route requires of a caller: read from the REQUEST, so it
   * produces a `Headed`, as `:@` does */
  def secured(scopes: String*): Headed[A, Unit] =
    new Headed[A, Unit](this, Vector.empty, Vector.empty, _ => Some(()), Vector(Route.Security(scopes = scopes.toSet)))

  def securedBy(s: Route.Security): Headed[A, Unit] =
    new Headed[A, Unit](this, Vector.empty, Vector.empty, _ => Some(()), Vector(s))

  /** the bridge to the core optics: a route IS a prism `String <-> A` */
  def prism: Optic.Prism[String, String, A, A] = Optic.Prism[String, String, A, A](s => unapply(s).toRight(s), url)

  /**
   * The same route over a case class instead of the capture: positional,
   * the TYPES checked by the macro at compile time (Scala 3: by the
   * Mirror), the NAMES — path parameters, then query parameters, against
   * the fields — checked at construction, refused on a mismatch.
   */
  def of[C]: Route.Of[C, A] = macro RouteMacro.of[C, A]
}

/** the PATH stage: segments may still be added, and a query may start */
final class Route[A] private[http] (
    val segments: Vector[Route.Seg],
    val queries: Vector[Route.Q],
    private[http] val decode: (Vector[String], Route.Params) => Option[A],
    private[http] val encode: A => (Vector[String], Vector[(String, String)]))
  extends Routed[A] {

  /** a literal segment */
  def /(lit: String): Route[A] =
    new Route[A](segments :+ Route.Seg.Lit(lit), queries,
      (ss, qs) => if (ss.nonEmpty && ss.last == lit) decode(ss.init, qs) else None,
      a => { val (segs, qp) = encode(a); (segs :+ lit, qp) })

  /** another route's segments after these, the captures joined */
  def /[B](that: Route[B])(implicit c: Split[A, B]): Route[c.Out] = {
    val width = segments.length
    new Route[c.Out](segments ++ that.segments, queries ++ that.queries,
      (ss, qs) =>
        if (ss.length != width + that.segments.length) None
        else {
          val (l, r) = ss.splitAt(width)
          decode(l, qs).flatMap(a => that.decode(r, qs).map(b => c.join(a, b)))
        },
      o => {
        val (a, b) = c.split(o)
        val (ls, lq) = encode(a)
        val (rs, rq) = that.encode(b)
        (ls ++ rs, lq ++ rq)
      })
  }

  def /[T](n: Route.Named[T])(implicit c: Split[A, T]): Route[c.Out] = this / Route[T](n.name)(n.param)

  /** the query starts: no segment can follow */
  def :?[B](q: Query[B])(implicit c: Split[A, B]): Queried[c.Out] = Route.queried(this, q)

  def :?[T](n: Route.Named[T])(implicit c: Split[A, T]): Queried[c.Out] = this :? Query[T](n.name)(n.param)
}

/** the QUERY stage: more query parameters, no more segments */
final class Queried[A] private[http] (
    val segments: Vector[Route.Seg],
    val queries: Vector[Route.Q],
    private[http] val decode: (Vector[String], Route.Params) => Option[A],
    private[http] val encode: A => (Vector[String], Vector[(String, String)]))
  extends Routed[A] {

  def :?[B](q: Query[B])(implicit c: Split[A, B]): Queried[c.Out] = Route.queried(this, q)

  def :?[T](n: Route.Named[T])(implicit c: Split[A, T]): Queried[c.Out] = this :? Query[T](n.name)(n.param)
}

/** a url and the request headers it reads: the request-shaped
 * declaration, a different type from the url's prism on purpose */
final class Headed[A, Hs] private[http] (
    val route: Routed[A],
    val headers: Vector[Route.Hdr],
    private[http] val names: Vector[String],
    private[http] val decodeHeaders: Route.Params => Option[Hs],
    val security: Vector[Route.Security] = Vector.empty) {

  def :@[B](q: Query[B])(implicit c: Split[Hs, B]): Headed[A, c.Out] =
    new Headed[A, c.Out](route, headers ++ q.declared.map(Route.headerOf), names ++ q.declared.map(_.name),
      hs => decodeHeaders(hs).flatMap(a => q.decode(hs).map(b => c.join(a, b))), security)

  def :@[T](n: Route.Named[T])(implicit c: Split[Hs, T]): Headed[A, c.Out] = this :@ Query[T](n.name)(n.param)

  def secured(scopes: String*): Headed[A, Hs] =
    new Headed[A, Hs](route, headers, names, decodeHeaders, security :+ Route.Security(scopes = scopes.toSet))

  def securedBy(s: Route.Security): Headed[A, Hs] = new Headed[A, Hs](route, headers, names, decodeHeaders, security :+ s)

  /** the declared headers, case-insensitively, a repeated one kept whole */
  def readHeaders(r: Request): Option[Hs] = {
    val lower = r.headers.map { case (k, v) => (k.toLowerCase, v) }
    val m: Route.Params = names.map { n =>
      val ln = n.toLowerCase
      n -> lower.collect { case (k, v) if k == ln => v }.toVector
    }.toMap
    decodeHeaders(m)
  }

  def read(r: Request): Option[(A, Hs)] = route.unapply(r.url).flatMap(a => readHeaders(r).map(h => (a, h)))

  def describe: String = route.describe
  def described: Route.Described = route.described.copy(headers = headers, security = security)
}

object Route {

  sealed trait Seg
  object Seg {
    final case class Lit(value: String) extends Seg
    final case class Var(name: String, kind: String, schema: Json = Param.schemaOf("string")) extends Seg
  }

  final case class Q(name: String, kind: String, required: Boolean, repeated: Boolean, schema: Json = Param.schemaOf("string"))

  final case class Hdr(name: String, kind: String, required: Boolean, repeated: Boolean, schema: Json = Param.schemaOf("string"))

  private[http] def headerOf(q: Q): Hdr = Hdr(q.name, q.kind, q.required, q.repeated, q.schema)

  /** what a route requires of a caller: a bearer credential with scopes */
  final case class Security(scheme: String = "bearer", scopes: Set[String] = Set.empty, realm: String = "okay")

  final case class Described(path: String, params: Vector[Seg.Var], queries: Vector[Q],
                             headers: Vector[Hdr] = Vector.empty, security: Vector[Security] = Vector.empty)

  /** a typed name: `"id".as[Int]` */
  final case class Named[T](name: String, param: Param[T])

  type Params = Map[String, Vector[String]]

  /** how one value is read from and written to a segment or a parameter */
  trait Param[T] {
    def kind: String
    def parse(s: String): Option[T]
    def print(t: T): String
    def jsonSchema: Json = Param.schemaOf(kind)
  }

  object Param {
    def arrayOf[T](p: Param[T]): Json = Json.JObj(Vector("type" -> Json.JStr("array"), "items" -> p.jsonSchema))

    /** the JSON Schema of a kind: an integer, a boolean, else a string */
    def schemaOf(kind: String): Json = kind match {
      case "int" | "long" => Json.JObj(Vector("type" -> Json.JStr("integer")))
      case "boolean" => Json.JObj(Vector("type" -> Json.JStr("boolean")))
      case _ => Json.JObj(Vector("type" -> Json.JStr("string")))
    }

    implicit val string: Param[String] = new Param[String] {
      def kind = "string"
      def parse(s: String): Option[String] = Some(s)
      def print(t: String): String = t
    }

    implicit val int: Param[Int] = new Param[Int] {
      def kind = "int"
      def parse(s: String): Option[Int] = s.toIntOption
      def print(t: Int): String = t.toString
    }

    implicit val long: Param[Long] = new Param[Long] {
      def kind = "long"
      def parse(s: String): Option[Long] = s.toLongOption
      def print(t: Long): String = t.toString
    }

    implicit val boolean: Param[Boolean] = new Param[Boolean] {
      def kind = "boolean"
      def parse(s: String): Option[Boolean] = s match {
        case "true" => Some(true)
        case "false" => Some(false)
        case _ => None
      }
      def print(t: Boolean): String = t.toString
    }
  }

  def /(lit: String): Route[Unit] = root / lit

  def /[B](r: Route[B]): Route[B] = r

  /** the empty path */
  val root: Route[Unit] =
    new Route[Unit](Vector.empty, Vector.empty, (ss, _) => if (ss.isEmpty) Some(()) else None, _ => (Vector.empty, Vector.empty))

  def lit(s: String): Route[Unit] = root / s

  /** one captured segment, parsed by its Param; an empty one refused */
  def apply[T](name: String)(implicit p: Param[T]): Route[T] =
    new Route[T](Vector(Seg.Var(name, p.kind, p.jsonSchema)), Vector.empty,
      (ss, _) => ss match {
        case Vector(one) if one.nonEmpty => p.parse(one)
        case _ => None
      },
      t => (Vector(p.print(t)), Vector.empty))

  private[http] def queried[A, B](r: Routed[A], q: Query[B])(implicit c: Split[A, B]): Queried[c.Out] =
    new Queried[c.Out](r.segments, r.queries ++ q.declared,
      (ss, qs) => r.decode(ss, qs).flatMap(a => q.decode(qs).map(b => c.join(a, b))),
      o => {
        val (a, b) = c.split(o)
        val (segs, qp) = r.encode(a)
        (segs, qp ++ q.encode(b))
      })

  object Of {
    /** the names, path then query, against the fields: refused at
     * construction (a route is a val, so at start-up) */
    def checked[C, A](r: Routed[A], make: A => C, parts: C => A, labels: Vector[String]): Of[C, A] = {
      val names = r.params.map(_.name) ++ r.queries.map(_.name)
      require(names == labels,
        s"route parameters ${names.mkString("(", ", ", ")")} do not match the field names ${labels.mkString("(", ", ", ")")}: " +
          "the mapping is positional, so the types already agree — rename one side so the declaration says what it means")
      new Of(r, make, parts)
    }
  }

  final class Of[C, A] private[http] (private val r: Routed[A], make: A => C, parts: C => A) {
    def unapply(path: String): Option[C] = r.unapply(path).map(make)
    def url(c: C): String = r.url(parts(c))
    def describe: String = r.describe
    def describeFull: String = r.describeFull
    def segments: Vector[Seg] = r.segments
    def queries: Vector[Q] = r.queries
    def described: Described = r.described
    def prism: Optic.Prism[String, String, C, C] = Optic.Prism[String, String, C, C](s => unapply(s).toRight(s), url)
  }

  /** the path's segments, each decoded after the split: a `%2F` inside a
   * parameter is not a boundary */
  private[http] def segmentsOf(path: String): Option[Vector[String]] = {
    val p0 = pathOf(path)
    val p = if (p0.startsWith("/")) p0.substring(1) else p0
    if (p.isEmpty) Some(Vector.empty)
    else {
      val out = p.split("/", -1).toVector.map(decodeSeg)
      if (out.forall(_.isDefined)) Some(out.map(_.get)) else None
    }
  }

  private[http] def pathOf(url: String): String = {
    val cut = Seq(url.indexOf('?'), url.indexOf('#')).filter(_ >= 0)
    if (cut.isEmpty) url else url.substring(0, cut.min)
  }

  private[http] def paramsOf(url: String): Option[Params] = {
    val q = url.indexOf('?')
    if (q < 0) Some(Map.empty)
    else {
      val body = url.substring(q + 1)
      val frag = body.indexOf('#')
      val text = if (frag >= 0) body.substring(0, frag) else body
      if (text.isEmpty) Some(Map.empty)
      else {
        val pairs = text.split("&", -1).toVector.filter(_.nonEmpty).map { kv =>
          val i = kv.indexOf('=')
          val (k, v) = if (i < 0) (kv, "") else (kv.substring(0, i), kv.substring(i + 1))
          decodeParam(k).flatMap(dk => decodeParam(v).map(dv => (dk, dv)))
        }
        if (pairs.forall(_.isDefined)) Some(pairs.map(_.get).groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) })
        else None
      }
    }
  }

  /** a query value: `+` is a space, then percent-decoding */
  private[http] def decodeParam(s: String): Option[String] = decodeSeg(if (s.indexOf('+') < 0) s else s.replace('+', ' '))

  private def hex(c: Char): Int =
    if (c >= '0' && c <= '9') c - '0'
    else if (c >= 'a' && c <= 'f') c - 'a' + 10
    else if (c >= 'A' && c <= 'F') c - 'A' + 10
    else -1

  private val hexDigits = "0123456789ABCDEF"

  /** RFC 3986's unreserved set passes, every other byte of the UTF-8 is
   * escaped */
  private[http] def encodeSeg(s: String): String = {
    val bs = s.getBytes(UTF_8)
    val sb = new StringBuilder(bs.length)
    var i = 0
    while (i < bs.length) {
      val b = bs(i) & 0xff
      val c = b.toChar
      if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-' || c == '.' || c == '_' || c == '~')
        sb.append(c)
      else {
        sb.append('%')
        sb.append(hexDigits.charAt(b >> 4))
        sb.append(hexDigits.charAt(b & 0xf))
      }
      i += 1
    }
    sb.toString
  }

  /** percent-decoding; malformed escaping is None, not a literal `%` */
  private[http] def decodeSeg(s: String): Option[String] =
    if (s.indexOf('%') < 0) Some(s)
    else {
      val out = scala.collection.mutable.ArrayBuilder.make[Byte]
      var i = 0
      var ok = true
      while (i < s.length && ok) {
        if (s.charAt(i) == '%') {
          if (i + 2 < s.length) {
            val h = hex(s.charAt(i + 1))
            val l = hex(s.charAt(i + 2))
            if (h < 0 || l < 0) ok = false
            else { out.addOne(((h << 4) | l).toByte); i += 3 }
          } else ok = false
        } else {
          // the whole run up to the next escape at once, so a surrogate
          // pair keeps its two halves together
          val j = s.indexOf('%', i)
          val end = if (j < 0) s.length else j
          out.addAll(s.substring(i, end).getBytes(UTF_8))
          i = end
        }
      }
      if (ok) Some(new String(out.result(), UTF_8)) else None
    }
}

/** query parameters (or, after `:@`, headers) by name */
final class Query[B] private[http] (
    val declared: Vector[Route.Q],
    private[http] val decode: Route.Params => Option[B],
    private[http] val encode: B => Vector[(String, String)]) {

  def +&[C](that: Query[C])(implicit c: Split[B, C]): Query[c.Out] =
    new Query[c.Out](declared ++ that.declared,
      qs => decode(qs).flatMap(b => that.decode(qs).map(d => c.join(b, d))),
      o => { val (b, d) = c.split(o); encode(b) ++ that.encode(d) })
}

object Query {

  /** required: absent or unparseable is a miss */
  def apply[T](name: String)(implicit p: Route.Param[T]): Query[T] =
    new Query[T](Vector(Route.Q(name, p.kind, required = true, repeated = false, p.jsonSchema)),
      qs => qs.get(name).flatMap(_.headOption).flatMap(p.parse),
      t => Vector(name -> p.print(t)))

  /** optional: absent is None, present and unparseable is a miss */
  def opt[T](name: String)(implicit p: Route.Param[T]): Query[Option[T]] =
    new Query[Option[T]](Vector(Route.Q(name, p.kind, required = false, repeated = false, p.jsonSchema)),
      qs => qs.get(name).flatMap(_.headOption) match {
        case None => Some(None)
        case Some(v) => p.parse(v).map(Some(_))
      },
      t => t.map(x => name -> p.print(x)).toVector)

  /** repeated: every value, in wire order */
  def all[T](name: String)(implicit p: Route.Param[T]): Query[Vector[T]] =
    new Query[Vector[T]](Vector(Route.Q(name, p.kind, required = false, repeated = true, Route.Param.arrayOf(p))),
      qs => {
        val parsed = qs.getOrElse(name, Vector.empty).map(p.parse)
        if (parsed.forall(_.isDefined)) Some(parsed.map(_.get)) else None
      },
      t => t.map(x => name -> p.print(x)))
}

/** the terse spelling: `"id".as[Int]`, `"page".opt[Int]`, `"tag".all[String]` */
object syntax {
  implicit final class NameOps(private val name: String) extends AnyVal {
    def as[T](implicit p: Route.Param[T]): Route.Named[T] = Route.Named(name, p)
    def opt[T](implicit p: Route.Param[T]): Query[Option[T]] = Query.opt[T](name)
    def all[T](implicit p: Route.Param[T]): Query[Vector[T]] = Query.all[T](name)
  }
}

/** the verbs as extractors: `case Get(userPost(id, slug))` */
object Get { def unapply(r: Request): Option[String] = if (r.method == Method.Get) Some(r.url) else None }
object Post { def unapply(r: Request): Option[String] = if (r.method == Method.Post) Some(r.url) else None }
object Put { def unapply(r: Request): Option[String] = if (r.method == Method.Put) Some(r.url) else None }
object Patch { def unapply(r: Request): Option[String] = if (r.method == Method.Patch) Some(r.url) else None }
object Delete { def unapply(r: Request): Option[String] = if (r.method == Method.Delete) Some(r.url) else None }
