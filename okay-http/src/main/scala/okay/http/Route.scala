package okay.http

import okay.*
import scala.deriving.Mirror
import java.nio.charset.StandardCharsets.UTF_8

/**
 * A route: one declaration, three interpreters (specs/optics-outside.md,
 * stage 1).
 *
 * This is the first piece of the arc that puts optics on the OUTSIDE —
 * as the vocabulary a user of the library writes, rather than as an
 * implementation of ours. The test that arc sets is a single one: a
 * declaration earns an optic only when it must be handed to more than
 * one interpreter and at least one of them DESCRIBES it instead of
 * running it. A path passes that test three times over, which is why
 * it goes first:
 *
 *   - MATCH — `unapply`, so a server writes `case Get(userPost(id, slug))`
 *     and gets an `Int` and a `String`, not a `Map[String, String]`;
 *   - BUILD — `url`, so the client, the redirect and the test build the
 *     path from the SAME declaration the server matches with, which is
 *     what makes `unapply(url(a)) == Some(a)` a law about a program
 *     rather than about a pair of string literals somebody kept in sync;
 *   - DESCRIBE — `describe` and `segments`, which need no request at
 *     all, and are what OpenAPI or an MCP tool declaration is generated
 *     from.
 *
 * `prism` is the bridge back to `specs/optics.md`: for a fixed `A` a
 * route IS a prism `String <-> A`, so it composes with the core optics
 * and its law is the prism law the repository already tests.
 *
 * What is in the tree today is the argument for it. Every `routes` in
 * this repository is `case r if r.method == Get && r.url == "/healthz"`
 * (okay-ops, okay-admin, okay-acme, okay-demo): not one route carries a
 * typed parameter. The one router that does have parameters —
 * okay-script's `Site.resolve` — hands them back as an untyped
 * `Map[String, String]` and splits on `/` BEFORE decoding, so a `%2F`
 * inside a parameter becomes a segment boundary. `Route` decodes per
 * segment, after the split, and that class of confusion cannot happen.
 */
final class Route[A <: Tuple] private[http] (
    /** the description: what this route looks like, with no request in hand */
    val segments: Vector[Route.Seg],
    /** the query half of the description, in declaration order */
    val queries: Vector[Route.Q],
    private[http] val decode: (Vector[String], Route.Params) => Option[A],
    private[http] val encode: A => (Vector[String], Vector[(String, String)])):

  /** append a literal segment */
  def /(lit: String): Route[A] =
    new Route(segments :+ Route.Seg.Lit(lit), queries,
      (ss, qs) =>
        if ss.nonEmpty && ss.last == lit then decode(ss.init, qs) else None,
      a =>
        val (segs, qp) = encode(a)
        (segs :+ lit, qp))

  /**
   * append another route, accumulating its parameters.
   *
   * The witness is a typeclass and not `scala.Tuple.Concat` on
   * purpose, and its name says the whole difference: the standard
   * library gives the forward direction (the match type, and `++` for
   * the values) and nothing that takes a tuple APART again. `url`
   * needs `A` and `B` back out of `c.Out`. Closing that with an
   * `asInstanceOf` is what AGENTS.md forbids, and the inductive
   * witness carries both directions honestly.
   */
  def /[B <: Tuple](that: Route[B])(using c: Route.Split[A, B]): Route[c.Out] =
    val width = segments.length
    new Route[c.Out](segments ++ that.segments, queries ++ that.queries,
      (ss, qs) =>
        if ss.length != width + that.segments.length then None
        else
          val (l, r) = ss.splitAt(width)
          decode(l, qs).flatMap(a => that.decode(r, qs).map(b => c.join(a, b))),
      o =>
        val (a, b) = c.split(o)
        val (ls, lq) = encode(a)
        val (rs, rq) = that.encode(b)
        (ls ++ rs, lq ++ rq))

  /**
   * append the query string this route reads.
   *
   * Written as one `Query` rather than a parameter at a time, so the
   * split between the path's tuple and the query's is exactly one
   * `Split`, and the path's own parameters keep their positions.
   *
   * `:?` and not `?`, and the reason is Scala's precedence table
   * rather than taste. An operator's precedence comes from its FIRST
   * character, and `?` is in the "all other special characters" group,
   * which binds TIGHTER than `/` — so `Route / "search" ? q` parses as
   * `Route / ("search" ? q)` and does not compile. `:` is below `/`,
   * so `:?` groups the way it reads; and since associativity comes
   * from the LAST character, `:?` stays left-associative. `Query`'s
   * `+&` sits between them (the `+ -` group), so a whole declaration
   * needs no parentheses at all. These are http4s's operators, and
   * this is why they are the ones they are.
   */
  def :?[B <: Tuple](q: Query[B])(using c: Route.Split[A, B]): Route[c.Out] =
    new Route[c.Out](segments, queries ++ q.declared,
      (ss, qs) => decode(ss, qs).flatMap(a => q.decode(qs).map(b => c.join(a, b))),
      o =>
        val (a, b) = c.split(o)
        val (segs, qp) = encode(a)
        (segs, qp ++ q.encode(b)))

  /** interpreter 1 — MATCH. Also the extractor. */
  def unapply(url: String): Option[A] =
    Route.segmentsOf(url).filter(_.length == segments.length)
      .flatMap(ss => Route.paramsOf(url).flatMap(decode(ss, _)))

  /**
   * interpreter 2 — BUILD. Reverse routing from the same declaration.
   *
   * The query is written in DECLARATION order, which makes this the
   * canonical url among the many a route accepts — a query string is
   * unordered on the wire and `unapply` reads it from a map. That is
   * also the moment a route stops resembling an iso and is plainly a
   * prism: `unapply(url(a)) == Some(a)` holds, and the other direction
   * does not, because `url(unapply(u))` normalises `u`.
   */
  def url(a: A): String =
    val (segs, qp) = encode(a)
    val path = segs.map(Route.encodeSeg).mkString("/", "/", "")
    if qp.isEmpty then path
    else path + qp.map((k, v) => Route.encodeSeg(k) + "=" + Route.encodeSeg(v)).mkString("?", "&", "")

  /** interpreter 3 — DESCRIBE, the path. `/users/{id}/posts/{slug}` —
   * the shape OpenAPI wants, where query parameters are a separate
   * list rather than part of the template. */
  def describe: String =
    segments.map {
      case Route.Seg.Lit(v) => v
      case Route.Seg.Var(n, _) => "{" + n + "}"
    }.mkString("/", "/", "")

  /** the whole thing, for a human: `/search?q={q}&page={page}` */
  def describeFull: String =
    if queries.isEmpty then describe
    else describe + queries.map(q => q.name + "={" + q.name + "}").mkString("?", "&", "")

  /** the path parameters this route captures, in order — what a
   * generated OpenAPI operation or MCP tool schema is built from */
  def params: Vector[Route.Seg.Var] =
    segments.collect { case v: Route.Seg.Var => v }

  /**
   * the bridge to `specs/optics.md`. A miss leaves the path unchanged,
   * which is exactly `Prism`'s `Either[T, A]`, and `review` is `url`.
   */
  def prism: Prism[String, String, A, A] =
    Prism(s => unapply(s).toRight(s), url)

  /**
   * the same route, reading and writing a case class instead of a
   * tuple. This is the form worth using once a route has parameters:
   * `case Get(userPost(p)) => p.id` beats positional tuples, and one
   * extractor result is unambiguous at every arity.
   */
  def of[C <: Product](using m: Mirror.ProductOf[C] { type MirroredElemTypes = A }): Route.Of[C, A] =
    new Route.Of(this, m)

object Route:

  /** a segment of the description */
  enum Seg:
    case Lit(value: String)
    case Var(name: String, kind: String)

  /** a query parameter of the description */
  final case class Q(name: String, kind: String, required: Boolean, repeated: Boolean)

  /** a parsed query string: every value for every key, in wire order */
  type Params = Map[String, Vector[String]]

  /**
   * a captured segment's codec — the only thing a new parameter type
   * has to supply, and it is three lines.
   */
  trait Param[T]:
    def kind: String
    def parse(s: String): Option[T]
    def print(t: T): String

  object Param:
    given string: Param[String] with
      def kind = "string"
      def parse(s: String): Option[String] = Some(s)
      def print(t: String): String = t

    given int: Param[Int] with
      def kind = "int"
      def parse(s: String): Option[Int] = s.toIntOption
      def print(t: Int): String = t.toString

    given long: Param[Long] with
      def kind = "long"
      def parse(s: String): Option[Long] = s.toLongOption
      def print(t: Long): String = t.toString

    given boolean: Param[Boolean] with
      def kind = "boolean"
      def parse(s: String): Option[Boolean] = s match
        case "true" => Some(true)
        case "false" => Some(false)
        case _ => None
      def print(t: Boolean): String = t.toString

  /**
   * The witness that `A` and `B` concatenate — and, unlike
   * `scala.Tuple.Concat`, come apart again. `split` is the whole
   * addition: `join` could have been the standard library's `++`.
   *
   * Two instances, by induction on the LEFT tuple, so the chain of
   * instances is exactly as long as `A` — which is how "where to cut"
   * gets answered without counting anything at run time, and why no
   * invariant has to be maintained by hand.
   *
   * `Out = H *: c.Out` also NORMALISES at every step, and that is the
   * deeper reason not to reuse the standard library's type. With
   * `Out = Tuple.Concat[A, B]` the induction works once and then the
   * accumulated type is itself a `Tuple.Concat[A1, A2]`, a stuck match
   * type when its arguments are abstract, with no head to peel — so
   * the next `/` in the chain cannot resolve. A cons chain always can.
   *
   * The refinement in each given's declared TYPE (`{ type Out = ... }`)
   * is load-bearing, not decoration: without it `c.Out` stays abstract
   * at the call site and a user sees `Route[c.Out]` instead of
   * `Route[(Int, String)]`.
   */
  trait Split[A <: Tuple, B <: Tuple]:
    type Out <: Tuple
    def join(a: A, b: B): Out
    def split(o: Out): (A, B)

  object Split:
    given empty[B <: Tuple]: (Split[EmptyTuple, B] { type Out = B }) =
      new Split[EmptyTuple, B]:
        type Out = B
        def join(a: EmptyTuple, b: B): B = b
        def split(o: B): (EmptyTuple, B) = (EmptyTuple, o)

    given cons[H, T <: Tuple, B <: Tuple](using c: Split[T, B])
        : (Split[H *: T, B] { type Out = H *: c.Out }) =
      new Split[H *: T, B]:
        type Out = H *: c.Out
        def join(a: H *: T, b: B): H *: c.Out = a match
          case h *: t => h *: c.join(t, b)
        def split(o: H *: c.Out): (H *: T, B) = o match
          case h *: rest =>
            val (t, b) = c.split(rest)
            (h *: t, b)

  /** start a route from the companion: `Route / "users" / Route[Int]("id")` */
  def /(lit: String): Route[EmptyTuple] = root / lit

  /** the same, when the first segment is a captured one */
  def /[B <: Tuple](r: Route[B]): Route[B] = r

  /** the empty path, `"/"` */
  val root: Route[EmptyTuple] =
    new Route(Vector.empty, Vector.empty,
      (ss, _) => if ss.isEmpty then Some(EmptyTuple) else None,
      _ => (Vector.empty, Vector.empty))

  /** one literal segment */
  def lit(s: String): Route[EmptyTuple] = root / s

  /**
   * one captured segment.
   *
   * An EMPTY segment is refused here, before the `Param` sees it, and
   * that is deliberate placement: a path segment is not a value when
   * it is empty, because `"/x//y"` and `"/x/y"` would build the same
   * url from different parameters. The constraint belongs to the
   * POSITION and not to the type — `?tag=` is a perfectly good empty
   * value in a query string, and `Param.string` accepting it is what
   * lets `Query.all[String]` round-trip one. Attaching the rule to
   * `Param` instead was the first cut, and it broke the query the day
   * the query arrived.
   */
  def apply[T](name: String)(using p: Param[T]): Route[T *: EmptyTuple] =
    new Route(Vector(Seg.Var(name, p.kind)), Vector.empty,
      (ss, _) => ss match
        case Vector(one) if one.nonEmpty => p.parse(one).map(_ *: EmptyTuple)
        case _ => None,
      t => (Vector(p.print(t.head)), Vector.empty))

  /** a route read as a case class rather than a tuple */
  final class Of[C <: Product, A <: Tuple] private[http] (
      private val r: Route[A],
      private val m: Mirror.ProductOf[C] { type MirroredElemTypes = A }):
    def unapply(path: String): Option[C] = r.unapply(path).map(m.fromProduct)
    def url(c: C): String = r.url(Tuple.fromProductTyped(c)(using m))
    def describe: String = r.describe
    def describeFull: String = r.describeFull
    def segments: Vector[Seg] = r.segments
    def queries: Vector[Q] = r.queries
    def prism: Prism[String, String, C, C] = Prism(s => unapply(s).toRight(s), url)

  /**
   * A path's segments, percent-decoded AFTER the split so an encoded
   * `/` can never be read as a boundary. A query string is not part of
   * the path (stage 2), and malformed escaping is a MISS rather than a
   * silent literal `%`.
   */
  private[http] def segmentsOf(path: String): Option[Vector[String]] =
    val p0 = pathOf(path)
    val p = if p0.startsWith("/") then p0.substring(1) else p0
    if p.isEmpty then Some(Vector.empty)
    else
      val raw = p.split("/", -1).toVector
      val out = raw.map(decodeSeg)
      if out.forall(_.isDefined) then Some(out.map(_.get)) else None

  /** the path half of a url — everything before `?` or `#` */
  private[http] def pathOf(url: String): String =
    val cut = Seq(url.indexOf('?'), url.indexOf('#')).filter(_ >= 0)
    if cut.isEmpty then url else url.substring(0, cut.min)

  /**
   * The query string, as every value for every key.
   *
   * A key with no `=` reads as the empty value, a repeated key keeps
   * all of its values in wire order, and malformed escaping is a MISS
   * for the same reason it is in a path segment.
   */
  private[http] def paramsOf(url: String): Option[Params] =
    val q = url.indexOf('?')
    if q < 0 then Some(Map.empty)
    else
      val body = url.substring(q + 1)
      val frag = body.indexOf('#')
      val text = if frag >= 0 then body.substring(0, frag) else body
      if text.isEmpty then Some(Map.empty)
      else
        val pairs = text.split("&", -1).toVector.filter(_.nonEmpty).map { kv =>
          val i = kv.indexOf('=')
          val (k, v) = if i < 0 then (kv, "") else (kv.substring(0, i), kv.substring(i + 1))
          decodeParam(k).flatMap(dk => decodeParam(v).map(dv => (dk, dv)))
        }
        if pairs.forall(_.isDefined) then
          Some(pairs.map(_.get).groupBy(_._1).map((k, vs) => k -> vs.map(_._2)))
        else None

  /**
   * A query value decodes like a segment, except that a raw `+` is a
   * space — the form-encoding convention every browser writes and
   * every server reads. `url` never emits one, because `+` is not
   * unreserved and comes out as `%2B`, so the round trip is not
   * affected either way.
   */
  private[http] def decodeParam(s: String): Option[String] =
    decodeSeg(if s.indexOf('+') < 0 then s else s.replace('+', ' '))

  private def hex(c: Char): Int =
    if c >= '0' && c <= '9' then c - '0'
    else if c >= 'a' && c <= 'f' then c - 'a' + 10
    else if c >= 'A' && c <= 'F' then c - 'A' + 10
    else -1

  private val hexDigits = "0123456789ABCDEF"

  /** unreserved characters through, everything else as UTF-8 `%XX` —
   * a SEGMENT encoder, so it is not `URLEncoder` (JVM-only, and it
   * writes `+` for a space, which is a query-string rule) */
  private[http] def encodeSeg(s: String): String =
    val bs = s.getBytes(UTF_8)
    val sb = new StringBuilder(bs.length)
    var i = 0
    while i < bs.length do
      val b = bs(i) & 0xff
      val c = b.toChar
      if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
        || c == '-' || c == '.' || c == '_' || c == '~'
      then sb.append(c)
      else
        sb.append('%')
        sb.append(hexDigits.charAt(b >> 4))
        sb.append(hexDigits.charAt(b & 0xf))
      i += 1
    sb.toString

  private[http] def decodeSeg(s: String): Option[String] =
    if s.indexOf('%') < 0 then Some(s)
    else
      val out = scala.collection.mutable.ArrayBuilder.make[Byte]
      var i = 0
      var ok = true
      while i < s.length && ok do
        if s.charAt(i) == '%' then
          if i + 2 < s.length then
            val h = hex(s.charAt(i + 1))
            val l = hex(s.charAt(i + 2))
            if h < 0 || l < 0 then ok = false
            else
              out.addOne(((h << 4) | l).toByte)
              i += 3
          else ok = false
        else
          // the whole run up to the next escape at once, so a
          // surrogate pair keeps its two halves together
          val j = s.indexOf('%', i)
          val end = if j < 0 then s.length else j
          out.addAll(s.substring(i, end).getBytes(UTF_8))
          i = end
      if ok then Some(new String(out.result(), UTF_8)) else None

/**
 * The query string a route reads — a monoid of named parameters,
 * composed with `&` and handed to a route with `?`
 * (specs/optics-outside.md, stage 3).
 *
 * Its own type, and not more `Route` combinators, for one reason: the
 * path is ORDERED and the query is not. A path parameter is found by
 * position, a query parameter by name, and a url that writes them in
 * a different order is the same request. Keeping them apart lets the
 * split between the two halves of the tuple be exactly one `Split`,
 * so the path's parameters keep their positions whatever the query
 * does.
 *
 * The consequence worth stating: with a query, `url` writes the
 * parameters in DECLARATION order, which makes it the canonical url
 * among the many a route accepts. `unapply(url(a)) == Some(a)` still
 * holds; `url(unapply(u)) == u` does not, and never did — that is
 * what makes this a prism rather than an iso.
 */
final class Query[B <: Tuple] private[http] (
    /** the description: the parameters, with no request in hand */
    val declared: Vector[Route.Q],
    private[http] val decode: Route.Params => Option[B],
    private[http] val encode: B => Vector[(String, String)]):

  /** `+&` and not `&`: `&` is below `:` in Scala's precedence table,
   * so `r :? a & b` would group as `(r :? a) & b`. `+` is above it,
   * and the whole declaration then needs no parentheses. */
  def +&[C <: Tuple](that: Query[C])(using c: Route.Split[B, C]): Query[c.Out] =
    new Query[c.Out](declared ++ that.declared,
      qs => decode(qs).flatMap(b => that.decode(qs).map(d => c.join(b, d))),
      o =>
        val (b, d) = c.split(o)
        encode(b) ++ that.encode(d))

object Query:

  /** required: the route misses without it */
  def apply[T](name: String)(using p: Route.Param[T]): Query[T *: EmptyTuple] =
    new Query(Vector(Route.Q(name, p.kind, required = true, repeated = false)),
      qs => qs.get(name).flatMap(_.headOption).flatMap(p.parse).map(_ *: EmptyTuple),
      t => Vector(name -> p.print(t.head)))

  /**
   * optional: absent is `None`, and `None` writes nothing.
   *
   * PRESENT AND UNPARSEABLE IS A MISS, not `None`. `?page=abc` on an
   * `Int` is a request that meant something and got it wrong, and
   * answering it as though the parameter had been omitted hides the
   * caller's mistake behind a page of results.
   */
  def opt[T](name: String)(using p: Route.Param[T]): Query[Option[T] *: EmptyTuple] =
    new Query(Vector(Route.Q(name, p.kind, required = false, repeated = false)),
      qs => qs.get(name).flatMap(_.headOption) match
        case None => Some(None *: EmptyTuple)
        case Some(v) => p.parse(v).map(x => Some(x) *: EmptyTuple),
      t => t.head.map(x => name -> p.print(x)).toVector)

  /** repeated: every occurrence in wire order, and an empty vector
   * writes nothing */
  def all[T](name: String)(using p: Route.Param[T]): Query[Vector[T] *: EmptyTuple] =
    new Query(Vector(Route.Q(name, p.kind, required = false, repeated = true)),
      qs =>
        val parsed = qs.getOrElse(name, Vector.empty).map(p.parse)
        if parsed.forall(_.isDefined) then Some(parsed.map(_.get) *: EmptyTuple) else None,
      t => t.head.map(x => name -> p.print(x)))

/** the verb, as an extractor, so it composes with any route:
 * `case Get(userPost(id, slug)) =>` */
object Get:
  def unapply(r: Request): Option[String] = if r.method == Method.Get then Some(r.url) else None

object Post:
  def unapply(r: Request): Option[String] = if r.method == Method.Post then Some(r.url) else None

object Put:
  def unapply(r: Request): Option[String] = if r.method == Method.Put then Some(r.url) else None

object Patch:
  def unapply(r: Request): Option[String] = if r.method == Method.Patch then Some(r.url) else None

object Delete:
  def unapply(r: Request): Option[String] = if r.method == Method.Delete then Some(r.url) else None

/**
 * A dispatch table that is also a description.
 *
 * The point is the last line of `describe`: the listing is derived
 * from the same values that dispatch, so a route cannot be documented
 * and unrouted, or routed and undocumented. That is the whole reason
 * to reify a route instead of writing a `case` — and the existing
 * convention is untouched, since `routes` is the same
 * `PartialFunction[Request, Response ! Async]` every server in this
 * repository already takes.
 */
final class Router private (val entries: Vector[Router.Entry]):

  /** a handler that needs only the path's parameters */
  def on[A <: Tuple](method: Method, route: Route[A])(h: A => Response ! Async): Router =
    at(method, route)((a, _) => h(a))

  /** a handler that needs the request too — its body, its headers, its
   * peer. Most do, which is why `on` is defined in terms of this one
   * and not the other way round. */
  def at[A <: Tuple](method: Method, route: Route[A])(h: (A, Request) => Response ! Async): Router =
    new Router(entries :+ new Router.Entry(method, route.describe, r =>
      if r.method != method then None else route.unapply(r.url).map(a => h(a, r))))

  /** the same, reading a case class */
  def of[C <: Product, A <: Tuple](method: Method, route: Route.Of[C, A])(h: C => Response ! Async): Router =
    ofAt(method, route)((c, _) => h(c))

  def ofAt[C <: Product, A <: Tuple](method: Method, route: Route.Of[C, A])
                                    (h: (C, Request) => Response ! Async): Router =
    new Router(entries :+ new Router.Entry(method, route.describe, r =>
      if r.method != method then None else route.unapply(r.url).map(c => h(c, r))))

  /** the existing convention, unchanged: a miss is simply undefined,
   * so the caller's 404 stays the caller's */
  def routes: PartialFunction[Request, Response ! Async] =
    Function.unlift(r => entries.iterator.map(_.run(r)).collectFirst { case Some(x) => x })

  /** every entry, from the same values that dispatch */
  def describe: Vector[(Method, String)] = entries.map(e => (e.method, e.path))

object Router:
  val empty: Router = new Router(Vector.empty)

  final class Entry private[http] (val method: Method, val path: String,
                                   private[http] val run: Request => Option[Response ! Async])
