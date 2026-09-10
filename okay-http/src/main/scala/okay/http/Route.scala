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
    private[http] val decode: Vector[String] => Option[A],
    private[http] val encode: A => Vector[String]):

  /** append a literal segment */
  def /(lit: String): Route[A] =
    new Route(segments :+ Route.Seg.Lit(lit), ss =>
      if ss.nonEmpty && ss.last == lit then decode(ss.init) else None,
      a => encode(a) :+ lit)

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
    new Route[c.Out](segments ++ that.segments,
      ss =>
        if ss.length != width + that.segments.length then None
        else
          val (l, r) = ss.splitAt(width)
          decode(l).flatMap(a => that.decode(r).map(b => c.join(a, b))),
      o =>
        val (a, b) = c.split(o)
        encode(a) ++ that.encode(b))

  /** interpreter 1 — MATCH. Also the extractor. */
  def unapply(path: String): Option[A] =
    Route.segmentsOf(path).filter(_.length == segments.length).flatMap(decode)

  /** interpreter 2 — BUILD. Reverse routing from the same declaration. */
  def url(a: A): String =
    encode(a).map(Route.encodeSeg).mkString("/", "/", "")

  /** interpreter 3 — DESCRIBE. `/users/{id}/posts/{slug}`. */
  def describe: String =
    segments.map {
      case Route.Seg.Lit(v) => v
      case Route.Seg.Var(n, _) => "{" + n + "}"
    }.mkString("/", "/", "")

  /** the parameters this route captures, in order — what a generated
   * OpenAPI operation or MCP tool schema is built from */
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

  /**
   * a captured segment's codec — the only thing a new parameter type
   * has to supply, and it is three lines.
   */
  trait Param[T]:
    def kind: String
    def parse(s: String): Option[T]
    def print(t: T): String

  object Param:
    /**
     * A path segment is not a value when it is empty: `"/x//y"` and
     * `"/x/y"` would build the same URL from different parameters, so
     * an empty capture is REFUSED rather than silently round-tripping
     * wrong. That is a property of URLs, not of this encoding, and it
     * is what keeps `unapply(url(a)) == Some(a)` total on the domain.
     */
    given string: Param[String] with
      def kind = "string"
      def parse(s: String): Option[String] = if s.isEmpty then None else Some(s)
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

  /** the empty path, `"/"` */
  val root: Route[EmptyTuple] =
    new Route(Vector.empty, ss => if ss.isEmpty then Some(EmptyTuple) else None, _ => Vector.empty)

  /** one literal segment */
  def lit(s: String): Route[EmptyTuple] = root / s

  /** one captured segment */
  def apply[T](name: String)(using p: Param[T]): Route[T *: EmptyTuple] =
    new Route(Vector(Seg.Var(name, p.kind)),
      ss => ss match
        case Vector(one) => p.parse(one).map(_ *: EmptyTuple)
        case _ => None,
      t => Vector(p.print(t.head)))

  /** a route read as a case class rather than a tuple */
  final class Of[C <: Product, A <: Tuple] private[http] (
      private val r: Route[A],
      private val m: Mirror.ProductOf[C] { type MirroredElemTypes = A }):
    def unapply(path: String): Option[C] = r.unapply(path).map(m.fromProduct)
    def url(c: C): String = r.url(Tuple.fromProductTyped(c)(using m))
    def describe: String = r.describe
    def segments: Vector[Seg] = r.segments
    def prism: Prism[String, String, C, C] = Prism(s => unapply(s).toRight(s), url)

  /**
   * A path's segments, percent-decoded AFTER the split so an encoded
   * `/` can never be read as a boundary. A query string is not part of
   * the path (stage 2), and malformed escaping is a MISS rather than a
   * silent literal `%`.
   */
  private[http] def segmentsOf(path: String): Option[Vector[String]] =
    val q = path.indexOf('?')
    val p0 = if q >= 0 then path.substring(0, q) else path
    val p = if p0.startsWith("/") then p0.substring(1) else p0
    if p.isEmpty then Some(Vector.empty)
    else
      val raw = p.split("/", -1).toVector
      val out = raw.map(decodeSeg)
      if out.forall(_.isDefined) then Some(out.map(_.get)) else None

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
