package okay.http

import okay.*
import scala.deriving.Mirror
import scala.compiletime.constValueTuple
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
/**
 * What a declared url can do once it exists, whether or not it has a
 * query: the three interpreters and the bridge to the core optics.
 *
 * It is a trait rather than one class because a url is a path and THEN
 * a query — `:?` answers a `Queried`, which has no `/`, so a path
 * segment written after a query parameter cannot compile. That refusal
 * is structural on purpose (specs/optics-outside.md, stage 5): an
 * attempt to make the MESSAGE nicer, by giving `Named` a poisoned `/`
 * carrying `@compileTimeOnly`, fired in five isolated variants and
 * silently did not in the real expression — and made two invalid
 * declarations compile. A guarantee with an awkward message beats a
 * good message that holds only usually.
 */
sealed trait Routed[A <: Tuple]:

  /** the description: what this url looks like, with no request in hand */
  val segments: Vector[Route.Seg]
  /** the query half of the description, in declaration order */
  val queries: Vector[Route.Q]
  private[http] val decode: (Vector[String], Route.Params) => Option[A]
  private[http] val encode: A => (Vector[String], Vector[(String, String)])

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
      case v: Route.Seg.Var => "{" + v.name + "}"
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
   * The description as ONE value — the whole of what a renderer is
   * given, and the only thing a `Router.Entry` carries about the url.
   *
   * `describe` alone was what the router used to take, and a template
   * without its parameters is a description that has already lost the
   * interesting half (`Route.Described`).
   */
  def described: Route.Described = Route.Described(describe, params, queries)

  /**
   * A REQUEST HEADER, declared (specs/route-headers.md).
   *
   * `:@` because `@` reads as *at* — metadata carried AT the request
   * rather than in the url — and because its first character is `:`,
   * so it shares `:?`'s precedence and stays left-associative. The
   * precedence reasoning is in `:?`'s own comment.
   *
   * The result is NOT a `Routed`. A header cannot join `A` without
   * breaking `unapply(url(a)) == Some(a)`: `url` would have nowhere
   * to put it, and the law would hold "mostly". `Routed[A]` is a
   * prism on the URL and keeps its law; `Headed[A, H]` is the
   * request-shaped declaration, and they are different types on
   * purpose.
   */
  def :@[B <: Tuple](q: Query[B]): Headed[A, B] =
    new Headed[A, B](this, q.declared.map(Route.headerOf), q.declared.map(_.name), q.decode)

  /** one declared header: `:@ "last-event-id".as[String]` */
  def :@[T](n: Route.Named[T]): Headed[A, T *: EmptyTuple] =
    this :@ Query[T](n.name)(using n.param)

  /**
   * WHAT THIS ROUTE REQUIRES OF A CALLER (stage B).
   *
   * It lives on the request-shaped declaration for stage A's own
   * reason: a credential is read from the REQUEST, not from the url,
   * and `Routed[A]` is the url's prism. So `secured` produces a
   * `Headed`, exactly as `:@` does.
   */
  def secured(scopes: String*): Headed[A, EmptyTuple] =
    new Headed[A, EmptyTuple](this, Vector.empty, Vector.empty,
      _ => Some(EmptyTuple), Vector(Route.Security(scopes = scopes.toSet)))

  def securedBy(s: Route.Security): Headed[A, EmptyTuple] =
    new Headed[A, EmptyTuple](this, Vector.empty, Vector.empty,
      _ => Some(EmptyTuple), Vector(s))

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
   *
   * The mapping is POSITIONAL (`m.fromProduct`), so the compiler checks
   * the types and says nothing about the names. This checks the names
   * (specs/optics-outside.md, stage 6): the path's parameters in order,
   * then the query's, against the class's fields. A mismatch is refused
   * here rather than reported, because the mapping it would produce is
   * wrong and there is no sensible way to carry on with it.
   *
   * The check runs at CONSTRUCTION, not at compile time, and the
   * spec says why: the labels are type-level, the route's names are
   * values, and lifting the names into the type would cost a second
   * type parameter on every signature. A route is a `val`, so this
   * fires at class initialisation — start-up, before the first
   * request, or never.
   */
  inline def of[C <: Product](using m: Mirror.ProductOf[C] { type MirroredElemTypes = A }): Route.Of[C, A] =
    Route.Of.checked(this, m,
      constValueTuple[m.MirroredElemLabels].productIterator.map(_.toString).toVector)


/**
 * The PATH stage: segments may still be added, and a query may start.
 */
final class Route[A <: Tuple] private[http] (
    val segments: Vector[Route.Seg],
    val queries: Vector[Route.Q],
    private[http] val decode: (Vector[String], Route.Params) => Option[A],
    private[http] val encode: A => (Vector[String], Vector[(String, String)]))
  extends Routed[A]:

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
  def :?[B <: Tuple](q: Query[B])(using c: Route.Split[A, B]): Queried[c.Out] =
    new Queried[c.Out](segments, queries ++ q.declared,
      (ss, qs) => decode(ss, qs).flatMap(a => q.decode(qs).map(b => c.join(a, b))),
      o =>
        val (a, b) = c.split(o)
        val (segs, qp) = encode(a)
        (segs, qp ++ q.encode(b)))

  /** one required query parameter, named: `:? "q"[String]` */
  def :?[T](n: Route.Named[T])(using c: Route.Split[A, T *: EmptyTuple]): Queried[c.Out] =
    this :? Query[T](n.name)(using n.param)

  /** one captured segment, named: `/ "id"[Int]` */
  def /[T](n: Route.Named[T])(using c: Route.Split[A, T *: EmptyTuple]): Route[c.Out] =
    this / Route[T](n.name)(using n.param)

/**
 * The QUERY stage: more parameters may be added, but no more path.
 *
 * There is no `/` here, and that is the whole reason the type exists:
 * a url is a path and THEN a query, so a segment written after a query
 * parameter would declare something `url` could never place.
 */
final class Queried[A <: Tuple] private[http] (
    val segments: Vector[Route.Seg],
    val queries: Vector[Route.Q],
    private[http] val decode: (Vector[String], Route.Params) => Option[A],
    private[http] val encode: A => (Vector[String], Vector[(String, String)]))
  extends Routed[A]:

  def :?[B <: Tuple](q: Query[B])(using c: Route.Split[A, B]): Queried[c.Out] =
    new Queried[c.Out](segments, queries ++ q.declared,
      (ss, qs) => decode(ss, qs).flatMap(a => q.decode(qs).map(b => c.join(a, b))),
      o =>
        val (a, b) = c.split(o)
        val (segs, qp) = encode(a)
        (segs, qp ++ q.encode(b)))

  def :?[T](n: Route.Named[T])(using c: Route.Split[A, T *: EmptyTuple]): Queried[c.Out] =
    this :? Query[T](n.name)(using n.param)

/**
 * The HEADER stage: a url declaration plus what it reads off the
 * request (specs/route-headers.md).
 *
 * It WRAPS a `Routed[A]` rather than extending it, so nothing holding
 * a `Routed` can be handed a value whose law is different. The url
 * half is untouched — `route.unapply`, `route.url` and `route.prism`
 * are the same prism they were — and the header half has a law of the
 * same shape: `readHeaders(requestWith(h)) == Some(h)`.
 */
final class Headed[A <: Tuple, Hs <: Tuple] private[http] (
    val route: Routed[A],
    /** the description: the headers, with no request in hand */
    val headers: Vector[Route.Hdr],
    private[http] val names: Vector[String],
    private[http] val decodeHeaders: Route.Params => Option[Hs],
    /** what it requires of a caller (stage B); empty is "nothing" */
    val security: Vector[Route.Security] = Vector.empty):

  def :@[B <: Tuple](q: Query[B])(using c: Route.Split[Hs, B]): Headed[A, c.Out] =
    new Headed[A, c.Out](route, headers ++ q.declared.map(Route.headerOf),
      names ++ q.declared.map(_.name),
      hs => decodeHeaders(hs).flatMap(a => q.decode(hs).map(b => c.join(a, b))),
      security)

  def :@[T](n: Route.Named[T])(using c: Route.Split[Hs, T *: EmptyTuple]): Headed[A, c.Out] =
    this :@ Query[T](n.name)(using n.param)

  /** see `Routed.secured` — a credential is read from the request */
  def secured(scopes: String*): Headed[A, Hs] =
    new Headed[A, Hs](route, headers, names, decodeHeaders,
      security :+ Route.Security(scopes = scopes.toSet))

  def securedBy(s: Route.Security): Headed[A, Hs] =
    new Headed[A, Hs](route, headers, names, decodeHeaders, security :+ s)

  /**
   * The headers this route declares, read off a request.
   *
   * A HEADER BLOCK IS A `Map[String, Vector[String]]` — the very shape
   * a query string is, which is why one builder (`Query`) serves both
   * and there is no parallel set of header combinators to keep in
   * step. The one difference is CASE: header names are
   * case-insensitive on the wire, so the map is built under the names
   * the DECLARATION used, each filled from whatever casing arrived.
   */
  def readHeaders(r: Request): Option[Hs] =
    val lower = r.headers.map((k, v) => (k.toLowerCase, v))
    val m: Route.Params = names.map { n =>
      val ln = n.toLowerCase
      n -> lower.collect { case (k, v) if k == ln => v }.toVector
    }.toMap
    decodeHeaders(m)

  /** the whole match: the url's parameters and the request's headers */
  def read(r: Request): Option[(A, Hs)] =
    route.unapply(r.url).flatMap(a => readHeaders(r).map(h => (a, h)))

  def describe: String = route.describe
  def described: Route.Described =
    route.described.copy(headers = headers, security = security)

object Route:

  /** a segment of the description.
   *
   * `Var` carries the parameter's JSON Schema as well as its `kind`,
   * because the kind is a WORD and a renderer needs a shape — and
   * because a custom `Param` may say more about itself than four
   * words can (a format, an enum). Capturing it at declaration time is
   * what keeps that override alive; a renderer handed only the kind
   * would have to re-derive it and would lose exactly the parameters
   * whose author took the trouble. */
  enum Seg:
    case Lit(value: String)
    case Var(name: String, kind: String, schema: okay.codec.Json = Param.schemaOf("string"))

  /** a query parameter of the description */
  final case class Q(name: String, kind: String, required: Boolean, repeated: Boolean,
                     schema: okay.codec.Json = Param.schemaOf("string"))

  /**
   * a REQUEST HEADER of the description (specs/route-headers.md).
   *
   * `Q`'s shape exactly, because a header parameter and a query
   * parameter differ only in where they are read — and they are
   * separate TYPES rather than one, so a renderer cannot put `in:
   * query` on a header by holding the wrong vector.
   *
   * `Hdr` and not `H`: `H` is the conventional name for the head of a
   * tuple and `Split.cons[H, T, B]` below uses it, so a case class of
   * that name shadows a type parameter three screens away. The
   * compiler says so (E226), and a warning the gate now refuses is
   * not a thing to argue with over one letter.
   */
  final case class Hdr(name: String, kind: String, required: Boolean, repeated: Boolean,
                       schema: okay.codec.Json = Param.schemaOf("string"))

  private[http] def headerOf(q: Q): Hdr = Hdr(q.name, q.kind, q.required, q.repeated, q.schema)

  /**
   * WHAT A PROTECTED ROUTE REQUIRES (specs/route-headers.md, stage B).
   *
   * Plain data, and deliberately vocabulary-free: okay-security
   * depends on okay-http and not the other way round, so a `Policy`
   * or a `Verified` cannot appear here and okay-http must not grow an
   * identity model of its own. What it owns is the HTTP half — a
   * scheme name, the scopes an operation asks for, and the realm a
   * challenge names — which is also exactly what an OpenAPI
   * `securityScheme` carries.
   *
   * A richer rule than "these scopes" stays with
   * `okay.security.Secure.granted`: this is the declaration a document
   * can render and a table can enforce, not a replacement for a
   * policy language.
   */
  final case class Security(scheme: String = "bearer",
                            scopes: Set[String] = Set.empty,
                            realm: String = "okay")

  /**
   * The description of one url, whole: the template AND what its
   * parameters are (openapi-parameters).
   *
   * It exists as ONE value because the halves came apart. `Router`
   * used to take `route.describe` — a String — so the path template
   * reached a renderer without its parameters' kinds, and the
   * renderer re-parsed `{name}` out of the template and declared
   * every one of them a string. An `Int` path parameter was
   * documented as text, in a document whose whole claim is that it
   * cannot drift from the router. A value that cannot be passed
   * without its parts is the fix; two more fields would have been the
   * patch.
   */
  final case class Described(path: String, params: Vector[Seg.Var], queries: Vector[Q],
                            /** request headers the route declares; a header is
                             * not part of the url, so it is beside the
                             * template rather than in it */
                            headers: Vector[Hdr] = Vector.empty,
                            /** what the route requires of a caller; empty is
                             * "nothing", which is what most routes require */
                            security: Vector[Security] = Vector.empty)

  /**
   * A NAMED TYPED PARAMETER — the same thing in a path and in a query,
   * which is why there is one type and not two: the OPERATOR says where
   * it goes (`/` into the path, `:?` into the query). Written
   * `"id"[Int]` with `import okay.http.syntax.*`.
   */
  final case class Named[T](name: String, param: Param[T])

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

    /**
     * The DECLARE interpreter of a parameter: its shape as JSON
     * Schema, for whoever renders the route (okay-openapi today).
     *
     * Concrete rather than abstract, so the three lines a new
     * parameter type has to supply stay three. Override it to say
     * more than the kind can — `{"type":"string","format":"uuid"}`
     * for a `Param[UUID]`, or an `enum` for a closed vocabulary.
     */
    def jsonSchema: okay.codec.Json = Param.schemaOf(kind)

  object Param:
    /**
     * A kind's shape, through okay-codec's OWN mapping rather than a
     * second one written here: `JsonSchema.of` is what every other
     * declaration in this repository renders with — a tool's
     * arguments, a request body, a declared answer — and a document
     * with two vocabularies for "integer" is a document that
     * disagrees with itself.
     *
     * An unrecognised kind is a string, which is what a url segment
     * is; a custom `Param` that wants to say otherwise overrides
     * `jsonSchema` rather than hoping this function learns its word.
     */
    /** a REPEATED parameter is an array of the element's shape —
     * `"tag".all[String]` accepts `?tag=a&tag=b`, and a document that
     * declared it a string would be describing a different service */
    def arrayOf[T](p: Param[T]): okay.codec.Json =
      okay.codec.Json.JObj(Vector(
        "type" -> okay.codec.Json.JStr("array"),
        "items" -> p.jsonSchema))

    def schemaOf(kind: String): okay.codec.Json = kind match
      case "int" | "long" => okay.codec.JsonSchema.of(okay.codec.Schema.SInt)
      case "boolean" => okay.codec.JsonSchema.of(okay.codec.Schema.SBool)
      case _ => okay.codec.JsonSchema.of(okay.codec.Schema.SString)

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
   * ARITY 1 COLLAPSES, at the handler boundary and nowhere else.
   *
   * A route's `A` is a tuple because a path may capture any number of
   * things, and `unapply`/`url` keep it: `Some(Tuple1(7))` is the
   * honest answer for a prism whose focus is one value, and the optic
   * laws are stated over `A`. But a HANDLER is written by a person,
   * and `t => t.head` is what a person writes when the API hands them
   * a `Tuple1`. Three sightings, one of them by another author and one
   * by the API's own, are what opened this.
   *
   * A witness rather than a match type, for the reason `Split` above
   * is one: a match type says what `Out` IS and leaves you to produce
   * it, which here would mean a cast. This carries the conversion.
   */
  sealed trait Arity[A <: Tuple]:
    type Out
    def apply(a: A): Out

  object Arity extends Arity.Whole:
    /** the one that collapses */
    given one[T]: (Arity[T *: EmptyTuple] { type Out = T }) =
      new Arity[T *: EmptyTuple]:
        type Out = T
        def apply(a: T *: EmptyTuple): T = a.head

    /** everything else is itself — including `EmptyTuple`, which a
     * handler already writes as `_ => ...` */
    trait Whole:
      given whole[A <: Tuple]: (Arity[A] { type Out = A }) =
        new Arity[A]:
          type Out = A
          def apply(a: A): A = a

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
    new Route(Vector(Seg.Var(name, p.kind, p.jsonSchema)), Vector.empty,
      (ss, _) => ss match
        case Vector(one) if one.nonEmpty => p.parse(one).map(_ *: EmptyTuple)
        case _ => None,
      t => (Vector(p.print(t.head)), Vector.empty))

  object Of:
    /**
     * Build the case-class view, refusing a route whose parameter
     * names are not the class's field names, in order.
     *
     * `names` comes from the caller because only an `inline` context
     * can read `MirroredElemLabels`; keeping the comparison here keeps
     * the message in one place.
     */
    def checked[C <: Product, A <: Tuple](
        r: Routed[A],
        m: Mirror.ProductOf[C] { type MirroredElemTypes = A },
        labels: Vector[String]): Of[C, A] =
      val names = r.params.map(_.name) ++ r.queries.map(_.name)
      require(names == labels,
        s"route parameters ${names.mkString("(", ", ", ")")} do not match " +
          s"the field names ${labels.mkString("(", ", ", ")")}: " +
          "the mapping is positional, so the types already agree — " +
          "rename one side so the declaration says what it means")
      new Of(r, m)

  /** a route read as a case class rather than a tuple */
  final class Of[C <: Product, A <: Tuple] private[http] (
      private val r: Routed[A],
      private val m: Mirror.ProductOf[C] { type MirroredElemTypes = A }):
    def unapply(path: String): Option[C] = r.unapply(path).map(m.fromProduct)
    def url(c: C): String = r.url(Tuple.fromProductTyped(c)(using m))
    def describe: String = r.describe
    def describeFull: String = r.describeFull
    def segments: Vector[Seg] = r.segments
    def queries: Vector[Q] = r.queries
    def described: Described = r.described
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
    new Query(Vector(Route.Q(name, p.kind, required = true, repeated = false, p.jsonSchema)),
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
    new Query(Vector(Route.Q(name, p.kind, required = false, repeated = false, p.jsonSchema)),
      qs => qs.get(name).flatMap(_.headOption) match
        case None => Some(None *: EmptyTuple)
        case Some(v) => p.parse(v).map(x => Some(x) *: EmptyTuple),
      t => t.head.map(x => name -> p.print(x)).toVector)

  /** repeated: every occurrence in wire order, and an empty vector
   * writes nothing */
  def all[T](name: String)(using p: Route.Param[T]): Query[Vector[T] *: EmptyTuple] =
    new Query(Vector(Route.Q(name, p.kind, required = false, repeated = true, Route.Param.arrayOf(p))),
      qs =>
        val parsed = qs.getOrElse(name, Vector.empty).map(p.parse)
        if parsed.forall(_.isDefined) then Some(parsed.map(_.get) *: EmptyTuple) else None,
      t => t.head.map(x => name -> p.print(x)))

/**
 * The terse declaration syntax, behind an import
 * (specs/optics-outside.md, stage 5).
 *
 * ```scala
 * import okay.http.syntax.*
 * val search = Route / "search" :? "q".as[String] :? "page".opt[Int]
 * val tagged = Route / "posts" / "id".as[Int] :? "tag".all[String]
 * ```
 *
 * One rule: **a bare string is a literal segment, `"name".as[T]` is a
 * hole.** The operator says where the parameter goes — `/` into the
 * path, `:?` into the query — because a named typed parameter is the
 * same thing in both places.
 *
 * Named `syntax` and not `query`: a `query` object beside the `Query`
 * class differs from it only in case, and on a case-insensitive
 * filesystem the two class files overwrite one another — the compiler
 * says so, and this repository allows no warnings.
 *
 * Behind an import on purpose: `apply[T]` on `String` reaches every
 * string in scope, and a stray `"abc"[Int]` should not answer with a
 * message about `Param` in a file that never asked for any of this.
 * `Route[Int]("id")` and `Query[String]("q")` stay for the reader
 * meeting the api rather than using it.
 */
object syntax:
  extension (name: String)
    /**
     * a named parameter of type `T`: a captured segment after `/`, a
     * required query parameter after `:?`.
     *
     * `as` and not `apply`, which would have let it be written
     * `"id"[Int]`: `String` ALREADY has an `apply` (via `StringOps`),
     * and an extension is only reached when the member does not exist,
     * so `"id"[Int]` resolves to the standard library and fails with a
     * message about `StringOps`. It compiled in isolation, where no
     * competitor was in scope, and stopped compiling the moment it met
     * one — which is also why all three forms here are named methods
     * now, rather than one magic `apply` beside two named ones.
     */
    def as[T](using p: Route.Param[T]): Route.Named[T] = Route.Named(name, p)

    /** an optional query parameter — absent is `None`, `None` writes nothing */
    def opt[T](using Route.Param[T]): Query[Option[T] *: EmptyTuple] = Query.opt[T](name)

    /** a repeated query parameter — every occurrence, in wire order */
    def all[T](using Route.Param[T]): Query[Vector[T] *: EmptyTuple] = Query.all[T](name)

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

  /**
   * What the operation just declared is FOR (openapi-prose).
   *
   * It attaches to the LAST entry, which is what a builder chain
   * already reads as, and it is one method rather than a parameter on
   * each of twenty combinators. Writing it second is deliberate: the
   * declaration says what the operation IS — its path, its body, what
   * it answers — and the sentence is about that, so it reads in the
   * order it is written.
   *
   * {{{
   *   Router.html(Get, Route.root)(_ => pure(page)).summarised("the chat page")
   * }}}
   *
   * Summarising an empty router throws, HERE, when the table is
   * built: a builder method that silently did nothing would put the
   * sentence on no operation at all and report that nowhere.
   */
  /**
   * The headers an answer carries, declared (stage C).
   *
   * It attaches to the entry just declared, as `summarised` does, and
   * for the same reason: one method rather than a parameter on twenty
   * combinators. A status this entry does not already answer gets an
   * answer with no schema — "it sends this header" is a fact worth
   * stating even when the body is undeclared.
   *
   * {{{
   *   Router.out(Get, task)(byId).answering(200, "etag".as[String])
   * }}}
   *
   * THIS IS DESCRIPTION, and the doc comment on `Answer.headers` says
   * why it cannot be more: the handler builds its own `Response`, and
   * refusing a request because a declared header was missing would
   * turn a documentation slip into a 500. The headers the ROUTER
   * itself writes — a secured route's `WWW-Authenticate` — are the
   * other kind, and they are true by construction.
   */
  def answering(status: Int, headers: Route.Named[?]*): Router =
    if entries.isEmpty then
      throw IllegalStateException(
        "answering: there is no operation to describe — it attaches to the entry just declared")
    else
      val hs = headers.toVector.map(n =>
        Route.Hdr(n.name, n.param.kind, required = true, repeated = false, n.param.jsonSchema))
      new Router(entries.init :+ entries.last.answeringWith(status, hs))

  def summarised(text: String): Router =
    if entries.isEmpty then
      throw IllegalStateException(
        "summarised: there is no operation to summarise — it attaches to the entry just declared")
    else new Router(entries.init :+ entries.last.saying(text))

  /** a handler that needs only the path's parameters */
  def on[A <: Tuple](method: Method, route: Routed[A])(using ar: Route.Arity[A])
                    (h: ar.Out => Response ! Async): Router =
    at(method, route)((a, _) => h(a))

  /** a handler that needs the request too — its body, its headers, its
   * peer. Most do, which is why `on` is defined in terms of this one
   * and not the other way round. */
  def at[A <: Tuple](method: Method, route: Routed[A])(using ar: Route.Arity[A])
                    (h: (ar.Out, Request) => Response ! Async): Router =
    new Router(entries :+ new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r => if r.method != method then None else route.unapply(r.url).map(a => h(ar(a), r))))

  /**
   * the same, for a route that also declares what it reads off the
   * request's HEADERS (specs/route-headers.md).
   *
   * A declared header that is REQUIRED and absent is a MISS, exactly
   * as a required query parameter is: the route does not match, and
   * the caller's 404 stays the caller's. It is not a 400 — a router
   * that answered 400 would be claiming no other route could have
   * matched, which it cannot know.
   */
  def on[A <: Tuple, Hs <: Tuple](method: Method, route: Headed[A, Hs])
                                 (using ar: Route.Arity[A], hr: Route.Arity[Hs])
                                 (h: (ar.Out, hr.Out) => Response ! Async): Router =
    at(method, route)((a, hs, _) => h(a, hs))

  def at[A <: Tuple, Hs <: Tuple](method: Method, route: Headed[A, Hs])
                                 (using ar: Route.Arity[A], hr: Route.Arity[Hs])
                                 (h: (ar.Out, hr.Out, Request) => Response ! Async): Router =
    new Router(entries :+ new Router.Entry(method, route.described,
      r => r.method == method && route.read(r).isDefined,
      r =>
        if r.method != method then None
        else route.read(r).map((a, hs) => h(ar(a), hr(hs), r)),
      None,
      Router.securityAnswers(route.described)))

  /**
   * a handler whose request carries a declared JSON body
   * (specs/optics-outside.md, stage 7).
   *
   * The body is declared HERE and not on the `Route`, because a route
   * describes a url and a url has no body. `Schema[B]` decodes before
   * the handler runs, so a handler never sees an undecoded body, and a
   * body that does not decode is answered 400 with DATA — the same
   * rule `Toolbox` states for a tool call, and for the same reason: a
   * caller given `{"error": …}` can see what was wrong with their
   * request, while one given a diagnosis about something else cannot.
   */
  def json[A <: Tuple, B](method: Method, route: Routed[A])(using ar: Route.Arity[A])
                         (h: (ar.Out, B) => Response ! Async)
                         (using okay.codec.Schema[B]): Router =
    jsonAt[A, B](method, route)((a, b, _) => h(a, b))

  /**
   * a declared body AND the request — its headers, its peer, its
   * session.
   *
   * The same relation `at` has to `on`, and discovered the same way:
   * `json` alone could not express okay-chat's route, whose
   * `turnOverride` seam is handed the request so a verified session
   * can identify the speaker over anything the body claims. A handler
   * that may not see the request is a demo, not an api — so `jsonAt`
   * is the primitive and `json` is written in terms of it.
   */
  def jsonAt[A <: Tuple, B](method: Method, route: Routed[A])(using ar: Route.Arity[A])
                           (h: (ar.Out, B, Request) => Response ! Async)
                           (using sc: okay.codec.Schema[B]): Router =
    new Router(entries :+ new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r =>
        if r.method != method then None
        else route.unapply(r.url).map { a =>
          okay.codec.Codecs.json(sc).decode(
            okay.codec.Json.parse(new String(r.body.bytes, java.nio.charset.StandardCharsets.UTF_8))) match
            case Right(b) => h(ar(a), b, r)
            case Left(why) => pure(Router.badRequest(why))
        },
      Some(okay.codec.JsonSchema.of(sc))))

  /**
   * THE HANDLER ANSWERS A VALUE, NOT A RESPONSE (openapi-responses).
   *
   * This is the output side of `json[B]`, and it is a declaration by
   * construction: the router encodes the handler's answer with the
   * same `Schema` the entry carries, so what a document says and what
   * a client receives are the same derivation. A handler that builds
   * its own `Response` is still allowed (`on`, `at`) — it simply
   * declares nothing, and a renderer says so.
   */
  def out[A <: Tuple, R](method: Method, route: Routed[A], status: Int = 200,
                         description: String = "the declared answer")
                        (using ar: Route.Arity[A])
                        (h: ar.Out => R ! Async)(using sr: okay.codec.Schema[R]): Router =
    outAt[A, R](method, route, status, description)((a, _) => h(a))

  /** the same, with the request in hand */
  def outAt[A <: Tuple, R](method: Method, route: Routed[A], status: Int = 200,
                           description: String = "the declared answer")
                          (using ar: Route.Arity[A])
                          (h: (ar.Out, Request) => R ! Async)(using sr: okay.codec.Schema[R]): Router =
    new Router(entries :+ new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r =>
        if r.method != method then None
        else route.unapply(r.url).map(a => h(ar(a), r).map(Router.encoded(status, _))),
      None,
      Vector(Router.Answer(status, Some(okay.codec.JsonSchema.of(sr)), description))))

  /** both sides declared: a body in, a value out */
  def jsonOut[A <: Tuple, B, R](method: Method, route: Routed[A], status: Int = 200,
                                description: String = "the declared answer")
                               (using ar: Route.Arity[A])
                               (h: (ar.Out, B) => R ! Async)
                               (using sb: okay.codec.Schema[B], sr: okay.codec.Schema[R]): Router =
    jsonOutAt[A, B, R](method, route, status, description)((a, b, _) => h(a, b))

  /** the same, with the request in hand */
  def jsonOutAt[A <: Tuple, B, R](method: Method, route: Routed[A], status: Int = 200,
                                  description: String = "the declared answer")
                                 (using ar: Route.Arity[A])
                                 (h: (ar.Out, B, Request) => R ! Async)
                                 (using sb: okay.codec.Schema[B], sr: okay.codec.Schema[R]): Router =
    new Router(entries :+ new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r =>
        if r.method != method then None
        else route.unapply(r.url).map { a =>
          okay.codec.Codecs.json(sb).decode(
            okay.codec.Json.parse(new String(r.body.bytes, java.nio.charset.StandardCharsets.UTF_8))) match
            case Right(b) => h(ar(a), b, r).map(Router.encoded(status, _))
            case Left(why) => pure(Router.badRequest(why))
        },
      Some(okay.codec.JsonSchema.of(sb)),
      Vector(Router.Answer(status, Some(okay.codec.JsonSchema.of(sr)), description),
             Router.badRequestAnswer)))

  /**
   * A DECLARED ANSWER THAT IS NOT JSON (openapi-media).
   *
   * `out` declares by construction because the router encodes: the
   * entry carries `JsonSchema.of[R]` and the answer is written with
   * the same `Schema[R]`, so a document cannot promise one thing
   * while the service sends another. Nothing about that argument is
   * specific to JSON — it needs only that the ROUTER, and not the
   * handler, decides what goes on the wire.
   *
   * So these three take the same shape for the media that has no
   * `Schema` at all. The handler answers the CONTENT — a page's text,
   * a bundle's bytes, a stream's chunks — and the router writes the
   * content-type from the same value the entry declares. A handler
   * that builds its own `Response` is still allowed and still
   * declares nothing; what changes is that it no longer has to.
   *
   * The demo's six operations were the argument for this: every one
   * of them answered HTML, an event stream or a byte bundle, so its
   * committed document said `undeclared` six times while the service
   * answered perfectly well-defined content.
   */
  def html[A <: Tuple](method: Method, route: Routed[A], status: Int = 200,
                       description: String = "an HTML page")
                      (using ar: Route.Arity[A])
                      (h: ar.Out => String ! Async): Router =
    htmlAt[A](method, route, status, description)((a, _) => h(a))

  /** the same, with the request in hand */
  def htmlAt[A <: Tuple](method: Method, route: Routed[A], status: Int = 200,
                         description: String = "an HTML page")
                        (using ar: Route.Arity[A])
                        (h: (ar.Out, Request) => String ! Async): Router =
    // the charset is declared because the ROUTER did the encoding: it
    // took a `String` and wrote UTF-8 bytes, so it can say so. Where
    // the handler hands over bytes, nobody here knows, and the header
    // stays the bare media type
    media[A](method, route, Router.textHtml, status, description,
             contentType = Some(Router.textHtml + "; charset=utf-8"))(
      (a, r) => h(a, r).map(t => Http.one(t.getBytes(java.nio.charset.StandardCharsets.UTF_8))))

  /**
   * bytes under a media type the author names: a bundle, an image, a
   * PDF — anything whose content-type is known and whose shape is not
   * a schema's business
   */
  def bytes[A <: Tuple](method: Method, route: Routed[A], media: String, status: Int = 200,
                        description: String = "a body of the declared media type")
                       (using ar: Route.Arity[A])
                       (h: ar.Out => Array[Byte] ! Async): Router =
    bytesAt[A](method, route, media, status, description)((a, _) => h(a))

  /** the same, with the request in hand */
  def bytesAt[A <: Tuple](method: Method, route: Routed[A], media: String, status: Int = 200,
                          description: String = "a body of the declared media type")
                         (using ar: Route.Arity[A])
                         (h: (ar.Out, Request) => Array[Byte] ! Async): Router =
    this.media[A](method, route, media, status, description)(
      (a, r) => h(a, r).map(Http.one))

  /**
   * a server-sent-event stream.
   *
   * The handler answers the SOURCE and not a `Response`, so the one
   * header that makes a stream a stream is written in one place. What
   * OpenAPI can say about it is the media type; the event NAMES a
   * client should listen for are not its vocabulary, so they belong
   * in the description rather than in a schema that pretends.
   */
  def events[A <: Tuple](method: Method, route: Routed[A], status: Int = 200,
                         description: String = "a server-sent-event stream, held open")
                        (using ar: Route.Arity[A])
                        (h: ar.Out => Source[Chunk[Byte]] ! Async): Router =
    eventsAt[A](method, route, status, description)((a, _) => h(a))

  /** the same, with the request in hand */
  def eventsAt[A <: Tuple](method: Method, route: Routed[A], status: Int = 200,
                           description: String = "a server-sent-event stream, held open")
                          (using ar: Route.Arity[A])
                          (h: (ar.Out, Request) => Source[Chunk[Byte]] ! Async): Router =
    media[A](method, route, Router.eventStream, status, description)(h)

  /**
   * what the three above are: the handler answers a BODY, the router
   * writes the header and the entry declares the same media.
   *
   * Public because the list of media types is not ours to close — a
   * service that answers `application/xml` or an image gets the same
   * property without waiting for a combinator to be added here.
   */
  def media[A <: Tuple](method: Method, route: Routed[A], media: String, status: Int, description: String,
                        /** what goes on the wire, when that is more than the
                         * media type: a charset the router itself applied.
                         * The DECLARATION stays bare either way, which is
                         * what `TestRouterMedia`'s law checks. `None` sends the
                         * media type as it stands — the default cannot be
                         * written as `= media`, since a default in the same
                         * parameter list reads the enclosing METHOD of that
                         * name, not the parameter. */
                        contentType: Option[String] = None)
                       (using ar: Route.Arity[A])
                       (h: (ar.Out, Request) => Source[Chunk[Byte]] ! Async): Router =
    new Router(entries :+ new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r =>
        if r.method != method then None
        else route.unapply(r.url).map(a =>
          h(ar(a), r).map(src =>
            Response(status, Seq("content-type" -> contentType.getOrElse(media)), src))),
      None,
      Vector(Router.Answer(status, None, description, media))))

  /** the same, reading a case class */
  def of[C <: Product, A <: Tuple](method: Method, route: Route.Of[C, A])(h: C => Response ! Async): Router =
    ofAt(method, route)((c, _) => h(c))

  def ofAt[C <: Product, A <: Tuple](method: Method, route: Route.Of[C, A])
                                    (h: (C, Request) => Response ! Async): Router =
    new Router(entries :+ new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r => if r.method != method then None else route.unapply(r.url).map(c => h(c, r))))

  /**
   * THE TABLE, ENFORCING WHAT IT DECLARES (specs/route-headers.md,
   * stage B).
   *
   * `Secure.granted` wraps the finished `PartialFunction`, which is
   * why a protected route could never be described: the requirement
   * was applied AFTER the table was built and never reached an entry.
   * Here the declaration is on the entry and this reads it, so the
   * document and the behaviour come from one value. The law is set
   * equality — `enforcing` refuses exactly the entries whose
   * `security` is non-empty, which is exactly the set a renderer
   * calls protected.
   *
   * **Protection does not change WHICH requests a route answers, only
   * who gets through.** `matches` is untouched, so a secured route
   * still MATCHES a request with no credential and answers 401 — the
   * opposite of a declared required HEADER, which is a miss. A route
   * that missed instead would answer 404 to everyone without a token:
   * it leaks less and lies more, and it breaks the invariant
   * `Secure.bearer` states in its own comment.
   *
   * The handler is NOT run for a refused request. Definedness is
   * `matches`, never `run` — the distinction that cost a one-time
   * login code its 401 in stage 7.
   *
   * `verify` is the DEPLOYMENT's, not the route's: a declaration says
   * what is required, a table says what to check it with.
   * `okay.security.Secure.verifier` adapts a `String => Verified`.
   */
  def enforcing(verify: Router.Verify): Router =
    new Router(entries.map { e =>
      if e.security.isEmpty then e
      else
        val realm = e.security.head.realm
        val wanted = e.security.flatMap(_.scopes).toSet
        e.guarded(
          r =>
            if !e.matches(r) then None
            else Router.bearerToken(r) match
              case None => Some(pure(Router.challenge(401, realm, "no token")))
              case Some(t) => verify(t) match
                case Left(_) =>
                  // the WHY stays server-side: a uniform refusal tells
                  // an attacker nothing about how close the token was
                  Some(pure(Router.challenge(401, realm, "invalid_token")))
                case Right(scopes) if !wanted.subsetOf(scopes) =>
                  Some(pure(Router.challenge(403, realm, "insufficient_scope")))
                case Right(_) => e.run(r))
    })

  /** the existing convention, unchanged: a miss is simply undefined,
   * so the caller's 404 stays the caller's */
  def routes: PartialFunction[Request, Response ! Async] =
    // FAIL CLOSED. A secured entry with no verifier installed does not
    // serve: declaring a requirement and forgetting `enforcing` would
    // open a hole the document swears is shut, and a route that
    // suddenly 401s everywhere is a loud mistake rather than a silent
    // one.
    val answer: Router.Entry => Request => Option[Response ! Async] = e =>
      if e.security.isEmpty || e.enforced then e.run
      else r =>
        if !e.matches(r) then None
        else Some(pure(Router.challenge(401, e.security.head.realm, "no_verifier")))
    new PartialFunction[Request, Response ! Async]:
      def isDefinedAt(r: Request): Boolean = entries.exists(_.matches(r))
      def apply(r: Request): Response ! Async =
        entries.iterator.map(e => answer(e)(r)).collectFirst { case Some(x) => x }
          .getOrElse(throw MatchError(r))
      override def applyOrElse[R <: Request, B >: Response ! Async](r: R, other: R => B): B =
        entries.iterator.map(e => answer(e)(r)).collectFirst { case Some(x) => x }.getOrElse(other(r))

  /** every entry, from the same values that dispatch */
  def describe: Vector[(Method, String)] = entries.map(e => (e.method, e.path))

  /**
   * The table as markdown, for a module's documentation
   * (specs/optics-outside.md, stage 8).
   *
   * The point is not the rendering, it is the DRIFT TEST it makes
   * possible: a doc block generated from the entries that dispatch can
   * be asserted against the file that ships, so a route cannot be
   * served and undocumented, or documented and unserved. That is the
   * property `describe` was built for, and this is the first consumer
   * that makes it load-bearing — the repository already keeps
   * deployment renderings honest the same way.
   *
   * A declared request body shows as its type's name rather than its
   * schema: the table is a map of the surface, and a reader who needs
   * the shape has the type to look at.
   */
  def markdown: String =
    val rows = entries.map { e =>
      val body = e.body.fold("—")(_ => "yes")
      s"| `${e.method.name}` | `${e.path}` | $body |"
    }
    ("| verb | path | body |" +: "|---|---|---|" +: rows).mkString("\n")

object Router:
  /** the zero: what a fold over several tables starts from, and what a
   * module answers when it contributes no routes */
  val empty: Router = new Router(Vector.empty)

  /** start a table from the companion, as `Route / "users"` starts a
   * path — so a declaration never opens with `.empty.` */
  def on[A <: Tuple](method: Method, route: Routed[A])(using ar: Route.Arity[A])
                    (h: ar.Out => Response ! Async): Router =
    empty.on(method, route)(h)

  def at[A <: Tuple](method: Method, route: Routed[A])(using ar: Route.Arity[A])
                    (h: (ar.Out, Request) => Response ! Async): Router =
    empty.at(method, route)(h)

  def on[A <: Tuple, Hs <: Tuple](method: Method, route: Headed[A, Hs])
                                 (using ar: Route.Arity[A], hr: Route.Arity[Hs])
                                 (h: (ar.Out, hr.Out) => Response ! Async): Router =
    empty.on(method, route)(h)

  def at[A <: Tuple, Hs <: Tuple](method: Method, route: Headed[A, Hs])
                                 (using ar: Route.Arity[A], hr: Route.Arity[Hs])
                                 (h: (ar.Out, hr.Out, Request) => Response ! Async): Router =
    empty.at(method, route)(h)

  def json[A <: Tuple, B](method: Method, route: Routed[A])(using ar: Route.Arity[A])
                         (h: (ar.Out, B) => Response ! Async)
                         (using okay.codec.Schema[B]): Router =
    empty.json(method, route)(h)

  def jsonAt[A <: Tuple, B](method: Method, route: Routed[A])(using ar: Route.Arity[A])
                           (h: (ar.Out, B, Request) => Response ! Async)
                           (using okay.codec.Schema[B]): Router =
    empty.jsonAt(method, route)(h)

  def of[C <: Product, A <: Tuple](method: Method, route: Route.Of[C, A])(h: C => Response ! Async): Router =
    empty.of(method, route)(h)

  /** the value-answering forms from the companion too, so every
   * declaring shape can begin a table (openapi-prose found the gap:
   * `out` was reachable only through `Router.empty`) */
  def out[A <: Tuple, R](method: Method, route: Routed[A], status: Int = 200,
                         description: String = "the declared answer")
                        (using ar: Route.Arity[A])
                        (h: ar.Out => R ! Async)(using okay.codec.Schema[R]): Router =
    empty.out(method, route, status, description)(h)

  def outAt[A <: Tuple, R](method: Method, route: Routed[A], status: Int = 200,
                           description: String = "the declared answer")
                          (using ar: Route.Arity[A])
                          (h: (ar.Out, Request) => R ! Async)(using okay.codec.Schema[R]): Router =
    empty.outAt(method, route, status, description)(h)

  def jsonOut[A <: Tuple, B, R](method: Method, route: Routed[A], status: Int = 200,
                                description: String = "the declared answer")
                               (using ar: Route.Arity[A])
                               (h: (ar.Out, B) => R ! Async)
                               (using okay.codec.Schema[B], okay.codec.Schema[R]): Router =
    empty.jsonOut(method, route, status, description)(h)

  def jsonOutAt[A <: Tuple, B, R](method: Method, route: Routed[A], status: Int = 200,
                                  description: String = "the declared answer")
                                 (using ar: Route.Arity[A])
                                 (h: (ar.Out, B, Request) => R ! Async)
                                 (using okay.codec.Schema[B], okay.codec.Schema[R]): Router =
    empty.jsonOutAt(method, route, status, description)(h)

  /** a page, from the companion — `Router.html(Get, Route.root) { … }`
   * is how a small service's whole table begins (openapi-media) */
  def html[A <: Tuple](method: Method, route: Routed[A], status: Int = 200,
                       description: String = "an HTML page")
                      (using ar: Route.Arity[A])
                      (h: ar.Out => String ! Async): Router =
    empty.html(method, route, status, description)(h)

  def htmlAt[A <: Tuple](method: Method, route: Routed[A], status: Int = 200,
                         description: String = "an HTML page")
                        (using ar: Route.Arity[A])
                        (h: (ar.Out, Request) => String ! Async): Router =
    empty.htmlAt(method, route, status, description)(h)

  def bytes[A <: Tuple](method: Method, route: Routed[A], media: String, status: Int = 200,
                        description: String = "a body of the declared media type")
                       (using ar: Route.Arity[A])
                       (h: ar.Out => Array[Byte] ! Async): Router =
    empty.bytes(method, route, media, status, description)(h)

  def bytesAt[A <: Tuple](method: Method, route: Routed[A], media: String, status: Int = 200,
                          description: String = "a body of the declared media type")
                         (using ar: Route.Arity[A])
                         (h: (ar.Out, Request) => Array[Byte] ! Async): Router =
    empty.bytesAt(method, route, media, status, description)(h)

  def events[A <: Tuple](method: Method, route: Routed[A], status: Int = 200,
                         description: String = "a server-sent-event stream, held open")
                        (using ar: Route.Arity[A])
                        (h: ar.Out => Source[Chunk[Byte]] ! Async): Router =
    empty.events(method, route, status, description)(h)

  def eventsAt[A <: Tuple](method: Method, route: Routed[A], status: Int = 200,
                           description: String = "a server-sent-event stream, held open")
                          (using ar: Route.Arity[A])
                          (h: (ar.Out, Request) => Source[Chunk[Byte]] ! Async): Router =
    empty.eventsAt(method, route, status, description)(h)

  def media[A <: Tuple](method: Method, route: Routed[A], media: String, status: Int, description: String,
                        contentType: Option[String] = None)
                       (using ar: Route.Arity[A])
                       (h: (ar.Out, Request) => Source[Chunk[Byte]] ! Async): Router =
    empty.media(method, route, media, status, description, contentType)(h)


  def ofAt[C <: Product, A <: Tuple](method: Method, route: Route.Of[C, A])
                                    (h: (C, Request) => Response ! Async): Router =
    empty.ofAt(method, route)(h)

  /**
   * One row of the table: whether it MATCHES, and what it answers.
   *
   * The two are separate because a `PartialFunction`'s `isDefinedAt`
   * must not run the handler. The first cut kept a single
   * `Request => Option[Response ! Async]` and built `routes` with
   * `Function.unlift`, so `isDefinedAt` called the handler to find out
   * whether it matched — harmless while every handler in the tree
   * merely BUILT a program, and a real defect the moment one did work
   * outside it: okay-demo's `/login/confirm` spends a one-time code,
   * so an `isDefinedAt` followed by an `apply` spent it twice and
   * answered 401 to a correct code (optics-outside stage 7).
   */
  /**
   * What an operation ANSWERS, declared (openapi-responses): a status
   * and, when there is one, the JSON Schema of what it sends.
   *
   * There are two ways for this to be true rather than hopeful. A
   * handler that answers a VALUE declares by its own type — the
   * router encodes with the same `Schema` the entry carries, so the
   * declaration cannot drift from what is sent. And the router
   * declares the failures IT produces itself: `json[B]` answers 400
   * with `{"error": …}` when a body does not parse, so the entry says
   * so without the author writing anything.
   *
   * A handler that still builds its own `Response` declares nothing,
   * and a renderer says exactly that rather than inventing a 200.
   */
  final case class Answer(status: Int, schema: Option[okay.codec.Json], description: String,
                         /**
                          * the media type it answers, BARE — `text/html`, not
                          * `text/html; charset=utf-8` (openapi-media).
                          *
                          * Bare because this is the key a document files the
                          * answer under, and a charset is a wire detail of one
                          * response rather than a kind of content. The router
                          * adds the charset when it writes the header, which is
                          * also why the two cannot disagree: both come from
                          * this one value.
                          */
                         media: String = "application/json",
                         /**
                          * the headers this answer carries
                          * (specs/route-headers.md, stage C).
                          *
                          * TWO KINDS, and blurring them would be the whole
                          * mistake. What the ROUTER sends is true by
                          * construction — the 401 a secured route answers
                          * declares `www-authenticate`, and `challenge` is
                          * what writes it, from this same value. What an
                          * AUTHOR declares with `answering` is DESCRIPTION:
                          * the handler builds its own `Response` and nothing
                          * here checks the claim, because failing a request
                          * over a documentation slip would be worse than the
                          * slip.
                          */
                         headers: Vector[Route.Hdr] = Vector.empty)

  final class Entry private[http] (val method: Method,
                                   /** the url's whole description — template AND
                                    * parameters; see `Route.Described` for why the
                                    * two may not be passed apart */
                                   val described: Route.Described,
                                   private[http] val matches: Request => Boolean,
                                   private[http] val run: Request => Option[Response ! Async],
                                   /** the declared body's JSON Schema, when there is one —
                                    * what a renderer reads, as `Toolbox` already gives tools */
                                   val body: Option[okay.codec.Json] = None,
                                   /** what it answers, when the handler's type said so */
                                   val answers: Vector[Answer] = Vector.empty,
                                   /**
                                    * one sentence about what this operation is FOR
                                    * (openapi-prose).
                                    *
                                    * The only part of an entry that cannot be derived
                                    * from something already written: a path comes from
                                    * the route, a parameter's kind from its `Param`, an
                                    * answer from the handler's type. This is the
                                    * author's, or it is absent — and absent is a fine
                                    * answer, which is why it is an `Option` and not an
                                    * empty string pretending to be prose.
                                    */
                                   val summary: Option[String] = None,
                                   /**
                                    * has a verifier been installed for what this
                                    * entry REQUIRES (`Router.enforcing`)?
                                    *
                                    * A secured entry that has none FAILS CLOSED —
                                    * it answers 401 rather than serving. Declaring
                                    * a requirement and forgetting to enforce it
                                    * would otherwise open a hole the document
                                    * swears is shut, which is worse than having no
                                    * declaration at all.
                                    */
                                   private[http] val enforced: Boolean = false):

    /** the same entry, with headers declared on one of its answers —
     * or a new answer, when the status had none */
    private[http] def answeringWith(status: Int, hs: Vector[Route.Hdr]): Entry =
      val updated =
        if answers.exists(_.status == status) then
          answers.map(a => if a.status == status then a.copy(headers = a.headers ++ hs) else a)
        else answers :+ Answer(status, None, "declared", headers = hs)
      new Entry(method, described, matches, run, body, updated, summary, enforced)

    private[http] def saying(text: String): Entry =
      new Entry(method, described, matches, run, body, answers, Some(text), enforced)

    /** the same entry with its answer guarded — everything else kept,
     * which is why this is a method and not five call sites */
    private[http] def guarded(g: Request => Option[Response ! Async]): Entry =
      new Entry(method, described, matches, g, body, answers, summary, enforced = true)

    /** the path template that dispatches — the query is not part of it */
    def path: String = described.path
    /** the path parameters, each with its kind and its JSON Schema */
    def params: Vector[Route.Seg.Var] = described.params
    /** the query parameters this route declares */
    def queries: Vector[Route.Q] = described.queries
    /** the request headers this route declares (specs/route-headers.md) */
    def headers: Vector[Route.Hdr] = described.headers
    /** what it requires of a caller (stage B); empty is "nothing" */
    def security: Vector[Route.Security] = described.security

  /**
   * A DEPLOYMENT'S VERIFIER, as a function rather than a type.
   *
   * okay-security depends on okay-http, so `Verified` and `Policy`
   * cannot be named here and okay-http must not grow an identity
   * model of its own. This is the whole seam: a bearer token in, and
   * either the scopes it grants or a refusal.
   * `okay.security.Secure.verifier` adapts `String => Verified` to it.
   */
  type Verify = String => Either[String, Set[String]]

  /** `Authorization: Bearer <token>`, case-insensitively, and nothing
   * else — a scheme this table does not know is no credential */
  private[http] def bearerToken(r: Request): Option[String] =
    r.headers.collectFirst {
      case (k, v) if k.equalsIgnoreCase("authorization")
        && v.length > 7 && v.take(7).equalsIgnoreCase("bearer ") => v.drop(7)
    }

  private[http] def challenge(status: Int, realm: String, error: String): Response =
    Response(status,
      Seq(("www-authenticate", s"""Bearer realm="$realm", error="$error"""")),
      Http.one(Array.emptyByteArray))

  /** the shape of the router's own error answer — declared, because
   * the router produces it whether or not the author thought about it */
  private[http] val errorSchema: okay.codec.Json =
    okay.codec.Json.JObj(Vector(
      "type" -> okay.codec.Json.JStr("object"),
      "properties" -> okay.codec.Json.JObj(Vector(
        "error" -> okay.codec.Json.JObj(Vector("type" -> okay.codec.Json.JStr("string"))))),
      "required" -> okay.codec.Json.JArr(Vector(okay.codec.Json.JStr("error")))))

  private[http] val badRequestAnswer: Answer =
    Answer(400, Some(errorSchema), "the body did not parse; the answer names what was wrong")

  /**
   * What a SECURED route answers, without the author writing it —
   * the same way `json[B]`'s 400 already appears.
   *
   * 401 and 403 are different facts and the ladder keeps them apart:
   * 401 is "I do not know who you are", 403 is "I know, and no". The
   * WHY of a 401 stays server-side — a uniform refusal tells an
   * attacker nothing about how close a token was.
   */
  private[http] def securityAnswers(d: Route.Described): Vector[Answer] =
    if d.security.isEmpty then Vector.empty
    else Vector(
      Answer(401, None, "no credential, or one that did not verify",
        headers = Vector(challengeHeader)),
      Answer(403, None, "verified, and not permitted",
        headers = Vector(challengeHeader)))

  /** declared because `challenge` writes it — the document and the
   * wire read one value, not two that agree by care */
  private[http] val challengeHeader: Route.Hdr =
    Route.Hdr("www-authenticate", "string", required = true, repeated = false,
      Route.Param.schemaOf("string"))

  private[http] val textHtml: String = "text/html"
  private[http] val eventStream: String = "text/event-stream"

  /** the declared answer, encoded by the schema the entry carries */
  private[http] def encoded[R](status: Int, r: R)(using okay.codec.Schema[R]): Response =
    Response(status, Seq("content-type" -> "application/json"),
      Http.one(okay.codec.Codecs.writeJson(r).getBytes(java.nio.charset.StandardCharsets.UTF_8)))

  /** a request that does not decode is answered with data, never an
   * exception: the caller can see what was wrong with what they sent */
  private[http] def badRequest(why: String): Response =
    Response(400, Seq("content-type" -> "application/json"),
      Http.one(okay.codec.Json.print(
        okay.codec.Json.JObj(Vector("error" -> okay.codec.Json.JStr(why)))).getBytes(
          java.nio.charset.StandardCharsets.UTF_8)))
