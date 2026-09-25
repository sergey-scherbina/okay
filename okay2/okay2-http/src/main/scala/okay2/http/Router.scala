package okay2.http

import java.nio.charset.StandardCharsets.UTF_8
import okay2.{!, pure}
import okay2.async.Async
import okay2.codec.{Json, JsonSchema, Schema}
import okay2.stream.{Chunk, Source}

/**
 * A table of routes (okay-http's Router): each entry is a verb, a route,
 * a handler and a DESCRIPTION, so the table that dispatches is the table
 * a document is generated from (`describe`, `markdown`, the entries'
 * answers and bodies).
 *
 * A handler takes the route's capture in its outside form: `Unit`, the
 * value, or the tuple. Scala 3 untuples a lambda over a tuple
 * (`(id, slug) => ...`); Scala 2 does not, so a handler of several
 * captures is `{ case (id, slug) => ... }`.
 */
final class Router private (val entries: Vector[Router.Entry]) {

  /** response headers the operation just declared answers with */
  def answering(status: Int, headers: Route.Named[_]*): Router =
    if (entries.isEmpty)
      throw new IllegalStateException("answering: there is no operation to describe — it attaches to the entry just declared")
    else {
      val hs = headers.toVector.map(n => Route.Hdr(n.name, n.param.kind, required = true, repeated = false, n.param.jsonSchema))
      new Router(entries.init :+ entries.last.answeringWith(status, hs))
    }

  /** one line saying what the operation just declared is for */
  def summarised(text: String): Router =
    if (entries.isEmpty)
      throw new IllegalStateException("summarised: there is no operation to summarise — it attaches to the entry just declared")
    else new Router(entries.init :+ entries.last.saying(text))

  // ---- url routes ----

  def on[A](method: Method, route: Routed[A])(h: A => Response ! Async): Router =
    at[A](method, route)((a, _) => h(a))

  def at[A](method: Method, route: Routed[A])(h: (A, Request) => Response ! Async): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r => if (r.method != method) None else route.unapply(r.url).map(a => h(a, r))))

  def json[A, B](method: Method, route: Routed[A])(h: (A, B) => Response ! Async)(implicit sc: Schema[B]): Router =
    jsonAt[A, B](method, route)((a, b, _) => h(a, b))

  /** a JSON body decoded by its schema; a body that does not decode is a
   * 400 naming what was wrong, and the handler is not called */
  def jsonAt[A, B](method: Method, route: Routed[A])(h: (A, B, Request) => Response ! Async)(implicit sc: Schema[B]): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r =>
        if (r.method != method) None
        else route.unapply(r.url).map(a => Router.body(sc, r) match {
          case Right(b) => h(a, b, r)
          case Left(why) => pure[Async, Response](Router.badRequest(why))
        }),
      Some(JsonSchema.of(sc))).bodyTyped(sc))

  /** the handler answers a VALUE, encoded by its schema, and the entry
   * declares that answer */
  def out[A, R](method: Method, route: Routed[A], status: Int = 200, description: String = "the declared answer")
               (h: A => R ! Async)(implicit sr: Schema[R]): Router =
    outAt[A, R](method, route, status, description)((a, _) => h(a))

  def outAt[A, R](method: Method, route: Routed[A], status: Int = 200, description: String = "the declared answer")
                 (h: (A, Request) => R ! Async)(implicit sr: Schema[R]): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r => if (r.method != method) None else route.unapply(r.url).map(a => h(a, r).map(Router.encoded(status, _))),
      None,
      Vector(Router.Answer(status, Some(JsonSchema.of(sr)), description, tpe = Some(sr)))))

  def jsonOut[A, B, R](method: Method, route: Routed[A], status: Int = 200, description: String = "the declared answer")
                      (h: (A, B) => R ! Async)(implicit sb: Schema[B], sr: Schema[R]): Router =
    jsonOutAt[A, B, R](method, route, status, description)((a, b, _) => h(a, b))

  def jsonOutAt[A, B, R](method: Method, route: Routed[A], status: Int = 200, description: String = "the declared answer")
                        (h: (A, B, Request) => R ! Async)(implicit sb: Schema[B], sr: Schema[R]): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r =>
        if (r.method != method) None
        else route.unapply(r.url).map(a => Router.body(sb, r) match {
          case Right(b) => h(a, b, r).map(Router.encoded(status, _))
          case Left(why) => pure[Async, Response](Router.badRequest(why))
        }),
      Some(JsonSchema.of(sb)),
      Vector(Router.Answer(status, Some(JsonSchema.of(sr)), description, tpe = Some(sr)), Router.badRequestAnswer)).bodyTyped(sb))

  def html[A](method: Method, route: Routed[A], status: Int = 200, description: String = "an HTML page")
             (h: A => String ! Async): Router =
    htmlAt[A](method, route, status, description)((a, _) => h(a))

  def htmlAt[A](method: Method, route: Routed[A], status: Int = 200, description: String = "an HTML page")
               (h: (A, Request) => String ! Async): Router =
    media[A](method, route, Router.textHtml, status, description, contentType = Some(Router.textHtml + "; charset=utf-8"))(
      (a, r) => h(a, r).map(t => Http.one(t.getBytes(UTF_8))))

  def bytes[A](method: Method, route: Routed[A], media: String, status: Int = 200,
               description: String = "a body of the declared media type")(h: A => Array[Byte] ! Async): Router =
    bytesAt[A](method, route, media, status, description)((a, _) => h(a))

  def bytesAt[A](method: Method, route: Routed[A], media: String, status: Int = 200,
                 description: String = "a body of the declared media type")(h: (A, Request) => Array[Byte] ! Async): Router =
    this.media[A](method, route, media, status, description)((a, r) => h(a, r).map(Http.one))

  def events[A](method: Method, route: Routed[A], status: Int = 200,
                description: String = "a server-sent-event stream, held open")(h: A => Source[Chunk[Byte]] ! Async): Router =
    eventsAt[A](method, route, status, description)((a, _) => h(a))

  def eventsAt[A](method: Method, route: Routed[A], status: Int = 200,
                  description: String = "a server-sent-event stream, held open")(h: (A, Request) => Source[Chunk[Byte]] ! Async): Router =
    media[A](method, route, Router.eventStream, status, description)(h)

  /** a body of a declared media type, streamed as the handler answers it */
  def media[A](method: Method, route: Routed[A], media: String, status: Int, description: String,
               contentType: Option[String] = None)(h: (A, Request) => Source[Chunk[Byte]] ! Async): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r =>
        if (r.method != method) None
        else route.unapply(r.url).map(a => h(a, r).map(src => Response(status, Seq("content-type" -> contentType.getOrElse(media)), src))),
      None,
      Vector(Router.Answer(status, None, description, media))))

  def of[C, A](method: Method, route: Route.Of[C, A])(h: C => Response ! Async): Router = ofAt(method, route)((c, _) => h(c))

  def ofAt[C, A](method: Method, route: Route.Of[C, A])(h: (C, Request) => Response ! Async): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.unapply(r.url).isDefined,
      r => if (r.method != method) None else route.unapply(r.url).map(c => h(c, r))))

  // ---- request-shaped routes: the url and its headers ----

  def on[A, Hs](method: Method, route: Headed[A, Hs])(h: (A, Hs) => Response ! Async): Router =
    at[A, Hs](method, route)((a, hs, _) => h(a, hs))

  def at[A, Hs](method: Method, route: Headed[A, Hs])(h: (A, Hs, Request) => Response ! Async): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.read(r).isDefined,
      r => if (r.method != method) None else route.read(r).map { case (a, hs) => h(a, hs, r) },
      None,
      Router.securityAnswers(route.described)))

  def json[A, Hs, B](method: Method, route: Headed[A, Hs])(h: (A, Hs, B) => Response ! Async)(implicit sc: Schema[B]): Router =
    jsonAt[A, Hs, B](method, route)((a, hs, b, _) => h(a, hs, b))

  def jsonAt[A, Hs, B](method: Method, route: Headed[A, Hs])(h: (A, Hs, B, Request) => Response ! Async)(implicit sc: Schema[B]): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.read(r).isDefined,
      r =>
        if (r.method != method) None
        else route.read(r).map { case (a, hs) =>
          Router.body(sc, r) match {
            case Right(b) => h(a, hs, b, r)
            case Left(why) => pure[Async, Response](Router.badRequest(why))
          }
        },
      Some(JsonSchema.of(sc)),
      Router.securityAnswers(route.described)).bodyTyped(sc))

  def out[A, Hs, R](method: Method, route: Headed[A, Hs], status: Int, description: String)
                   (h: (A, Hs) => R ! Async)(implicit sr: Schema[R]): Router =
    outAt[A, Hs, R](method, route, status, description)((a, hs, _) => h(a, hs))

  def outAt[A, Hs, R](method: Method, route: Headed[A, Hs], status: Int, description: String)
                     (h: (A, Hs, Request) => R ! Async)(implicit sr: Schema[R]): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.read(r).isDefined,
      r => if (r.method != method) None else route.read(r).map { case (a, hs) => h(a, hs, r).map(Router.encoded(status, _)) },
      None,
      Router.securityAnswers(route.described) :+ Router.Answer(status, Some(JsonSchema.of(sr)), description, tpe = Some(sr))))

  def jsonOut[A, Hs, B, R](method: Method, route: Headed[A, Hs], status: Int, description: String)
                          (h: (A, Hs, B) => R ! Async)(implicit sb: Schema[B], sr: Schema[R]): Router =
    jsonOutAt[A, Hs, B, R](method, route, status, description)((a, hs, b, _) => h(a, hs, b))

  def jsonOutAt[A, Hs, B, R](method: Method, route: Headed[A, Hs], status: Int, description: String)
                            (h: (A, Hs, B, Request) => R ! Async)(implicit sb: Schema[B], sr: Schema[R]): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.read(r).isDefined,
      r =>
        if (r.method != method) None
        else route.read(r).map { case (a, hs) =>
          Router.body(sb, r) match {
            case Right(b) => h(a, hs, b, r).map(Router.encoded(status, _))
            case Left(why) => pure[Async, Response](Router.badRequest(why))
          }
        },
      Some(JsonSchema.of(sb)),
      Router.securityAnswers(route.described) ++
        Vector(Router.Answer(status, Some(JsonSchema.of(sr)), description, tpe = Some(sr)), Router.badRequestAnswer)).bodyTyped(sb))

  def html[A, Hs](method: Method, route: Headed[A, Hs], status: Int, description: String)(h: (A, Hs) => String ! Async): Router =
    htmlAt[A, Hs](method, route, status, description)((a, hs, _) => h(a, hs))

  def htmlAt[A, Hs](method: Method, route: Headed[A, Hs], status: Int, description: String)
                   (h: (A, Hs, Request) => String ! Async): Router =
    media[A, Hs](method, route, Router.textHtml, status, description, Some(Router.textHtml + "; charset=utf-8"))(
      (a, hs, r) => h(a, hs, r).map(t => Http.one(t.getBytes(UTF_8))))

  def bytes[A, Hs](method: Method, route: Headed[A, Hs], media: String, status: Int, description: String)
                  (h: (A, Hs) => Array[Byte] ! Async): Router =
    bytesAt[A, Hs](method, route, media, status, description)((a, hs, _) => h(a, hs))

  def bytesAt[A, Hs](method: Method, route: Headed[A, Hs], media: String, status: Int, description: String)
                    (h: (A, Hs, Request) => Array[Byte] ! Async): Router =
    this.media[A, Hs](method, route, media, status, description, None)((a, hs, r) => h(a, hs, r).map(Http.one))

  def events[A, Hs](method: Method, route: Headed[A, Hs], status: Int, description: String)
                   (h: (A, Hs) => Source[Chunk[Byte]] ! Async): Router =
    eventsAt[A, Hs](method, route, status, description)((a, hs, _) => h(a, hs))

  def eventsAt[A, Hs](method: Method, route: Headed[A, Hs], status: Int, description: String)
                     (h: (A, Hs, Request) => Source[Chunk[Byte]] ! Async): Router =
    media[A, Hs](method, route, Router.eventStream, status, description, None)(h)

  def media[A, Hs](method: Method, route: Headed[A, Hs], media: String, status: Int, description: String,
                   contentType: Option[String])(h: (A, Hs, Request) => Source[Chunk[Byte]] ! Async): Router =
    add(new Router.Entry(method, route.described,
      r => r.method == method && route.read(r).isDefined,
      r =>
        if (r.method != method) None
        else route.read(r).map { case (a, hs) =>
          h(a, hs, r).map(src => Response(status, Seq("content-type" -> contentType.getOrElse(media)), src))
        },
      None,
      Router.securityAnswers(route.described) :+ Router.Answer(status, None, description, media)))

  /**
   * The declared security, ENFORCED: an entry that requires a credential
   * answers 401 without one or with one `verify` refuses, 403 when its
   * scopes fall short, and runs the handler only past both.
   */
  def enforcing(verify: Router.Verify): Router =
    new Router(entries.map { e =>
      if (e.security.isEmpty) e
      else {
        val realm = e.security.head.realm
        val wanted = e.security.flatMap(_.scopes).toSet
        e.guarded(r =>
          if (!e.matches(r)) None
          else Router.bearerToken(r) match {
            case None => Some(pure[Async, Response](Router.challenge(401, realm, "no token")))
            case Some(t) => verify(t) match {
              case Left(_) => Some(pure[Async, Response](Router.challenge(401, realm, "invalid_token")))
              case Right(scopes) if !wanted.subsetOf(scopes) => Some(pure[Async, Response](Router.challenge(403, realm, "insufficient_scope")))
              case Right(_) => e.run(r)
            }
          })
      }
    })

  def ++(that: Router): Router = new Router(entries ++ that.entries)

  /**
   * The table as a handler. An entry that declares security and was not
   * `enforcing`-ed answers 401 "no_verifier": a declared requirement is
   * never silently open. Candidates come from the trie index, in
   * declaration order.
   */
  def routes: PartialFunction[Request, Response ! Async] = {
    val answer: Router.Entry => Request => Option[Response ! Async] = e =>
      if (e.security.isEmpty || e.enforced) e.run
      else r => if (!e.matches(r)) None else Some(pure[Async, Response](Router.challenge(401, e.security.head.realm, "no_verifier")))
    val ix = index
    new PartialFunction[Request, Response ! Async] {
      def isDefinedAt(r: Request): Boolean = ix.candidates(r).exists(i => entries(i).matches(r))
      def apply(r: Request): Response ! Async =
        ix.candidates(r).iterator.map(i => answer(entries(i))(r)).collectFirst { case Some(x) => x }.getOrElse(throw new MatchError(r))
      override def applyOrElse[R <: Request, B >: Response ! Async](r: R, other: R => B): B =
        ix.candidates(r).iterator.map(i => answer(entries(i))(r)).collectFirst { case Some(x) => x }.getOrElse(other(r))
    }
  }

  private lazy val index: Router.Index = new Router.Index(entries)

  def describe: Vector[(Method, String)] = entries.map(e => (e.method, e.path))

  def markdown: String = {
    val rows = entries.map(e => s"| `${e.method.name}` | `${e.path}` | ${e.body.fold("—")(_ => "yes")} |")
    ("| verb | path | body |" +: "|---|---|---|" +: rows).mkString("\n")
  }

  private def add(e: Router.Entry): Router = new Router(entries :+ e)
}

object Router {
  val empty: Router = new Router(Vector.empty)

  def on[A](method: Method, route: Routed[A])(h: A => Response ! Async): Router = empty.on(method, route)(h)
  def at[A](method: Method, route: Routed[A])(h: (A, Request) => Response ! Async): Router = empty.at(method, route)(h)
  def on[A, Hs](method: Method, route: Headed[A, Hs])(h: (A, Hs) => Response ! Async): Router = empty.on(method, route)(h)
  def at[A, Hs](method: Method, route: Headed[A, Hs])(h: (A, Hs, Request) => Response ! Async): Router = empty.at(method, route)(h)
  def json[A, B](method: Method, route: Routed[A])(h: (A, B) => Response ! Async)(implicit sc: Schema[B]): Router = empty.json(method, route)(h)
  def jsonAt[A, B](method: Method, route: Routed[A])(h: (A, B, Request) => Response ! Async)(implicit sc: Schema[B]): Router =
    empty.jsonAt(method, route)(h)
  def of[C, A](method: Method, route: Route.Of[C, A])(h: C => Response ! Async): Router = empty.of(method, route)(h)
  def ofAt[C, A](method: Method, route: Route.Of[C, A])(h: (C, Request) => Response ! Async): Router = empty.ofAt(method, route)(h)
  def out[A, R](method: Method, route: Routed[A], status: Int = 200, description: String = "the declared answer")
               (h: A => R ! Async)(implicit sr: Schema[R]): Router = empty.out(method, route, status, description)(h)
  def outAt[A, R](method: Method, route: Routed[A], status: Int = 200, description: String = "the declared answer")
                 (h: (A, Request) => R ! Async)(implicit sr: Schema[R]): Router = empty.outAt(method, route, status, description)(h)
  def jsonOut[A, B, R](method: Method, route: Routed[A], status: Int = 200, description: String = "the declared answer")
                      (h: (A, B) => R ! Async)(implicit sb: Schema[B], sr: Schema[R]): Router =
    empty.jsonOut(method, route, status, description)(h)
  def jsonOutAt[A, B, R](method: Method, route: Routed[A], status: Int = 200, description: String = "the declared answer")
                        (h: (A, B, Request) => R ! Async)(implicit sb: Schema[B], sr: Schema[R]): Router =
    empty.jsonOutAt(method, route, status, description)(h)
  def html[A](method: Method, route: Routed[A], status: Int = 200, description: String = "an HTML page")
             (h: A => String ! Async): Router = empty.html(method, route, status, description)(h)
  def htmlAt[A](method: Method, route: Routed[A], status: Int = 200, description: String = "an HTML page")
               (h: (A, Request) => String ! Async): Router = empty.htmlAt(method, route, status, description)(h)
  def bytes[A](method: Method, route: Routed[A], media: String, status: Int = 200,
               description: String = "a body of the declared media type")(h: A => Array[Byte] ! Async): Router =
    empty.bytes(method, route, media, status, description)(h)
  def events[A](method: Method, route: Routed[A], status: Int = 200,
                description: String = "a server-sent-event stream, held open")(h: A => Source[Chunk[Byte]] ! Async): Router =
    empty.events(method, route, status, description)(h)
  def media[A](method: Method, route: Routed[A], media: String, status: Int, description: String,
               contentType: Option[String] = None)(h: (A, Request) => Source[Chunk[Byte]] ! Async): Router =
    empty.media(method, route, media, status, description, contentType)(h)

  // the request-shaped forms, from the companion as well
  def json[A, Hs, B](method: Method, route: Headed[A, Hs])(h: (A, Hs, B) => Response ! Async)(implicit sc: Schema[B]): Router =
    empty.json(method, route)(h)
  def jsonAt[A, Hs, B](method: Method, route: Headed[A, Hs])(h: (A, Hs, B, Request) => Response ! Async)(implicit sc: Schema[B]): Router =
    empty.jsonAt(method, route)(h)
  def out[A, Hs, R](method: Method, route: Headed[A, Hs], status: Int, description: String)
                   (h: (A, Hs) => R ! Async)(implicit sr: Schema[R]): Router = empty.out(method, route, status, description)(h)
  def outAt[A, Hs, R](method: Method, route: Headed[A, Hs], status: Int, description: String)
                     (h: (A, Hs, Request) => R ! Async)(implicit sr: Schema[R]): Router = empty.outAt(method, route, status, description)(h)
  def jsonOut[A, Hs, B, R](method: Method, route: Headed[A, Hs], status: Int, description: String)
                          (h: (A, Hs, B) => R ! Async)(implicit sb: Schema[B], sr: Schema[R]): Router =
    empty.jsonOut(method, route, status, description)(h)
  def jsonOutAt[A, Hs, B, R](method: Method, route: Headed[A, Hs], status: Int, description: String)
                            (h: (A, Hs, B, Request) => R ! Async)(implicit sb: Schema[B], sr: Schema[R]): Router =
    empty.jsonOutAt(method, route, status, description)(h)
  def html[A, Hs](method: Method, route: Headed[A, Hs], status: Int, description: String)(h: (A, Hs) => String ! Async): Router =
    empty.html(method, route, status, description)(h)
  def htmlAt[A, Hs](method: Method, route: Headed[A, Hs], status: Int, description: String)
                   (h: (A, Hs, Request) => String ! Async): Router = empty.htmlAt(method, route, status, description)(h)
  def bytes[A, Hs](method: Method, route: Headed[A, Hs], media: String, status: Int, description: String)
                  (h: (A, Hs) => Array[Byte] ! Async): Router = empty.bytes(method, route, media, status, description)(h)
  def events[A, Hs](method: Method, route: Headed[A, Hs], status: Int, description: String)
                   (h: (A, Hs) => Source[Chunk[Byte]] ! Async): Router = empty.events(method, route, status, description)(h)
  def eventsAt[A, Hs](method: Method, route: Headed[A, Hs], status: Int, description: String)
                     (h: (A, Hs, Request) => Source[Chunk[Byte]] ! Async): Router = empty.eventsAt(method, route, status, description)(h)

  /** one declared answer: its status, body schema, media and headers */
  final case class Answer(status: Int, schema: Option[Json], description: String,
                          media: String = "application/json",
                          headers: Vector[Route.Hdr] = Vector.empty,
                          tpe: Option[Schema[_]] = None)

  final class Entry private[http] (val method: Method,
                                   val described: Route.Described,
                                   private[http] val matches: Request => Boolean,
                                   private[http] val run: Request => Option[Response ! Async],
                                   val body: Option[Json] = None,
                                   val answers: Vector[Answer] = Vector.empty,
                                   val summary: Option[String] = None,
                                   private[http] val enforced: Boolean = false,
                                   val bodyType: Option[Schema[_]] = None) {

    private[http] def bodyTyped(s: Schema[_]): Entry =
      new Entry(method, described, matches, run, body, answers, summary, enforced, Some(s))

    private[http] def answeringWith(status: Int, hs: Vector[Route.Hdr]): Entry = {
      val updated =
        if (answers.exists(_.status == status)) answers.map(a => if (a.status == status) a.copy(headers = a.headers ++ hs) else a)
        else answers :+ Answer(status, None, "declared", headers = hs)
      new Entry(method, described, matches, run, body, updated, summary, enforced, bodyType)
    }

    private[http] def saying(text: String): Entry =
      new Entry(method, described, matches, run, body, answers, Some(text), enforced, bodyType)

    private[http] def guarded(g: Request => Option[Response ! Async]): Entry =
      new Entry(method, described, matches, g, body, answers, summary, enforced = true, bodyType)

    def path: String = described.path
    def params: Vector[Route.Seg.Var] = described.params
    def queries: Vector[Route.Q] = described.queries
    def headers: Vector[Route.Hdr] = described.headers
    def security: Vector[Route.Security] = described.security
  }

  /** a bearer token to the scopes it grants, or why not */
  type Verify = String => Either[String, Set[String]]

  /**
   * The entries by verb, segment count and a trie over literal segments
   * (a `{name}` segment is the wildcard child): a request visits the
   * entries that can match, in declaration order. An entry whose path
   * does not parse as segments is always a candidate.
   */
  private[http] final class Index(entries: Vector[Entry]) {
    private final class Node {
      var lits: Map[String, Node] = Map.empty
      var wild: Node = null
      var leaf: Vector[Int] = Vector.empty
      def lit(seg: String): Node = lits.get(seg) match {
        case Some(n) => n
        case None => val n = new Node; lits = lits.updated(seg, n); n
      }
      def wildcard: Node = { if (wild == null) wild = new Node; wild }
    }

    private val roots: Array[Map[Int, Node]] = Array.fill(Method.values.length)(Map.empty)
    private var always: Vector[Int] = Vector.empty

    entries.zipWithIndex.foreach { case (e, i) =>
      Route.segmentsOf(e.path) match {
        case None => always = always :+ i
        case Some(segs) =>
          val m = Method.values.indexOf(e.method)
          val root = roots(m).get(segs.length) match {
            case Some(n) => n
            case None => val n = new Node; roots(m) = roots(m).updated(segs.length, n); n
          }
          var node = root
          for (seg <- segs)
            node = if (seg.length >= 2 && seg.charAt(0) == '{' && seg.charAt(seg.length - 1) == '}') node.wildcard else node.lit(seg)
          node.leaf = node.leaf :+ i
      }
    }

    /** the walk on an explicit worklist: a node and its depth */
    def candidates(r: Request): Vector[Int] =
      Route.segmentsOf(r.url) match {
        case None => always
        case Some(ss) =>
          roots(Method.values.indexOf(r.method)).get(ss.length) match {
            case None => always
            case Some(root) =>
              val out = scala.collection.mutable.ArrayBuffer.empty[Int]
              var work: List[(Node, Int)] = (root, 0) :: Nil
              while (work.nonEmpty) {
                val (n, d) = work.head
                work = work.tail
                if (d == ss.length) out ++= n.leaf
                else {
                  // order is irrelevant here: the result is sorted back
                  // into declaration order below
                  if (n.wild != null) work = (n.wild, d + 1) :: work
                  n.lits.get(ss(d)).foreach(c => work = (c, d + 1) :: work)
                }
              }
              if (always.nonEmpty) out ++= always
              if (out.length <= 1) out.toVector else out.sortInPlace().toVector
          }
      }
  }

  private[http] def body[B](sc: Schema[B], r: Request): Either[String, B] =
    Json.decode(sc)(Json.parse(new String(r.body.bytes, UTF_8)))

  private[http] def bearerToken(r: Request): Option[String] =
    r.headers.collectFirst {
      case (k, v) if k.equalsIgnoreCase("authorization") && v.length > 7 && v.take(7).equalsIgnoreCase("bearer ") => v.drop(7)
    }

  private[http] def challenge(status: Int, realm: String, error: String): Response =
    Response(status, Seq(("www-authenticate", s"""Bearer realm="$realm", error="$error"""")), Http.one(Array.emptyByteArray))

  private[http] val errorSchema: Json =
    Json.JObj(Vector(
      "type" -> Json.JStr("object"),
      "properties" -> Json.JObj(Vector("error" -> Json.JObj(Vector("type" -> Json.JStr("string"))))),
      "required" -> Json.JArr(Vector(Json.JStr("error")))))

  private[http] val badRequestAnswer: Answer = Answer(400, Some(errorSchema), "the body did not parse; the answer names what was wrong")

  private[http] def securityAnswers(d: Route.Described): Vector[Answer] =
    if (d.security.isEmpty) Vector.empty
    else Vector(
      Answer(401, None, "no credential, or one that did not verify", media = "", headers = Vector(challengeHeader)),
      Answer(403, None, "verified, and not permitted", media = "", headers = Vector(challengeHeader)))

  private[http] val challengeHeader: Route.Hdr =
    Route.Hdr("www-authenticate", "string", required = true, repeated = false, Route.Param.schemaOf("string"))

  private[http] val textHtml: String = "text/html"
  private[http] val eventStream: String = "text/event-stream"

  private[http] def encoded[R](status: Int, r: R)(implicit s: Schema[R]): Response =
    Response(status, Seq("content-type" -> "application/json"), Http.one(Json.write(r).getBytes(UTF_8)))

  private[http] def badRequest(why: String): Response =
    Response(400, Seq("content-type" -> "application/json"),
      Http.one(Json.print(Json.JObj(Vector("error" -> Json.JStr(why)))).getBytes(UTF_8)))
}
