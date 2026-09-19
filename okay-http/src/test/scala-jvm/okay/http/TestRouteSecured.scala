package okay.http

import okay.*
import okay.given
import okay.http.syntax.*

/**
 * A ROUTE DECLARES WHAT IT REQUIRES, AND THE TABLE ENFORCES IT
 * (specs/route-headers.md, stage B).
 *
 * The law: `enforcing` refuses exactly the entries whose `security` is
 * non-empty — which is exactly the set a renderer calls protected. One
 * value, two interpreters, and a test that they agree.
 *
 * JVM-only for one reason: these tests RUN the answers to read a
 * status and a challenge header, and okay-http's cross-platform route
 * tests deliberately never run an Async program.
 */
class TestRouteSecured extends munit.FunSuite:

  private def ok(s: String): Response ! Async =
    pure(Response(200, Nil, Http.one(s.getBytes("UTF-8"))))

  private val admin = (Route / "admin" / "replay").secured("admin")
  private val open = Route / "healthz"

  /** a deployment's verifier: `t:a,b` grants scopes a and b */
  private val verify: Router.Verify = t =>
    if t.startsWith("bad") then Left("invalid")
    else Right(t.split(':').drop(1).headOption.fold(Set.empty[String])(_.split(',').toSet))

  private var ran = 0
  private def table: Router = Router
    .on(Method.Get, open)(_ => ok("live"))
    .on(Method.Post, admin)((_, _) => { ran += 1; ok("replayed") })

  private def status(r: PartialFunction[Request, Response ! Async], req: Request): Int =
    r(req).runWith.status

  private def header(r: PartialFunction[Request, Response ! Async], req: Request, n: String) =
    r(req).runWith.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(n) => v }

  private def post(token: Option[String]) =
    Request.post("/admin/replay", Body.Empty,
      token.map(t => "authorization" -> s"Bearer $t").toSeq)

  test("no credential is 401 — and the route still MATCHES, which is the point") {
    val r = table.enforcing(verify).routes
    // protection must not change WHICH requests a route answers, only
    // who gets through: a miss would be 404 to everyone without a token
    assert(r.isDefinedAt(post(None)))
    assertEquals(status(r, post(None)), 401)
    assertEquals(header(r, post(None), "www-authenticate"),
      Some("""Bearer realm="okay", error="no token""""))
  }

  test("a credential that does not verify is 401, and says no more than that") {
    val r = table.enforcing(verify).routes
    assertEquals(status(r, post(Some("bad:admin"))), 401)
    assertEquals(header(r, post(Some("bad:admin")), "www-authenticate"),
      Some("""Bearer realm="okay", error="invalid_token""""))
  }

  test("verified and not permitted is 403 — a different fact from 401") {
    val r = table.enforcing(verify).routes
    assertEquals(status(r, post(Some("t:reader"))), 403)
  }

  test("verified and permitted runs the handler") {
    val r = table.enforcing(verify).routes
    ran = 0
    assertEquals(status(r, post(Some("t:admin,reader"))), 200)
    assertEquals(ran, 1)
  }

  test("a REFUSED request never runs the handler") {
    val r = table.enforcing(verify).routes
    ran = 0
    val _ = status(r, post(None))
    val _ = status(r, post(Some("bad:admin")))
    val _ = status(r, post(Some("t:reader")))
    assertEquals(ran, 0, "the handler ran for a request that was refused")
  }

  test("FAIL CLOSED: a secured entry with no verifier does not serve") {
    // declaring a requirement and forgetting `enforcing` must not open
    // a hole the document swears is shut
    val r = table.routes
    assert(r.isDefinedAt(post(Some("t:admin"))))
    assertEquals(status(r, post(Some("t:admin"))), 401)
    assertEquals(header(r, post(Some("t:admin")), "www-authenticate"),
      Some("""Bearer realm="okay", error="no_verifier""""))
  }

  test("an UNSECURED route in the same table is untouched, enforced or not") {
    for r <- Seq(table.routes, table.enforcing(verify).routes) do
      assertEquals(status(r, Request.get("/healthz")), 200)
  }

  test("THE LAW: what the table refuses is what the description calls protected") {
    val t = table.enforcing(verify)
    val declared = t.entries.filter(_.security.nonEmpty).map(e => (e.method, e.path)).toSet
    // drive every entry with no credential and see which refuse
    val refused = t.entries.map { e =>
      val req = Request(e.method, e.path.replace("{", "").replace("}", ""), Nil, Body.Empty)
      (e.method, e.path, t.routes.lift(req).map(_.runWith.status))
    }.collect { case (m, p, Some(401)) => (m, p) }.toSet
    assertEquals(refused, declared)
  }

  test("a secured entry declares 401 and 403 without the author writing them") {
    val e = table.entries.find(_.security.nonEmpty).getOrElse(fail("no secured entry"))
    assertEquals(e.answers.map(_.status).sorted, Vector(401, 403))
    assertEquals(e.security.map(_.scopes), Vector(Set("admin")))
  }

  test("the challenge header is DECLARED, and it is the one the wire carries") {
    // stage C: the router writes it from the same value the document
    // reads, so the two cannot disagree
    val e = table.entries.find(_.security.nonEmpty).getOrElse(fail("no secured entry"))
    assertEquals(e.answers.flatMap(_.headers).map(_.name).distinct, Vector("www-authenticate"))
    val sent = header(table.enforcing(verify).routes, post(None), "www-authenticate")
    assert(sent.isDefined, "declared and not sent")
  }

  test("an author's declared response header is DESCRIPTION, and attaches to the status") {
    val r = Router
      .on(Method.Get, Route / "t")(_ => ok("x"))
      .answering(200, "etag".as[String])
    val a = r.entries.head.answers.find(_.status == 200).getOrElse(fail("no 200"))
    assertEquals(a.headers.map(_.name), Vector("etag"))
    // nothing checks the claim: failing a request over a documentation
    // slip would be worse than the slip
    assertEquals(status(r.routes, Request.get("/t")), 200)
  }

  test("answering an empty table throws, here, where the mistake is") {
    intercept[IllegalStateException](Router.empty.answering(200, "etag".as[String]))
  }

  // ---- a declaring route can declare its ANSWER too
  // (route-secured-with-a-value). Every combinator has a `Headed`
  // form now, and each one carries the 401/403 beside what it says
  // itself — an operation that could state a requirement but not a
  // success case read as one that cannot succeed.

  final case class Task(id: Int, title: String) derives okay.codec.Schema
  final case class NewTask(title: String) derives okay.codec.Schema

  test("out: a secured route declares its VALUE beside its refusals") {
    val r = Router.out(Method.Get, (Route / "t" / "id".as[Int]).secured("admin"),
      200, "the task")((id, _) => pure(Task(id, "x")))
    val e = r.entries.head
    assertEquals(e.answers.map(_.status).sorted, Vector(200, 401, 403))
    assertEquals(e.answers.find(_.status == 200).flatMap(_.schema),
      Some(okay.codec.JsonSchema.of(summon[okay.codec.Schema[Task]])))
    // and it still refuses without a credential
    val served = r.enforcing(verify).routes
    assertEquals(status(served, Request.get("/t/7")), 401)
    assertEquals(status(served, Request.get("/t/7", Seq("authorization" -> "Bearer t:admin"))), 200)
  }

  test("jsonOut: a body in, a value out, and the refusals — all declared") {
    val r = Router.jsonOut[EmptyTuple, EmptyTuple, NewTask, Task](
      Method.Post, (Route / "t").secured("admin"), 201, "the task created")(
      (_, _, n) => pure(Task(1, n.title)))
    val e = r.entries.head
    assertEquals(e.answers.map(_.status).sorted, Vector(201, 400, 401, 403))
    assert(e.body.isDefined, "the body schema the decoder was derived from")
  }

  test("events: the shape `last-event-id` was declared for") {
    val resume = Route / "events" :@ "last-event-id".opt[Long]
    var seen: Any = null
    val r = Router.events(Method.Get, resume, 200, "the stream")((_, from) =>
      { seen = from; pure(Http.one(Array.empty[Byte])) })
    assertEquals(r.entries.head.answers.map(_.media), Vector("text/event-stream"))
    val _ = r.routes(Request.get("/events", Seq("last-event-id" -> "41")))
    assertEquals(seen, Some(41L))
  }

  test("bytes and htmlAt reach a declaring route too") {
    val b = Router.bytes(Method.Get, (Route / "b").secured("admin"), "application/pdf",
      200, "a pdf")((_, _) => pure(Array.empty[Byte]))
    assertEquals(b.entries.head.answers.find(_.status == 200).map(_.media), Some("application/pdf"))
    val h = Router.htmlAt(Method.Get, (Route / "h").secured("admin"), 200, "a page")(
      (_, _, _) => pure("<p>x</p>"))
    assertEquals(h.entries.head.answers.find(_.status == 200).map(_.media), Some("text/html"))
  }
