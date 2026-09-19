package okay.http

import okay.*
import okay.http.syntax.*

/**
 * A REQUEST HEADER, declared (specs/route-headers.md, stage A).
 *
 * The law the url half rests on is `unapply(url(a)) == Some(a)`, and a
 * header cannot join that tuple: `url` would have nowhere to put it.
 * So the header half is a different type with a law of the same shape,
 * `readHeaders(requestWith(h)) == Some(h)`, and these tests hold both
 * ends of it — including the part that says the url half did not move.
 */
class TestRouteHeaders extends munit.FunSuite:

  private val blank: Response ! Async =
    pure(Response(200, Nil, Http.one(Array.empty[Byte])))

  val events = Route / "events" :@ "last-event-id".opt[Long]
  val session = Route / "mcp" :@ "mcp-session-id".as[String]
  val tagged = Route / "t" :@ "x-tag".all[String]

  test("THE LAW: what a request carries is what the declaration reads") {
    assertEquals(session.readHeaders(Request.get("/mcp", Seq("mcp-session-id" -> "s1"))),
      Some(Tuple1("s1")))
    assertEquals(events.readHeaders(Request.get("/events", Seq("last-event-id" -> "41"))),
      Some(Tuple1(Some(41L))))
    assertEquals(tagged.readHeaders(
      Request.get("/t", Seq("x-tag" -> "a", "x-tag" -> "b"))), Some(Tuple1(Vector("a", "b"))))
  }

  test("header names are case-insensitive on the wire, whatever the declaration wrote") {
    val declared = Route / "mcp" :@ "Last-Event-ID".as[Long]
    assertEquals(declared.readHeaders(Request.get("/mcp", Seq("last-event-id" -> "7"))),
      Some(Tuple1(7L)))
    assertEquals(declared.readHeaders(Request.get("/mcp", Seq("LAST-EVENT-ID" -> "7"))),
      Some(Tuple1(7L)))
  }

  test("a REQUIRED header that is absent is a MISS, not a 400") {
    // the router cannot know that no other route would have matched,
    // so it does not answer for one
    val r = Router.on(Method.Get, session)((_, id) => { val _ = id; blank })
    assert(!r.routes.isDefinedAt(Request.get("/mcp")))
    assert(r.routes.isDefinedAt(Request.get("/mcp", Seq("mcp-session-id" -> "s"))))
  }

  test("optional is absent-tolerant; present and unparseable is still a miss") {
    assertEquals(events.readHeaders(Request.get("/events")), Some(Tuple1(None)))
    // `abc` on a Long meant something and got it wrong — the same rule
    // `Query.opt` states for a query parameter
    assertEquals(events.readHeaders(Request.get("/events", Seq("last-event-id" -> "abc"))), None)
  }

  test("the handler is handed the header, and arity 1 collapses there too") {
    var seen: Any = null
    val r = Router.on(Method.Get, session)((_, id) => { seen = id; blank })
    val _ = r.routes(Request.get("/mcp", Seq("mcp-session-id" -> "abc")))
    assertEquals(seen, "abc")
  }

  test("the URL half did not move: same prism, same law, no header in the template") {
    assertEquals(session.route.unapply("/mcp"), Some(EmptyTuple))
    assertEquals(session.describe, "/mcp")
    assertEquals(session.described.path, "/mcp")
    // the header is described BESIDE the template, never inside it
    assertEquals(session.described.headers.map(_.name), Vector("mcp-session-id"))
    assert(!session.described.path.contains("mcp-session-id"))
  }

  test("the description carries kind, required and repeated, like a query's") {
    assertEquals(events.described.headers.map(_.required), Vector(false))
    assertEquals(tagged.described.headers.map(_.repeated), Vector(true))
    assertEquals(session.described.headers.map(_.kind), Vector("string"))
    assertEquals(events.described.headers.map(_.kind), Vector("long"))
  }

  test("several headers chain, and the tuple grows in declaration order") {
    val both = Route / "x" :@ "a".as[String] :@ "b".as[Long]
    assertEquals(both.readHeaders(Request.get("/x", Seq("a" -> "p", "b" -> "2"))),
      Some(("p", 2L)))
    assertEquals(both.described.headers.map(_.name), Vector("a", "b"))
  }

  test("url parameters and headers arrive together, each in its own tuple") {
    var seen: Any = null
    val byId = Route / "users" / "id".as[Int] :@ "mcp-session-id".as[String]
    val r = Router.on(Method.Get, byId)((id, s) => { seen = (id, s); blank })
    val _ = r.routes(Request.get("/users/7", Seq("mcp-session-id" -> "z")))
    assertEquals(seen, (7, "z"))
  }
