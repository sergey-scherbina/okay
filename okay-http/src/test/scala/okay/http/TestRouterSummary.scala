package okay.http

import okay.*

/**
 * THE OPERATION SAYS WHAT IT IS FOR (openapi-prose).
 *
 * Everything else a router declares is derived from something that
 * already exists: the path from the route, the parameters from their
 * `Param`s, the answer from the handler's type. A sentence about what
 * an operation is FOR cannot be derived from any of them, so it is
 * the one thing an author has to write — and the only question is
 * where to put it so it cannot drift from the entry it describes.
 *
 * It goes on the entry just declared. A builder chain already reads
 * that way, and it keeps twenty combinators from growing a parameter
 * each.
 */
class TestRouterSummary extends munit.FunSuite:

  val board = Route / "board"

  test("a summary attaches to the entry just declared") {
    val r = Router
      .html(Method.Get, Route.root)(_ => pure("<p>hi</p>")).summarised("the home page")
      .html(Method.Get, board)(_ => pure("<p>b</p>")).summarised("the board")
    assertEquals(r.entries.map(_.summary), Vector(Some("the home page"), Some("the board")))
  }

  test("an entry nobody summarised has none, and that is not an error") {
    val r = Router.on(Method.Get, board)(_ => pure(Response(200, Seq.empty, Http.one(Array.empty))))
    assertEquals(r.entries.head.summary, None)
  }

  test("summarising nothing is refused where it is written, not where it is served") {
    // a builder method that silently did nothing would put the
    // sentence on no operation at all and say so nowhere
    intercept[IllegalStateException](Router.empty.summarised("about what?"))
  }

  test("a summary does not change what the router dispatches") {
    val plain = Router.html(Method.Get, board)(_ => pure("<p>b</p>"))
    val said = plain.summarised("the board")
    assertEquals(said.entries.length, plain.entries.length)
    assert(said.routes.isDefinedAt(Request.get("/board")))
    assertEquals(said.describe, plain.describe)
  }

  test("re-summarising replaces the sentence rather than adding an entry") {
    val r = Router.html(Method.Get, board)(_ => pure("<p>b</p>"))
      .summarised("first").summarised("second")
    assertEquals(r.entries.length, 1)
    assertEquals(r.entries.head.summary, Some("second"))
  }
