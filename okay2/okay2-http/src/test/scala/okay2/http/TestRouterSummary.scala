package okay2.http

import okay2.pure
import okay2.async.Async

/** the operation says what it is for (okay-http's TestRouterSummary): the
 * one thing a router cannot derive, attached to the entry just declared */
class TestRouterSummary extends munit.FunSuite {

  val board = Route / "board"
  private def page(s: String) = pure[Async, String](s)

  test("a summary attaches to the entry just declared") {
    val r = Router.html(Method.Get, Route.root)(_ => page("<p>hi</p>")).summarised("the home page")
      .html(Method.Get, board)(_ => page("<p>b</p>")).summarised("the board")
    assertEquals(r.entries.map(_.summary), Vector(Some("the home page"), Some("the board")))
  }

  test("an entry nobody summarised has none, and that is not an error") {
    val r = Router.on(Method.Get, board)(_ => pure[Async, Response](Response(200, Seq.empty, Http.one(Array.empty))))
    assertEquals(r.entries.head.summary, None)
  }

  test("summarising nothing is refused where it is written, not where it is served") {
    intercept[IllegalStateException](Router.empty.summarised("about what?")): Unit
  }

  test("a summary does not change what the router dispatches") {
    val plain = Router.html(Method.Get, board)(_ => page("<p>b</p>"))
    val said = plain.summarised("the board")
    assertEquals(said.entries.length, plain.entries.length)
    assert(said.routes.isDefinedAt(Request.get("/board")))
    assertEquals(said.describe, plain.describe)
  }

  test("re-summarising replaces the sentence rather than adding an entry") {
    val r = Router.html(Method.Get, board)(_ => page("<p>b</p>")).summarised("first").summarised("second")
    assertEquals(r.entries.length, 1)
    assertEquals(r.entries.head.summary, Some("second"))
  }
}
