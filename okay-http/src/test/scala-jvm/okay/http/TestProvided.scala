package okay.http

import okay.*
import okay.freer.*

import okay.given
import okay.freer.given

/**
 * A capability from the request (specs/app-host.md): `Route.provided`
 * for a PartialFunction route, `Router.provided` for one handler of a
 * table — the value is the request's, and it is CAPTURED, so a wrapper
 * that builds the route late, or a run on another thread, still sees it.
 */
class TestProvided extends munit.FunSuite:

  /** what a page is drawn with, read from the request */
  final case class Who(name: String)
  private def who(r: Request): Who = Who(r.headers.collectFirst { case ("x-who", v) => v }.getOrElse("nobody"))

  private def text(res: Response): String =
    new String(Async.run[Chunk[Byte], Pure](Http.bytes(res)).runWith.toArray, "UTF-8")

  private def page(using w: Who): String = s"hello ${w.name}"

  test("Route.provided: two requests get two values; definedness is the inner route's") {
    val route = Route.provided(who) {
      case r if r.url == "/hello" => pure(Response(200, Nil, Http.one(page.getBytes("UTF-8"))))
    }
    assert(route.isDefinedAt(Request.get("/hello")) && !route.isDefinedAt(Request.get("/other")))
    assertEquals(text(route(Request.get("/hello", Seq("x-who" -> "anna"))).runWith), "hello anna")
    assertEquals(text(route(Request.get("/hello", Seq("x-who" -> "bob"))).runWith), "hello bob")
    assertEquals(text(route(Request.get("/hello")).runWith), "hello nobody")
  }

  test("the value is the request's inside a wrapper that builds the route late, as Red and Lifecycle do") {
    val route = Route.provided(who) {
      case r if r.url == "/hello" => pure(Response(200, Nil, Http.one(page.getBytes("UTF-8"))))
    }
    // the wrapper: the route by name, built inside its own effect —
    // when the answer runs, on the runner's thread, not when matched
    def late(inner: => Response ! Async): Response ! Async = okay.async(()).flatMap(_ => inner)
    val wrapped: PartialFunction[Request, Response ! Async] =
      case r if route.isDefinedAt(r) => late(route(r))
    val effect = wrapped(Request.get("/hello", Seq("x-who" -> "anna")))
    // run apart from matching, on another thread
    val got = java.util.concurrent.CompletableFuture.supplyAsync(() => text(effect.runWith)).get
    assertEquals(got, "hello anna")
  }

  test("Router.provided: one handler of a table, the capability captured — read after the handler returned, elsewhere") {
    val table = Router.htmlAt(Method.Get, Route.lit("hello"))(Router.provided(who)((_, _) => pure(page)))
    val effect = table.routes(Request.get("/hello", Seq("x-who" -> "anna")))
    val got = java.util.concurrent.CompletableFuture.supplyAsync(() => text(effect.runWith)).get
    assertEquals(got, "hello anna")
    assertEquals(text(table.routes(Request.get("/hello")).runWith), "hello nobody")
  }
