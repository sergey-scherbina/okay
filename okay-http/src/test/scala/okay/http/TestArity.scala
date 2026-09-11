package okay.http

import okay.*

/**
 * ARITY 1 COLLAPSES AT THE HANDLER, and nowhere else
 * (route-arity-one-tuple).
 *
 * A route's `A` is a tuple because a path may capture any number of
 * things, and the OPTIC keeps it: `unapply` answers `Some(Tuple1(7))`
 * and `url` takes one, because the laws are stated over `A`. What
 * changed is the boundary where a PERSON writes code.
 *
 * Four sightings opened this, three by authors other than the one who
 * wrote the backlog entry, and one of them a comment in okay-demo
 * saying `.head` is "the spelling until it has a better one".
 */
class TestArity extends munit.FunSuite:

  private val blank: Response ! Async =
    pure(Response(200, Nil, Http.one(Array.empty[Byte])))

  test("the witness collapses arity 1, and leaves 0 and 2 alone") {
    // the ASCRIPTIONS are the test: if `whole` had answered for arity
    // 1, `val got: Int` would not typecheck. A summon proves an
    // instance exists, not that the right one answered.
    val got: Int = summon[Route.Arity[Int *: EmptyTuple]](7 *: EmptyTuple)
    assertEquals(got, 7)

    val pair: (Int, String) = summon[Route.Arity[(Int, String)]]((7, "x"))
    assertEquals(pair, (7, "x"))

    val none: EmptyTuple = summon[Route.Arity[EmptyTuple]](EmptyTuple)
    assertEquals(none, EmptyTuple)
  }

  test("a handler for one captured parameter is handed the VALUE") {
    var seen: Any = null
    val r = Router.on(Method.Get, Route / "users" / Route[Int]("id"))(id => { seen = id; blank })
    val _ = r.routes(Request.get("/users/7"))
    assertEquals(seen, 7)
  }

  test("two parameters still arrive as two, and none as none") {
    var two: Any = null
    var zero: Any = null
    val r = Router
      .on(Method.Get, Route / "u" / Route[Int]("id") / "p" / Route[String]("slug"))((id, slug) =>
        { two = (id, slug); blank })
      .on(Method.Get, Route / "healthz")(t => { zero = t; blank })
    val _ = r.routes(Request.get("/u/7/p/hello"))
    val _ = r.routes(Request.get("/healthz"))
    assertEquals(two, (7, "hello"))
    assertEquals(zero, EmptyTuple)
  }

  test("the OPTIC still speaks in tuples — the collapse is the handler's, not the route's") {
    val one = Route / "users" / Route[Int]("id")
    assertEquals(one.unapply("/users/7"), Some(Tuple1(7)))
    assertEquals(one.url(Tuple1(7)), "/users/7")
  }
