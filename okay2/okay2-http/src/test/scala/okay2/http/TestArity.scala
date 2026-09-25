package okay2.http

import okay2.{!, pure}
import okay2.async.Async

/** a capture's outside form, and the join (okay-http's TestArity, in
 * Scala 2 terms): `Unit` for none, the value for one, the tuple for
 * several — at the handler AND at the optic, where Scala 3 keeps a
 * Tuple1 at the optic and collapses it only at the handler */
class TestArity extends munit.FunSuite {

  private val blank: Response ! Async = pure[Async, Response](Response(200, Nil, Http.one(Array.empty[Byte])))

  test("each outside form has one inside form, and back: Unit, one, several") {
    val none = Captures[Unit]
    val one = Captures[Int]
    val two = Captures[(Int, String)]
    assertEquals(none.fromList(none.toList(())), ())
    assertEquals(one.fromList(one.toList(7)), 7)
    assertEquals(two.fromList(two.toList((7, "x"))), (7, "x"))
    assertEquals(two.toList((7, "x")), :*:(7, :*:("x", HNil)))
  }

  test("the join and its inverse, at every seam: split(join(a, b)) == (a, b)") {
    def law[A, B](a: A, b: B)(implicit s: Split[A, B]): Unit = assertEquals(s.split(s.join(a, b)), (a, b))
    law((), 7)
    law(7, ())
    law(7, "x")
    law((7, "x"), Option(2))
    law(1, (2L, "three"))
    law((1, 2), (3, 4))
    assertEquals(Split[Int, String].join(7, "x"), (7, "x"))
    assertEquals(Split[(Int, String), Option[Int]].join((7, "x"), Some(2)), (7, "x", Option(2)))
    assertEquals(Split[Unit, Int].join((), 7), 7)
  }

  test("up to 8 captures join; a ninth does not compile") {
    assertEquals(Split[(Int, Int, Int, Int), (Int, Int, Int, Int)].join((1, 2, 3, 4), (5, 6, 7, 8)), (1, 2, 3, 4, 5, 6, 7, 8))
    assert(compileErrors("okay2.http.Split[(Int, Int, Int, Int, Int, Int, Int, Int), Int]").nonEmpty)
    assertEquals(compileErrors("okay2.http.Split[(Int, Int, Int, Int, Int, Int, Int), Int]"), "")
  }

  test("a handler for one captured parameter is handed the VALUE, and so is the optic") {
    var seen: Any = null
    val one = Route / "users" / Route[Int]("id")
    val _ = Router.on(Method.Get, one)(id => { seen = id; blank }).routes(Request.get("/users/7"))
    assertEquals(seen, 7)
    assertEquals(one.unapply("/users/7"), Some(7))
    assertEquals(one.url(7), "/users/7")
  }

  test("two parameters arrive as the pair, and none as Unit") {
    var two: Any = null
    var zero: Any = null
    val r = Router
      .on(Method.Get, Route / "u" / Route[Int]("id") / "p" / Route[String]("slug")) { case (id, slug) => two = (id, slug); blank }
      .on(Method.Get, Route / "healthz")(t => { zero = t; blank })
    val _ = r.routes(Request.get("/u/7/p/hello"))
    val _ = r.routes(Request.get("/healthz"))
    assertEquals(two, (7, "hello"))
    assertEquals(zero, ())
  }
}
