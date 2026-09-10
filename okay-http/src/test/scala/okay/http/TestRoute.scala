package okay.http

import okay.*
import okay.given

/**
 * A route's three interpreters, and the law that ties two of them
 * together (specs/optics-outside.md, stage 1).
 *
 * The law is the reason the type exists: `unapply(url(a)) == Some(a)`
 * says the path a client builds is the path the server matches, from
 * ONE declaration — the property a hand-written `case r.url ==
 * "/users/" + id` cannot state, let alone check.
 */
class TestRoute extends munit.FunSuite {

  val healthz = Route.lit("healthz")
  val userPost = Route.lit("users") / Route[Int]("id") / "posts" / Route[String]("slug")
  val userId = Route.lit("users") / Route[Int]("id")

  test("a fully literal route matches its own path and nothing else") {
    assertEquals(healthz.unapply("/healthz"), Some(EmptyTuple))
    assertEquals(healthz.unapply("/healthy"), None)
    assertEquals(healthz.unapply("/healthz/x"), None)
    assertEquals(healthz.unapply("/"), None)
    assertEquals(healthz.url(EmptyTuple), "/healthz")
  }

  test("the root is the empty path") {
    assertEquals(Route.root.unapply("/"), Some(EmptyTuple))
    assertEquals(Route.root.unapply(""), Some(EmptyTuple))
    assertEquals(Route.root.url(EmptyTuple), "/")
    assertEquals(Route.root.unapply("/x"), None)
  }

  test("a captured segment parses by its Param and refuses what it refuses") {
    assertEquals(userPost.unapply("/users/7/posts/hello"), Some((7, "hello")))
    assertEquals(userPost.unapply("/users/abc/posts/hello"), None)
    assertEquals(userPost.unapply("/users/7/pages/hello"), None)
  }

  test("arity is part of the match") {
    assertEquals(userPost.unapply("/users/7/posts"), None)
    assertEquals(userPost.unapply("/users/7/posts/hello/extra"), None)
  }

  test("one captured segment destructures as a Tuple1") {
    assertEquals(userId.unapply("/users/7"), Some(Tuple1(7)))
    assertEquals(userId.url(Tuple1(7)), "/users/7")
  }

  test("the prism law: unapply(url(a)) == Some(a)") {
    val cases: Vector[(Int, String)] = Vector(
      (0, "a"), (7, "hello"), (-3, "x"), (Int.MaxValue, "z"),
      (1, "hello world"), (2, "a/b"), (3, "100%"), (4, "привет"),
      (5, "a?b"), (6, "a#b"), (8, "😀"))
    cases.foreach { a =>
      assertEquals(userPost.unapply(userPost.url(a)), Some(a), s"round trip of $a")
    }
  }

  test("a slash inside a parameter does not become a segment boundary") {
    val u = userPost.url((7, "a/b"))
    assertEquals(u, "/users/7/posts/a%2Fb")
    assertEquals(userPost.unapply(u), Some((7, "a/b")))
    // and the un-encoded form is a DIFFERENT path, with one segment too many
    assertEquals(userPost.unapply("/users/7/posts/a/b"), None)
  }

  test("malformed escaping is a miss, not a literal percent") {
    assertEquals(userPost.unapply("/users/7/posts/a%zz"), None)
    assertEquals(userPost.unapply("/users/7/posts/a%"), None)
    assertEquals(userPost.unapply("/users/7/posts/a%2"), None)
  }

  test("an empty capture is refused, so the law stays total on the domain") {
    assertEquals(Route[String]("s").unapply("//"), None)
    assertEquals(userPost.unapply("/users/7/posts/"), None)
  }

  test("a query string is not part of the path") {
    assertEquals(userPost.unapply("/users/7/posts/hello?draft=1"), Some((7, "hello")))
  }

  test("every Param round-trips") {
    val ints = Route[Int]("n")
    val longs = Route[Long]("n")
    val bools = Route[Boolean]("b")
    Vector(0, -1, Int.MinValue, Int.MaxValue).foreach(n =>
      assertEquals(ints.unapply(ints.url(Tuple1(n))), Some(Tuple1(n))))
    Vector(0L, -1L, Long.MinValue, Long.MaxValue).foreach(n =>
      assertEquals(longs.unapply(longs.url(Tuple1(n))), Some(Tuple1(n))))
    Vector(true, false).foreach(b =>
      assertEquals(bools.unapply(bools.url(Tuple1(b))), Some(Tuple1(b))))
    assertEquals(bools.unapply("/yes"), None)
  }

  test("describe names one placeholder per captured segment, in order") {
    assertEquals(userPost.describe, "/users/{id}/posts/{slug}")
    assertEquals(healthz.describe, "/healthz")
    assertEquals(Route.root.describe, "/")
    assertEquals(userPost.params.map(_.name), Vector("id", "slug"))
    assertEquals(userPost.params.map(_.kind), Vector("int", "string"))
  }

  test("describe needs no request, and names as many holes as there are params") {
    val holes = userPost.describe.count(_ == '{')
    assertEquals(holes, userPost.params.length)
  }

  test("the prism satisfies the same law through Optic") {
    val p = userPost.prism
    val c = p.compiled
    val a = (7, "hello world")
    val start = userPost.url((1, "x"))
    // the law in the core's own vocabulary: SET the parameters on a
    // path this route matches and you get the path `url` builds, and
    // previewing that reads the parameters back. `put` on a MISS is
    // the whole unchanged, which is what a prism's set means — the
    // review is reachable only through a hit, and that is the shape
    // this asserts rather than working around
    assertEquals(c.put(start, a), userPost.url(a))
    assertEquals(p.preview(c.put(start, a)), Some(a))
    assertEquals(c.put("/nope", a), "/nope")
    assertEquals(p.preview("/nope"), None)
  }

  final case class UserPost(id: Int, slug: String)

  test("a route can read a case class instead of a tuple") {
    val r = userPost.of[UserPost]
    assertEquals(r.unapply("/users/7/posts/hello"), Some(UserPost(7, "hello")))
    assertEquals(r.url(UserPost(7, "hello world")), "/users/7/posts/hello%20world")
    assertEquals(r.unapply(r.url(UserPost(3, "x"))), Some(UserPost(3, "x")))
    assertEquals(r.describe, "/users/{id}/posts/{slug}")
  }

  // ---- the router

  /** a body nobody reads: these tests are about DISPATCH, and they are
   * cross-platform, so nothing here runs an Async program */
  val blank: Response ! Async =
    pure(Response(200, Nil, Http.one(Array.empty[Byte])))

  val router: Router = Router.empty
    .on(Method.Get, healthz)(_ => blank)
    .on(Method.Get, userPost)((_, _) => blank)
    .on(Method.Post, userId)(_ => blank)

  test("a router dispatches by method and path") {
    val f = router.routes
    assert(f.isDefinedAt(Request.get("/healthz")))
    assert(f.isDefinedAt(Request.get("/users/7/posts/hello")))
    assert(!f.isDefinedAt(Request.get("/users/7")))
    assert(f.isDefinedAt(Request.post("/users/7", Body.Empty)))
    assert(!f.isDefinedAt(Request.post("/healthz", Body.Empty)))
    assert(!f.isDefinedAt(Request.get("/nope")))
  }

  test("the handler receives the parsed parameters") {
    var seen = ""
    val r = Router.empty
      .on(Method.Get, userPost)((id, slug) => { seen = s"$id:$slug"; blank })
      .on(Method.Post, userId)(t => { seen = s"posted ${t.head}"; blank })
    val _ = r.routes(Request.get("/users/7/posts/hello%20world"))
    assertEquals(seen, "7:hello world")
    val _ = r.routes(Request.post("/users/9", Body.Empty))
    assertEquals(seen, "posted 9")
  }

  test("a case-class route dispatches too") {
    var seen = UserPost(0, "")
    val r = Router.empty.of(Method.Get, userPost.of[UserPost])(p => { seen = p; blank })
    val _ = r.routes(Request.get("/users/4/posts/x"))
    assertEquals(seen, UserPost(4, "x"))
    assertEquals(r.describe, Vector((Method.Get, "/users/{id}/posts/{slug}")))
  }

  test("the acceptance routes are declared, and say so") {
    assertEquals(Acceptance.router.describe, Vector(
      (Method.Get, "/person"),
      (Method.Get, "/lines"),
      (Method.Post, "/echo")))
    val f = Acceptance.routes
    assert(f.isDefinedAt(Request.get("/person")))
    assert(f.isDefinedAt(Request.post("/echo", Body.Text("x"))))
    // what `startsWith` used to answer, and should not have
    assert(!f.isDefinedAt(Request.get("/personal")))
    assert(!f.isDefinedAt(Request.post("/person", Body.Empty)))
  }

  test("describe is derived from the values that dispatch") {
    assertEquals(router.describe, Vector(
      (Method.Get, "/healthz"),
      (Method.Get, "/users/{id}/posts/{slug}"),
      (Method.Post, "/users/{id}")))
    assertEquals(router.describe.length, router.entries.length)
  }
}
