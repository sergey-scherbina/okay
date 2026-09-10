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

  val healthz = Route / "healthz"
  val userPost = Route / "users" / Route[Int]("id") / "posts" / Route[String]("slug")
  val userId = Route / "users" / Route[Int]("id")

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

  // ---- the query string (stage 3)

  val search: Route[(String, Option[Int])] =
    Route / "search" :? Query[String]("q") +& Query.opt[Int]("page")

  val tagged: Route[(Int, Vector[String])] =
    Route / "posts" / Route[Int]("id") :? Query.all[String]("tag")

  test("a query parameter is read by name, not by position") {
    assertEquals(search.unapply("/search?q=cats&page=2"), Some(("cats", Some(2))))
    assertEquals(search.unapply("/search?page=2&q=cats"), Some(("cats", Some(2))))
  }

  test("an absent optional parameter is None, and writes nothing") {
    assertEquals(search.unapply("/search?q=cats"), Some(("cats", None)))
    assertEquals(search.url(("cats", None)), "/search?q=cats")
    assertEquals(search.url(("cats", Some(2))), "/search?q=cats&page=2")
  }

  test("a missing REQUIRED parameter is a miss") {
    assertEquals(search.unapply("/search?page=2"), None)
    assertEquals(search.unapply("/search"), None)
  }

  test("present and unparseable is a miss, not None") {
    // ?page=abc meant something and got it wrong; answering it as
    // though page had been omitted would hide the caller's mistake
    assertEquals(search.unapply("/search?q=cats&page=abc"), None)
  }

  test("unknown query parameters are ignored") {
    assertEquals(search.unapply("/search?q=cats&utm_source=x"), Some(("cats", None)))
  }

  test("a repeated parameter keeps every value, in wire order") {
    assertEquals(tagged.unapply("/posts/7?tag=a&tag=b"), Some((7, Vector("a", "b"))))
    assertEquals(tagged.unapply("/posts/7"), Some((7, Vector.empty)))
    assertEquals(tagged.url((7, Vector("a", "b"))), "/posts/7?tag=a&tag=b")
    assertEquals(tagged.url((7, Vector.empty)), "/posts/7")
  }

  test("the prism law holds with a query") {
    val cases: Vector[(String, Option[Int])] = Vector(
      ("cats", None), ("cats", Some(0)), ("a b", Some(-1)),
      ("a&b=c", Some(7)), ("100%", None), ("привет", Some(3)), ("a+b", None))
    cases.foreach(a => assertEquals(search.unapply(search.url(a)), Some(a), s"round trip of $a"))
    Vector((0, Vector.empty[String]), (7, Vector("a b", "x&y")), (1, Vector("", "z")))
      .foreach(a => assertEquals(tagged.unapply(tagged.url(a)), Some(a), s"round trip of $a"))
  }

  test("a raw + in a query value is a space, and a literal + survives") {
    // the form-encoding convention every browser writes
    assertEquals(search.unapply("/search?q=a+b"), Some(("a b", None)))
    // and `url` never emits a raw +, so the round trip is unaffected
    assertEquals(search.url(("a+b", None)), "/search?q=a%2Bb")
    assertEquals(search.unapply("/search?q=a%2Bb"), Some(("a+b", None)))
  }

  test("a query separator inside a value is not a separator") {
    val u = search.url(("a&page=9", None))
    assertEquals(u, "/search?q=a%26page%3D9")
    assertEquals(search.unapply(u), Some(("a&page=9", None)))
  }

  test("malformed escaping in the query is a miss") {
    assertEquals(search.unapply("/search?q=%zz"), None)
  }

  test("a fragment is not part of the path or the query") {
    assertEquals(search.unapply("/search?q=cats#top"), Some(("cats", None)))
    assertEquals(userPost.unapply("/users/7/posts/hello#top"), Some((7, "hello")))
  }

  test("describe stays the path, and describeFull shows the query") {
    assertEquals(search.describe, "/search")
    assertEquals(search.describeFull, "/search?q={q}&page={page}")
    assertEquals(userPost.describeFull, "/users/{id}/posts/{slug}")
    assertEquals(search.queries.map(_.name), Vector("q", "page"))
    assertEquals(search.queries.map(_.required), Vector(true, false))
    assertEquals(tagged.queries.map(_.repeated), Vector(true))
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

  test("a router dispatches a route that reads the query") {
    var seen = ("", Option.empty[Int])
    val r = Router.empty.on(Method.Get, search)((q, page) => { seen = (q, page); blank })
    assert(r.routes.isDefinedAt(Request.get("/search?q=cats&page=2")))
    // the required parameter is part of the match, so its absence is
    // a MISS and the caller's 404 stays the caller's
    assert(!r.routes.isDefinedAt(Request.get("/search")))
    val _ = r.routes(Request.get("/search?page=2&q=cats"))
    assertEquals(seen, ("cats", Some(2)))
    assertEquals(r.describe, Vector((Method.Get, "/search")))
  }

  test("describe is derived from the values that dispatch") {
    assertEquals(router.describe, Vector(
      (Method.Get, "/healthz"),
      (Method.Get, "/users/{id}/posts/{slug}"),
      (Method.Post, "/users/{id}")))
    assertEquals(router.describe.length, router.entries.length)
  }
}
