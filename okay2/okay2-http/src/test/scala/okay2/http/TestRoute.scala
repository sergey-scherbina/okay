package okay2.http

import okay2.{!, pure}
import okay2.async.Async
import okay2.codec.Json
import okay2.http.syntax._

final case class UserPost(id: Int, slug: String)
final case class Wrong(a: Int, b: String)
final case class Found(q: String, page: Option[Int])
final case class Misnamed(q: String, limit: Option[Int])

/** a route's three interpreters, and the law that ties two of them:
 * `unapply(url(a)) == Some(a)` (okay-http's TestRoute) */
class TestRoute extends munit.FunSuite {

  val healthz = Route / "healthz"
  val userPost = Route / "users" / Route[Int]("id") / "posts" / Route[String]("slug")
  val userId = Route / "users" / Route[Int]("id")

  test("a fully literal route matches its own path and nothing else") {
    assertEquals(healthz.unapply("/healthz"), Some(()))
    assertEquals(healthz.unapply("/healthy"), None)
    assertEquals(healthz.unapply("/healthz/x"), None)
    assertEquals(healthz.unapply("/"), None)
    assertEquals(healthz.url(()), "/healthz")
  }

  test("the root is the empty path") {
    assertEquals(Route.root.unapply("/"), Some(()))
    assertEquals(Route.root.unapply(""), Some(()))
    assertEquals(Route.root.url(()), "/")
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

  test("one captured segment is the value itself (Scala 3: a Tuple1)") {
    assertEquals(userId.unapply("/users/7"), Some(7))
    assertEquals(userId.url(7), "/users/7")
  }

  test("the route destructures in a pattern, as the server writes it") {
    val got = Request.get("/users/7/posts/hi") match {
      case Get(userPost(id, slug)) => s"$id:$slug"
      case _ => "miss"
    }
    assertEquals(got, "7:hi")
  }

  test("the prism law: unapply(url(a)) == Some(a)") {
    val cases: Vector[(Int, String)] = Vector(
      (0, "a"), (7, "hello"), (-3, "x"), (Int.MaxValue, "z"),
      (1, "hello world"), (2, "a/b"), (3, "100%"), (4, "привет"),
      (5, "a?b"), (6, "a#b"), (8, "😀"))
    cases.foreach(a => assertEquals(userPost.unapply(userPost.url(a)), Some(a), s"round trip of $a"))
  }

  test("a slash inside a parameter does not become a segment boundary") {
    val u = userPost.url((7, "a/b"))
    assertEquals(u, "/users/7/posts/a%2Fb")
    assertEquals(userPost.unapply(u), Some((7, "a/b")))
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

  test("a query string and a fragment are not part of the path") {
    assertEquals(userPost.unapply("/users/7/posts/hello?draft=1"), Some((7, "hello")))
    assertEquals(userPost.unapply("/users/7/posts/hello#top"), Some((7, "hello")))
  }

  test("every Param round-trips") {
    val ints = Route[Int]("n")
    val longs = Route[Long]("n")
    val bools = Route[Boolean]("b")
    Vector(0, -1, Int.MinValue, Int.MaxValue).foreach(n => assertEquals(ints.unapply(ints.url(n)), Some(n)))
    Vector(0L, -1L, Long.MinValue, Long.MaxValue).foreach(n => assertEquals(longs.unapply(longs.url(n)), Some(n)))
    Vector(true, false).foreach(b => assertEquals(bools.unapply(bools.url(b)), Some(b)))
    assertEquals(bools.unapply("/yes"), None)
  }

  test("describe names one placeholder per captured segment, in order") {
    assertEquals(userPost.describe, "/users/{id}/posts/{slug}")
    assertEquals(healthz.describe, "/healthz")
    assertEquals(Route.root.describe, "/")
    assertEquals(userPost.params.map(_.name), Vector("id", "slug"))
    assertEquals(userPost.params.map(_.kind), Vector("int", "string"))
    assertEquals(userPost.describe.count(_ == '{'), userPost.params.length)
  }

  test("the prism satisfies the same law through Optic") {
    val p = userPost.prism
    val c = p.compiled
    val a = (7, "hello world")
    val start = userPost.url((1, "x"))
    assertEquals(c.put(start, a), userPost.url(a))
    assertEquals(p.preview(c.put(start, a)), Some(a))
    assertEquals(c.put("/nope", a), "/nope")
    assertEquals(p.preview("/nope"), None)
  }

  test("the parameter names must be the field names") {
    val e = intercept[IllegalArgumentException](userPost.of[Wrong])
    assert(e.getMessage.contains("(id, slug)"), e.getMessage)
    assert(e.getMessage.contains("(a, b)"), e.getMessage)
  }

  test("the field TYPES must be the capture, at compile time (Scala 3: the Mirror)") {
    val errs = compileErrors(
      "okay2.http.Route./(\"users\")./(okay2.http.Route[Int](\"id\")).of[okay2.http.Misnamed]")
    assert(errs.contains("captures Int"), errs)
    // paired: the class whose fields ARE the capture compiles
    assertEquals(compileErrors(
      "okay2.http.Route./(\"users\")./(okay2.http.Route[Int](\"id\"))./(okay2.http.Route[String](\"slug\")).of[okay2.http.UserPost]"), "")
  }

  test("the query's parameters count too, in capture order") {
    assertEquals(search.of[Found].unapply("/search?q=cats&page=2"), Some(Found("cats", Some(2))))
    val e = intercept[IllegalArgumentException](search.of[Misnamed])
    assert(e.getMessage.contains("(q, page)"), e.getMessage)
    assert(e.getMessage.contains("(q, limit)"), e.getMessage)
  }

  test("a route can read a case class instead of a tuple") {
    val r = userPost.of[UserPost]
    assertEquals(r.unapply("/users/7/posts/hello"), Some(UserPost(7, "hello")))
    assertEquals(r.url(UserPost(7, "hello world")), "/users/7/posts/hello%20world")
    assertEquals(r.unapply(r.url(UserPost(3, "x"))), Some(UserPost(3, "x")))
    assertEquals(r.describe, "/users/{id}/posts/{slug}")
  }

  val search: Queried[(String, Option[Int])] = Route / "search" :? Query[String]("q") +& Query.opt[Int]("page")

  val tagged: Queried[(Int, Vector[String])] = Route / "posts" / Route[Int]("id") :? Query.all[String]("tag")

  test("a query parameter is read by name, not by position") {
    assertEquals(search.unapply("/search?q=cats&page=2"), Some(("cats", Some(2))))
    assertEquals(search.unapply("/search?page=2&q=cats"), Some(("cats", Some(2))))
  }

  test("an absent optional parameter is None, and writes nothing") {
    assertEquals(search.unapply("/search?q=cats"), Some(("cats", None)))
    assertEquals(search.url(("cats", None)), "/search?q=cats")
    assertEquals(search.url(("cats", Some(2))), "/search?q=cats&page=2")
  }

  test("a missing REQUIRED parameter is a miss; present and unparseable is a miss, not None") {
    assertEquals(search.unapply("/search?page=2"), None)
    assertEquals(search.unapply("/search"), None)
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
      ("cats", None), ("cats", Some(0)), ("a b", Some(-1)), ("a&b=c", Some(7)), ("100%", None), ("привет", Some(3)), ("a+b", None))
    cases.foreach(a => assertEquals(search.unapply(search.url(a)), Some(a), s"round trip of $a"))
    Vector((0, Vector.empty[String]), (7, Vector("a b", "x&y")), (1, Vector("", "z")))
      .foreach(a => assertEquals(tagged.unapply(tagged.url(a)), Some(a), s"round trip of $a"))
  }

  test("a raw + in a query value is a space, and a literal + survives") {
    assertEquals(search.unapply("/search?q=a+b"), Some(("a b", None)))
    assertEquals(search.url(("a+b", None)), "/search?q=a%2Bb")
    assertEquals(search.unapply("/search?q=a%2Bb"), Some(("a+b", None)))
  }

  test("a query separator inside a value is not a separator; malformed escaping is a miss") {
    val u = search.url(("a&page=9", None))
    assertEquals(u, "/search?q=a%26page%3D9")
    assertEquals(search.unapply(u), Some(("a&page=9", None)))
    assertEquals(search.unapply("/search?q=%zz"), None)
    assertEquals(search.unapply("/search?q=cats#top"), Some(("cats", None)))
  }

  test("describe stays the path, and describeFull shows the query") {
    assertEquals(search.describe, "/search")
    assertEquals(search.describeFull, "/search?q={q}&page={page}")
    assertEquals(userPost.describeFull, "/users/{id}/posts/{slug}")
    assertEquals(search.queries.map(_.name), Vector("q", "page"))
    assertEquals(search.queries.map(_.required), Vector(true, false))
    assertEquals(tagged.queries.map(_.repeated), Vector(true))
  }

  test("a typed string is a hole, a bare string is a literal") {
    val terse = Route / "users" / "id".as[Int] / "posts" / "slug".as[String]
    assertEquals(terse.describe, userPost.describe)
    assertEquals(terse.unapply("/users/7/posts/hello"), Some((7, "hello")))
    assertEquals(terse.url((7, "hello world")), userPost.url((7, "hello world")))
  }

  test("the same names in a query position, and :? chains") {
    val terse = Route / "search" :? "q".as[String] :? "page".opt[Int]
    assertEquals(terse.describeFull, search.describeFull)
    assertEquals(terse.unapply("/search?page=2&q=cats"), Some(("cats", Some(2))))
    assertEquals(terse.url(("cats", None)), "/search?q=cats")
    val tags = Route / "posts" / "id".as[Int] :? "tag".all[String]
    assertEquals(tags.unapply("/posts/7?tag=a&tag=b"), Some((7, Vector("a", "b"))))
    assertEquals(tags.describeFull, tagged.describeFull)
  }

  test("+& still composes a query value worth sharing") {
    val paging = Query.opt[Int]("page") +& Query.opt[Int]("size")
    val posts = Route / "posts" :? paging
    val users = Route / "users" :? paging
    assertEquals(posts.unapply("/posts?page=1&size=20"), Some((Some(1), Some(20))))
    assertEquals(users.unapply("/users"), Some((None, None)))
  }

  test("a path segment cannot follow a query parameter — both groupings; a query is not a segment") {
    assert(compileErrors("""
      import okay2.http._, okay2.http.syntax._
      Route / "search" :? "q".as[String] / "page"
    """).nonEmpty, "a segment after a query parameter compiled")
    assert(compileErrors("""
      import okay2.http._, okay2.http.syntax._
      (Route / "search" :? "q".as[String]) / "page"
    """).nonEmpty, "a segment after a query compiled when parenthesised")
    assert(compileErrors("""
      import okay2.http._, okay2.http.syntax._
      Route / "search" / "page".opt[Int]
    """).nonEmpty, "a query parameter was accepted as a path segment")
    // paired: the legal order compiles
    assertEquals(compileErrors("""
      import okay2.http._, okay2.http.syntax._
      Route / "search" / "page" :? "q".as[String]
    """), "")
  }

  /** a body nobody reads: these tests are about DISPATCH */
  val blank: Response ! Async = pure[Async, Response](Response(200, Nil, Http.one(Array.empty[Byte])))

  val router: Router = Router.empty
    .on(Method.Get, healthz)(_ => blank)
    .on(Method.Get, userPost)(_ => blank)
    .on(Method.Post, userId)(_ => blank)

  test("a table starts from the companion, and empty is still the zero") {
    assertEquals(Router.on(Method.Get, healthz)(_ => blank).describe, Vector((Method.Get, "/healthz")))
    assertEquals(Router.empty.describe, Vector.empty)
  }

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
      .on(Method.Get, userPost) { case (id, slug) => seen = s"$id:$slug"; blank }
      .on(Method.Post, userId)(id => { seen = s"posted $id"; blank })
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
    assertEquals(Acceptance.router.describe, Vector((Method.Get, "/person"), (Method.Get, "/lines"), (Method.Post, "/echo")))
    val f = Acceptance.routes
    assert(f.isDefinedAt(Request.get("/person")))
    assert(f.isDefinedAt(Request.post("/echo", Body.Text("x"))))
    assert(!f.isDefinedAt(Request.get("/personal")))
    assert(!f.isDefinedAt(Request.post("/person", Body.Empty)))
  }

  test("a router dispatches a route that reads the query") {
    var seen = ("", Option.empty[Int])
    val r = Router.empty.on(Method.Get, search) { case (q, page) => seen = (q, page); blank }
    assert(r.routes.isDefinedAt(Request.get("/search?q=cats&page=2")))
    assert(!r.routes.isDefinedAt(Request.get("/search")))
    val _ = r.routes(Request.get("/search?page=2&q=cats"))
    assertEquals(seen, ("cats", Some(2)))
    assertEquals(r.describe, Vector((Method.Get, "/search")))
  }

  test("describe is derived from the values that dispatch") {
    assertEquals(router.describe, Vector((Method.Get, "/healthz"), (Method.Get, "/users/{id}/posts/{slug}"), (Method.Post, "/users/{id}")))
    assertEquals(router.describe.length, router.entries.length)
  }

  private def typed(t: String) = Json.JObj(Vector("type" -> Json.JStr(t)))

  test("a parameter carries its JSON Schema, not only the word for its kind") {
    assertEquals(userPost.described.params.map(_.schema), Vector(typed("integer"), typed("string")))
    assertEquals(tagged.described.queries.map(_.schema), Vector(Json.JObj(Vector("type" -> Json.JStr("array"), "items" -> typed("string")))))
    assertEquals(search.described.queries.map(_.schema), Vector(typed("string"), typed("integer")))
  }

  test("a custom Param may say more about itself than its kind can, and it survives") {
    implicit val uuid: Route.Param[java.util.UUID] = new Route.Param[java.util.UUID] {
      def kind = "uuid"
      def parse(s: String): Option[java.util.UUID] = scala.util.Try(java.util.UUID.fromString(s)).toOption
      def print(t: java.util.UUID): String = t.toString
      override def jsonSchema: Json = Json.JObj(Vector("type" -> Json.JStr("string"), "format" -> Json.JStr("uuid")))
    }
    val thing = Route / "things" / Route[java.util.UUID]("ref")
    val id = java.util.UUID.fromString("3f2504e0-4f89-11d3-9a0c-0305e82c3301")
    assertEquals(thing.unapply(thing.url(id)), Some(id))
    assertEquals(thing.described.params.head.schema, Json.JObj(Vector("type" -> Json.JStr("string"), "format" -> Json.JStr("uuid"))))
    assertEquals(Route.Param.schemaOf("uuid"), typed("string"))
  }

  test("the router's table carries the whole description, path and query alike") {
    val r = Router.empty.on(Method.Get, search)(_ => blank)
    assertEquals(r.entries.head.path, "/search")
    assertEquals(r.entries.head.queries.map(_.name), Vector("q", "page"))
    assertEquals(r.entries.head.queries.map(_.required), Vector(true, false))
    assertEquals(r.entries.head.params, Vector.empty)
  }
}
