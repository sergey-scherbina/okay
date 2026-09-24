package scala2probe

import okay.codec.Schema
import okay.http.{Body, Method, Request}
import okay.scala2._

object HttpModel {
  final case class User(id: Int, name: String)
  object User {
    implicit val schema: Schema[User] = Schemas.product2("User", "id", "name")(User.apply)(u => (u.id, u.name))
  }

  val users = scala.collection.concurrent.TrieMap(1 -> User(1, "ada"))

  val routes: Request => Response ! Async = Routes {
    case GET(Path("users", id)) =>
      Async.delay(users.get(id.toInt) match {
        case Some(u) => Response.json(u)
        case None => Response.text("no user " + id, 404)
      })
    case r @ POST(Path("users")) =>
      Requests.json[User](r) match {
        case Right(u) => Async.delay { users.put(u.id, u); Response.json(u, 201) }
        case Left(e) => Eff.pure(Response.text(e, 400))
      }
    case r @ GET(Path("search")) =>
      Eff.pure(Response.text("q=" + Requests.query(r, "q").getOrElse("") + " tags=" + Requests.queryAll(r, "tag").mkString(",")))
  }
}

/** okay-http from Scala 2.13, no socket (specs/scala2-facade.md, stage 7) */
class TestHttpFromScala2 extends munit.FunSuite {
  import HttpModel._

  def call(r: Request): Response = Eff.runAsync(routes(r))

  test("a route matches method and path, and answers JSON") {
    val r = call(Request.get("/users/1"))
    assertEquals(r.status, 200)
    assertEquals(r.header("Content-Type"), Some("application/json"))
    assertEquals(Json.read[User](r.text), Right(User(1, "ada")))
  }

  test("a JSON body is decoded, and a bad one is a 400") {
    val created = call(Request(Method.Post, "/users", Seq("content-type" -> "application/json"), Body.Text("""{"id":2,"name":"bob"}""")))
    assertEquals(created.status, 201)
    assertEquals(call(Request.get("/users/2")).text, """{"id":2,"name":"bob"}""")
    assertEquals(call(Request(Method.Post, "/users", Nil, Body.Text("""{"id":"x"}"""))).status, 400)
  }

  test("query parameters, percent-decoded, one or all") {
    assertEquals(call(Request.get("/search?q=hello%20world&tag=a&tag=b")).text, "q=hello world tags=a,b")
  }

  test("no route answers 404, and a path segment is decoded") {
    assertEquals(call(Request.get("/nowhere")).status, 404)
    assertEquals(call(Request.get("/users/7")).text, "no user 7")
    assertEquals(Requests.path(Request.get("/a%2Fb/c?x=1")), Vector("a/b", "c"))
  }
}

/** a real socket: the server and the client together. Tagged Live, like
 * every suite in this repository that binds a port. */
class TestHttpLiveFromScala2 extends munit.FunSuite {
  import HttpModel._

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  test("Server.use serves while the body runs; the Client reads JSON back") {
    val client = Client()
    val got = Eff.runAsync(Server.use(0)(routes) { port =>
      client.get("http://127.0.0.1:" + port + "/users/1").map(r => (r.status, r.text))
    })
    assertEquals(got, (200, """{"id":1,"name":"ada"}"""))
  }

  test("Server.start returns a running server; close stops it") {
    val server = Server.start(0)(routes)
    try {
      val r = Eff.runAsync(Client().postJson("http://127.0.0.1:" + server.port + "/users", User(3, "cy")))
      assertEquals(r.status, 201)
    } finally server.close()
    val refused = scala.util.Try(Eff.runAsync(Client().get("http://127.0.0.1:" + server.port + "/users/1")))
    assert(refused.isFailure, refused.toString)
  }

  test("a streamed response is read line by line") {
    val lines = Routes { case GET(Path("count")) => Eff.pure(Response.lines(Source.range(1, 4).map(_.toString))) }
    val got = Eff.runAsync(Server.use(0)(lines) { port =>
      Client().lines(Request.get("http://127.0.0.1:" + port + "/count")).runCollect
    })
    assertEquals(got, Vector("1", "2", "3"))
  }
}
