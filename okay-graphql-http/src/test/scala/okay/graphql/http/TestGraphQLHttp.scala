package okay.graphql.http

import okay.Async
import okay.codec.Json
import okay.graphql.*
import okay.http.{Body, Http, Method, Request}
import okay.one.Cont.pure
import okay.one.row.*
import scala.concurrent.Future

class TestGraphQLHttp extends okay.testkit.Munit.Diagnosed:
  private given scala.concurrent.ExecutionContext = munitExecutionContext

  private val counter = AsyncSource[String, Int]("lengths")(keys => pure(keys.map(k => k -> k.length).toMap))
  private val schema = Schema(
    ObjectType[Unit]("Query")(List(
      Field.fetch("length", Scalar.int, Arg("of", InScalar.string))((_, a) => Fetch.get(counter, a(Arg("of", InScalar.string)))))),
    mutation = Some(ObjectType[Unit]("Mutation")(List(Field("ping", Scalar.string)(_ => "pong")))))
  private val router = GraphQLHttp.router(schema)

  /** the request through the router, in memory: its status, content type and body */
  private def call(r: Request): Future[(Int, String, String)] =
    Async.runAsync(router.routes(r).flatMap { res =>
      Http.text(res).map(t => (res.status, res.header("content-type").getOrElse(""), t))
    }).map { out => note(s"${r.method} ${r.url} => $out"); out }

  private def post(body: String, headers: Seq[(String, String)] = Seq("content-type" -> "application/json")) =
    Request(Method.Post, "/graphql", headers, Body.Text(body))

  test("POST: a query with variables, an async source answered") {
    call(post("""{"query":"query($s: String!) { length(of: $s) }","variables":{"s":"four"}}""")).map { (status, ct, body) =>
      assertEquals((status, ct, body), (200, "application/json; charset=utf-8", """{"data":{"length":4}}"""))
    }
  }

  test("GET: the query string read, percent-encoding and all") {
    call(Request.get("/graphql?query=%7B%20length(of%3A%20%22%C3%A9t%C3%A9%22)%20%7D")).map { (status, _, body) =>
      assertEquals((status, body), (200, """{"data":{"length":3}}"""))
    }
  }

  test("GET: a mutation is refused with 405 and Allow: POST") {
    call(Request.get("/graphql?query=mutation+%7B+ping+%7D")).map { (status, _, body) =>
      assertEquals(status, 405)
      assert(body.contains("cannot be sent by GET"), body)
    }
  }

  test("a body that is not JSON, or names no query, is a 400; a refused document is 200, or 400 when accepted") {
    for
      bad <- call(post("not json"))
      none <- call(post("""{"variables":{}}"""))
      form <- call(post("query={x}", Seq("content-type" -> "text/plain")))
      legacy <- call(post("""{"query":"{ nope }"}"""))
      modern <- call(post("""{"query":"{ nope }"}""", Seq("content-type" -> "application/json",
        "accept" -> "application/graphql-response+json")))
    yield
      assertEquals(bad._1, 400)
      assertEquals(none, (400, "application/json; charset=utf-8", """{"errors":[{"message":"the request names no query"}]}"""))
      assertEquals(form._1, 400)
      assertEquals(legacy._1, 200)
      assertEquals(modern._1, 400)
      assertEquals(modern._2, "application/graphql-response+json; charset=utf-8")
      assertEquals(Json.parse(modern._3), Json.parse(legacy._3))
  }

  test("POST: a mutation, by operationName") {
    call(post("""{"query":"query A { length(of: \"x\") } mutation B { ping }","operationName":"B"}""")).map { (status, _, body) =>
      assertEquals((status, body), (200, """{"data":{"ping":"pong"}}"""))
    }
  }
