package okay.http

import okay.*
import okay.codec.{Json, Schema}

/**
 * A handler that answers a VALUE (openapi-responses).
 *
 * The point of the shape is that the declaration and the wire are the
 * SAME derivation: the entry carries `JsonSchema.of[R]` and the
 * router encodes with `Schema[R]`, so a document cannot promise one
 * thing while the service sends another. These tests hold the two
 * ends together.
 */
class TestRouterOut extends munit.FunSuite:

  final case class NewTask(title: String) derives Schema
  final case class Task(id: Int, title: String) derives Schema

  val board = Route / "board"
  val byId  = Route / "board" / Route[Int]("id")

  private def body(s: String): Body = Body.Text(s)

  // `runAsync`, not the blocking runner: this file is compiled for JS
  // too, where `CanBlock` does not exist at compile time
  import scala.concurrent.{Future, ExecutionContext}
  import ExecutionContext.Implicits.global
  private def answer(r: Router, req: Request): Future[(Int, String)] =
    Async.runAsync(r.routes(req).flatMap(res => Http.text(res).map(t => (res.status, t))))

  test("out: the answer is the value, encoded by the schema the entry declares") {
    val r = Router.empty.out[Int *: EmptyTuple, Task](Method.Get, byId)(id => pure(Task(id, "x")))
    // what the entry declares is the same derivation the answer used
    assertEquals(r.entries.head.answers.head.schema, Some(okay.codec.JsonSchema.of(summon[Schema[Task]])))
    answer(r, Request.get("/board/7")).map { (status, text) =>
      assertEquals(status, 200)
      assertEquals(Json.parse(text), Json.parse(okay.codec.Codecs.writeJson(Task(7, "x"))))
    }
  }

  test("out: a declared status is the status sent") {
    val r = Router.empty.out[EmptyTuple, Task](Method.Post, board, status = 201)(_ => pure(Task(1, "t")))
    assertEquals(r.entries.head.answers.map(_.status), Vector(201))
    answer(r, Request.post("/board", body(""))).map((status, _) => assertEquals(status, 201))
  }

  test("jsonOut: a body in, a value out, both declared") {
    val r = Router.empty.jsonOut[EmptyTuple, NewTask, Task](Method.Post, board)((_, t) => pure(Task(9, t.title)))
    assertEquals(r.entries.head.body, Some(okay.codec.JsonSchema.of(summon[Schema[NewTask]])))
    answer(r, Request.post("/board", body("""{"title":"write it down"}"""))).map { (status, text) =>
      assertEquals(status, 200)
      assertEquals(Json.parse(text), Json.parse(okay.codec.Codecs.writeJson(Task(9, "write it down"))))
    }
  }

  test("jsonOut: a body that does not parse is the 400 the entry already declared") {
    val r = Router.empty.jsonOut[EmptyTuple, NewTask, Task](Method.Post, board)((_, t) => pure(Task(9, t.title)))
    // declared without the author writing anything
    assertEquals(r.entries.head.answers.map(_.status).sorted, Vector(200, 400))
    answer(r, Request.post("/board", body("{oops"))).map { (status, text) =>
      assertEquals(status, 400)
      assert(text.contains("\"error\""), text)
    }
  }

  test("on/at declare nothing: a handler that builds its own Response is unchanged") {
    val r = Router.empty.on(Method.Get, board)(_ => pure(Response(204, Nil, Http.one(Array.empty[Byte]))))
    assertEquals(r.entries.head.answers, Vector.empty)
    answer(r, Request.get("/board")).map((status, _) => assertEquals(status, 204))
  }
