package okay2.http

import okay2.pure
import okay2.async.Async
import okay2.codec.{Json, JsonSchema, Schema}
import Drive.answer

final case class NewTask(title: String)
final case class Task(id: Int, title: String)

/** a handler that answers a VALUE (okay-http's TestRouterOut): the entry
 * carries `JsonSchema.of[R]` and the router encodes with `Schema[R]`, so
 * a document cannot promise one thing while the service sends another */
class TestRouterOut extends munit.FunSuite {

  val board = Route / "board"
  val byId = Route / "board" / Route[Int]("id")

  test("out: the answer is the value, encoded by the schema the entry declares") {
    val r = Router.empty.out[Int, Task](Method.Get, byId)(id => pure[Async, Task](Task(id, "x")))
    assertEquals(r.entries.head.answers.head.schema, Some(JsonSchema.of(implicitly[Schema[Task]])))
    val (status, _, text) = answer(r, Request.get("/board/7"))
    assertEquals(status, 200)
    assertEquals(Json.parse(text), Json.parse(Json.write(Task(7, "x"))))
  }

  test("out: a declared status is the status sent") {
    val r = Router.empty.out[Unit, Task](Method.Post, board, status = 201)(_ => pure[Async, Task](Task(1, "t")))
    assertEquals(r.entries.head.answers.map(_.status), Vector(201))
    assertEquals(answer(r, Request.post("/board", Body.Text("")))._1, 201)
  }

  test("jsonOut: a body in, a value out, both declared") {
    val r = Router.empty.jsonOut[Unit, NewTask, Task](Method.Post, board)((_, t) => pure[Async, Task](Task(9, t.title)))
    assertEquals(r.entries.head.body, Some(JsonSchema.of(implicitly[Schema[NewTask]])))
    val (status, _, text) = answer(r, Request.post("/board", Body.Text("""{"title":"write it down"}""")))
    assertEquals(status, 200)
    assertEquals(Json.parse(text), Json.parse(Json.write(Task(9, "write it down"))))
  }

  test("jsonOut: a body that does not parse is the 400 the entry already declared") {
    val r = Router.empty.jsonOut[Unit, NewTask, Task](Method.Post, board)((_, t) => pure[Async, Task](Task(9, t.title)))
    assertEquals(r.entries.head.answers.map(_.status).sorted, Vector(200, 400))
    val (status, _, text) = answer(r, Request.post("/board", Body.Text("{oops")))
    assertEquals(status, 400)
    assert(text.contains("\"error\""), text)
  }

  test("on/at declare nothing: a handler that builds its own Response is unchanged") {
    val r = Router.empty.on(Method.Get, board)(_ => pure[Async, Response](Response(204, Nil, Http.one(Array.empty[Byte]))))
    assertEquals(r.entries.head.answers, Vector.empty)
    assertEquals(answer(r, Request.get("/board"))._1, 204)
  }
}
