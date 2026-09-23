package okay.http

import okay.*
import okay.codec.Schema

/** typescript-types T5, the generated text (default gate, no Node) */
class TestTsClientShape extends munit.FunSuite {
  final case class Task(id: Int, title: String) derives Schema
  final case class NewTask(title: String) derives Schema

  private val r = Router.empty
    .out[Int *: EmptyTuple, Task](Method.Get, Route / "tasks" / Route[Int]("id"))(id => pure(Task(id, "x")))
    .jsonOut[EmptyTuple, NewTask, Task](Method.Post, Route / "tasks")((_, t) => pure(Task(1, t.title)))
    .out[Vector[String] *: EmptyTuple, Vector[Task]](Method.Get, Route / "tagged" :? Query.all[String]("tag"))(_ => pure(Vector.empty))
    .html(Method.Get, Route.root)(_ => pure("<h1>hi</h1>"))

  test("one typed function per route: path, body and query parameters, the answer's type") {
    val c = TsClient.client(r)
    assert(c.contains("export async function getTasksById(o: ClientOptions, id: number): Promise<Task> {"), c)
    assert(c.contains("export async function postTasks(o: ClientOptions, body: NewTask): Promise<Task> {"), c)
    assert(c.contains("export async function getTagged(o: ClientOptions, query?: { tag?: string[] }): Promise<Task[]> {"), c)
    assert(c.contains("send(o, \"GET\", `/tasks/${encodeURIComponent(String(id))}`, undefined, undefined)"), c)
    assert(c.contains("export async function getRoot(o: ClientOptions): Promise<Response> {"), c)
    assert(c.contains("import type { Int, Task, NewTask } from \"./model.ts\";"), c)
  }

  test("the model is every type a route takes or answers, in the JSON codec's shape") {
    val m = TsClient.model(r)
    assert(m.contains("export interface Task {\n  id: Int;\n  title: string;\n}"), m)
    assert(m.contains("export interface NewTask {\n  title: string;\n}"), m)
  }
}
