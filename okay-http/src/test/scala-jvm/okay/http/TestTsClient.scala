package okay.http

import okay.*
import okay.given
import okay.codec.Schema

object TestTsClient:
  final case class Task(id: Int, title: String, done: Boolean, tags: Vector[String]) derives Schema
  final case class NewTask(title: String, tags: Vector[String]) derives Schema

  /** the Scala backend: three routes, their types written ONCE, here */
  def router(store: java.util.concurrent.ConcurrentHashMap[Int, Task]): Router =
    val byId = Route / "tasks" / Route[Int]("id")
    val board = Route / "tasks"
    val tagged = Route / "tagged" :? Query.all[String]("tag")
    Router.empty
      .out[Int *: EmptyTuple, Task](Method.Get, byId)(id => pure(store.get(id)))
      .jsonOut[EmptyTuple, NewTask, Task](Method.Post, board, status = 201) { (_, t) =>
        val task = Task(store.size + 1, t.title, false, t.tags)
        store.put(task.id, task): Unit
        pure(task)
      }
      .out[Vector[String] *: EmptyTuple, Vector[Task]](Method.Get, tagged)(tags =>
        pure(store.values.toArray(Array.empty[Task]).toVector.filter(t => tags.forall(t.tags.contains)).sortBy(_.id)))

  // no margin: the docs quote these lines
  val usage: String = """
import { getTagged, getTasksById, postTasks, OkayHttpError } from "./client.ts";
import type { Task } from "./model.ts";

const o = { base: process.argv[2] };
const made: Task = await postTasks(o, { title: "write the docs", tags: ["docs"] });
const again: Task = await getTasksById(o, made.id);
const docs: Task[] = await getTagged(o, { tag: ["docs"] });
let status = 0;
try { await postTasks(o, { title: 7 } as never); } catch (e) { if (e instanceof OkayHttpError) status = e.status; }
console.log(JSON.stringify({ made, again, docs: docs.map((t) => t.title), status }));
"""

/**
 * typescript-types T5 end to end (Live: binds a port, runs Node): the Scala
 * routes, the generated client and model, a real okay-http server, and a
 * TypeScript program calling it — typed by declarations nobody wrote.
 */
class TestTsClient extends munit.FunSuite {
  import TestTsClient.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !has("node", "--version")

  private def files(dir: java.nio.file.Path, r: Router): Unit =
    java.nio.file.Files.writeString(dir.resolve("model.ts"), TsClient.model(r)): Unit
    java.nio.file.Files.writeString(dir.resolve("client.ts"), TsClient.client(r)): Unit
    java.nio.file.Files.writeString(dir.resolve("usage.ts"), usage): Unit
    // an ES module, as a frontend project declares itself; tsc reads it
    // from here (node guessed it from the syntax, tsc does not)
    java.nio.file.Files.writeString(dir.resolve("package.json"), """{"type": "module"}"""): Unit

  test("a TypeScript program calls the Scala backend through the generated client: typed, and it works") {
    val dir = java.nio.file.Files.createTempDirectory("okay-ts-client")
    val r = router(java.util.concurrent.ConcurrentHashMap[Int, Task]())
    files(dir, r)
    val out = Resource.run[String, Pure](Server.serve(0)(req => r.routes.applyOrElse(req, _ => pure(Response(404, Nil, Http.one(Array.empty[Byte]))))).map { s =>
      val p = ProcessBuilder("node", "usage.ts", s"http://127.0.0.1:${Server.port(s)}")
        .directory(dir.toFile).redirectErrorStream(true).start()
      val text = String(p.getInputStream.readAllBytes())
      p.waitFor(): Unit
      text.trim
    }).runWith
    assertEquals(out,
      """{"made":{"id":1,"title":"write the docs","done":false,"tags":["docs"]},""" +
        """"again":{"id":1,"title":"write the docs","done":false,"tags":["docs"]},"docs":["write the docs"],"status":400}""")
  }

  test("tsc --strict compiles the program against the generated client; a wrong field is refused") {
    assume(has("tsc", "--version"), "tsc is not installed")
    val dir = java.nio.file.Files.createTempDirectory("okay-ts-client-tsc")
    files(dir, router(java.util.concurrent.ConcurrentHashMap[Int, Task]()))
    java.nio.file.Files.writeString(dir.resolve("usage.ts"), usage.replace("const o = { base: process.argv[2] };", "const o = { base: \"http://x\" };")): Unit
    def tsc(file: String): (Int, String) =
      val p = ProcessBuilder("tsc", "--noEmit", "--strict", "--allowImportingTsExtensions",
        "--target", "es2022", "--module", "nodenext", file).directory(dir.toFile).redirectErrorStream(true).start()
      (p.waitFor(), String(p.getInputStream.readAllBytes()))
    val (ok, out) = tsc("usage.ts")
    assertEquals(ok, 0, out)
    java.nio.file.Files.writeString(dir.resolve("bad.ts"),
      java.nio.file.Files.readString(dir.resolve("usage.ts")).replace("docs.map((t) => t.title)", "docs.map((t) => t.name)")): Unit
    val (bad, why) = tsc("bad.ts")
    assertNotEquals(bad, 0)
    assert(why.contains("name"), why)
  }
}
