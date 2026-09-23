package okay.live

import java.nio.file.{Files, Path}
import okay.*
import okay.given
import okay.codec.{Json, Schema, Stubs}
import okay.http.{Http, Response, Server}

object TestLiveTs:
  final case class Task(title: String, done: Boolean) derives Schema
  final case class Board(name: String, tasks: Vector[Task]) derives Schema

  // a DEF: an object val summoning a nested case class's Schema stalled once (ts-types-check)
  def paths: String = Stubs.typescriptPaths(summon[Schema[Board]], "Board")

  // no margin: the docs quote these lines
  val app: String = """
import { live } from "./live.ts";
import type { BoardPaths } from "./board.ts";
declare const process: { argv: string[] };

const board = live<BoardPaths>({ base: process.argv[2] });
const seen: (string | null)[] = [];
await new Promise<void>((finished) => {
  const stop = board.watch("tasks[0].title", (title) => {
    seen.push(title);
    if (seen.length === 1) void board.set("tasks[0].title", "write the docs, today");
    else { stop(); finished(); }
  });
});
const absent = await board.set("tasks[9].title", "nothing there").then(() => "written", (e) => e.name);
console.log(JSON.stringify({ seen, absent }));
"""

/** typescript-types T11 end to end (Live: binds a port, runs Node and tsc) */
class TestLiveTs extends munit.FunSuite {
  import TestLiveTs.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !has("node", "--version")

  private def project(): Path =
    val dir = Files.createTempDirectory("okay-live-ts")
    Files.writeString(dir.resolve("board.ts"), paths): Unit
    Files.writeString(dir.resolve("live.ts"), LiveHttp.client): Unit
    Files.writeString(dir.resolve("app.ts"), app): Unit
    Files.writeString(dir.resolve("package.json"), """{"type": "module"}"""): Unit
    dir

  test("a TypeScript frontend watches a typed path of a Scala document, writes it, and is told") {
    val dir = project()
    val w = Watched[Board](Json.parse("""{"name":"docs","tasks":[{"title":"write the docs","done":false}]}"""))
    val routes = LiveHttp.routes(w).routes
    val out = Resource.run[String, Pure](Server.serve(0)(req =>
      routes.applyOrElse(req, _ => pure(Response(404, Nil, Http.one(Array.empty[Byte]))))).map { s =>
      val p = ProcessBuilder("node", "app.ts", s"http://127.0.0.1:${Server.port(s)}")
        .directory(dir.toFile).redirectErrorStream(true).start()
      val text = String(p.getInputStream.readAllBytes())
      p.waitFor(): Unit
      text.trim
    }).runWith
    assertEquals(out, """{"seen":["write the docs","write the docs, today"],"absent":"LiveRefused"}""")
  }

  test("tsc --strict: a path the schema has no place for, or a value of the wrong type, does not compile") {
    assume(has("tsc", "--version"), "tsc is not installed")
    val dir = project()
    def tsc(file: String): (Boolean, String) =
      val p = ProcessBuilder("tsc", "--noEmit", "--strict", "--allowImportingTsExtensions", "--target", "es2022",
        "--module", "nodenext", "--lib", "es2022,dom", file).directory(dir.toFile).redirectErrorStream(true).start()
      val said = String(p.getInputStream.readAllBytes())
      (p.waitFor() == 0, said)
    val (ok, out) = tsc("app.ts")
    assert(ok, out)
    Files.writeString(dir.resolve("path.ts"), app.replace("""board.watch("tasks[0].title"""", """board.watch("tasks[0].nope"""")): Unit
    val (pathOk, pathSaid) = tsc("path.ts")
    assert(!pathOk && pathSaid.contains("tasks[0].nope"), pathSaid)
    Files.writeString(dir.resolve("value.ts"), app.replace("""board.set("tasks[0].title", "write the docs, today")""", """board.set("tasks[0].done", "yes")""")): Unit
    val (valueOk, valueSaid) = tsc("value.ts")
    assert(!valueOk && valueSaid.contains("boolean"), valueSaid)
  }
}

/** the React hook, typechecked where @types/react can be installed OFFLINE (from npm's cache); skipped, saying so, where it cannot */
class TestLiveReact extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def run(dir: Path, cmd: String*): (Boolean, String) =
    val p = ProcessBuilder(cmd*).directory(dir.toFile).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes())
    (p.waitFor() == 0, said)

  test("useWatch typechecks against React's own types") {
    val dir = Files.createTempDirectory("okay-live-react")
    Files.writeString(dir.resolve("package.json"), """{"name": "app", "private": true, "type": "module"}"""): Unit
    val (installed, _) = scala.util.Try(run(dir, "npm", "install", "--offline", "--no-audit", "--no-fund", "--silent", "@types/react"))
      .getOrElse((false, ""))
    assume(installed, "@types/react is not in npm's cache and this check does not reach the network")
    Files.writeString(dir.resolve("live.ts"), LiveHttp.client): Unit
    Files.writeString(dir.resolve("live-react.ts"), LiveHttp.react): Unit
    val (ok, out) = run(dir, "tsc", "--noEmit", "--strict", "--allowImportingTsExtensions", "--target", "es2022",
      "--module", "nodenext", "--lib", "es2022,dom", "live-react.ts")
    assert(ok, out)
  }
}

/** the HTTP side's answers, without a port (default gate) */
class TestLiveHttp extends munit.FunSuite {
  import TestLiveTs.*
  import okay.http.{Method, Request}

  private val w = Watched[Board](Json.parse("""{"name":"docs","tasks":[{"title":"a","done":false}]}"""))
  private val routes = LiveHttp.routes(w).routes

  private def post(body: String): Int =
    routes(Request(Method.Post, "/live/set", body = okay.http.Body.Text(body))).runWith.status

  test("set: 204 where the key has a value, 409 where its place is absent, 400 where the schema has none") {
    assertEquals(post("""{"key":"tasks[0].done","value":true}"""), 204)
    assertEquals(w.focus("tasks[0].done"), Right(Some(Json.JBool(true))))
    assertEquals(post("""{"key":"tasks[3].done","value":true}"""), 409)
    assertEquals(post("""{"key":"tasks[0].nope","value":1}"""), 400)
    assertEquals(post("""{"value":1}"""), 400)
  }

  test("the paths interface a TypeScript frontend is typed by") {
    // no margin: the docs quote these lines
    val expected = """
export interface BoardPaths {
  "": Board;
  "name": string;
  "tasks": Task[];
  [k: `tasks[${number}]`]: Task | null;
  [k: `tasks[${number}].title`]: string | null;
  [k: `tasks[${number}].done`]: boolean | null;
}
"""
    assert(paths.contains(expected), paths)
  }

  test("watch: the focused value first, as a server-sent event") {
    val res = routes(Request(Method.Get, "/live/watch?key=name")).runWith
    assert(res.headers.exists((k, v) => k.equalsIgnoreCase("content-type") && v.contains("text/event-stream")), res.headers)
  }
}
