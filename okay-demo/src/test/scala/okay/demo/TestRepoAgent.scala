package okay.demo

import okay.agent.ToolCall
import okay.rag.*

/**
 * The demo's own tests — and the first one is a regression for a bug
 * that only an application could find: `load(".")` indexed NOTHING,
 * because the walk skips hidden directories and "." is a hidden name.
 * Every test in the library passes absolute paths, so nothing caught
 * it; the first real run printed "indexed 0 files".
 */
class TestRepoAgent extends munit.FunSuite {

  test("load walks a relative root — '.' is not a hidden directory") {
    val here = java.io.File(".")
    val sources = RepoAgent.load(here, limit = 50)
    assert(sources.nonEmpty, "a relative root indexed nothing")
    assert(sources.forall(_.id.endsWith(".scala")))
    // the ids are relative to the root, so they are readable citations
    assert(sources.forall(s => !s.id.startsWith("/")), sources.take(3).map(_.id).toString)
  }

  test("load still skips build output and version control") {
    val sources = RepoAgent.load(java.io.File("."), limit = 400)
    assert(!sources.exists(_.id.contains("/target/")), "target was indexed")
    assert(!sources.exists(_.id.startsWith(".git")), ".git was indexed")
  }

  test("indexing this repository finds its own definitions") {
    val repo = RepoAgent.index(RepoAgent.load(java.io.File("."), limit = 400))
    // the library's own names, found by parsing its source
    for name <- Seq("translate", "window", "reparse") do
      assert(repo.index.definition(name).nonEmpty, s"no definition of '$name' found")
    // and a definition quotes its file exactly
    val sym = repo.index.definition("window").head
    val src = repo.corpus.sources(sym.source)
    assert(Symbols.segment(sym, src).quotes(src))
  }

  test("the tools answer without a model in play") {
    val repo = RepoAgent.index(RepoAgent.load(java.io.File("."), limit = 400))
    val table = RepoAgent.tools(repo)

    val found = table("definition")(ToolCall("c", "definition",
      okay.codec.Json.JObj(Vector("name" -> okay.codec.Json.JStr("translate")))))
    assert(found.contains("def translate"), found.take(120))

    val missing = table("definition")(ToolCall("c", "definition",
      okay.codec.Json.JObj(Vector("name" -> okay.codec.Json.JStr("nope")))))
    assert(missing.contains("no definition"), missing)

    // an unreadable path is an answer, not a fault
    val bad = table("read_file")(ToolCall("c", "read_file",
      okay.codec.Json.JObj(Vector("path" -> okay.codec.Json.JStr("nowhere.scala")))))
    assert(bad.contains("no such file"), bad)
  }
}
