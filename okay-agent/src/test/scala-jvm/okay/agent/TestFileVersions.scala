package okay.agent

import okay.Handler
import okay.codec.Json

import java.nio.file.{Files, Path}

/**
 * A version tree that outlives the process (specs/llm-agentic.md,
 * "Journal versions"): the same `Versions` contract as the in-memory
 * one, plus the two things only a file can get wrong — surviving a
 * restart, and staying readable when something else lands in the
 * directory.
 */
class TestFileVersions extends munit.FunSuite {

  def call(name: String, arg: String): ToolCall =
    ToolCall("id", name, Json.JObj(Vector(("q", Json.JStr(arg)))))

  def world(answers: Map[String, String]): Handler[Tool] = new Handler[Tool]:
    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(c) =>
        val q = c.args match
          case Json.JObj(fs) => fs.find(_._1 == "q").map(_._2).collect { case Json.JStr(s) => s }.getOrElse("")
          case _ => ""
        answers.getOrElse(s"${c.name}:$q", s"no answer for ${c.name}:$q")

  def recorded(w: Handler[Tool], calls: Seq[ToolCall],
               p: Rerun.Provenance = Rerun.Provenance()): Rerun.Version =
    val j = Durable.MemoryJournal()
    val h = Durable.tools(w, j)()
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)
    Rerun.Version.root(j.all, p)

  val june = world(Map("read:a" -> "alpha", "read:b" -> "beta", "read:c" -> "gamma"))
  val calls = Seq(call("read", "a"), call("read", "b"), call("read", "c"))

  def tmp(): Path = Files.createTempDirectory("okay-versions")

  test("a version written to a directory reads back whole, entries and all") {
    val dir = tmp()
    val base = recorded(june, calls, Rerun.Provenance(revision = "june", model = "m", tools = "read"))
    FileVersions(dir).put(base)

    // a NEW store over the same directory: only the files cross this line
    val got = FileVersions(dir).get(base.id).get
    assertEquals(got.id, base.id)
    assertEquals(got.entries, base.entries)
    assertEquals(got.provenance, base.provenance)
    assertEquals(got.parent, None)
    assertEquals(got.divergence, None)
  }

  test("a branched version keeps its parent, its branch point and its divergence") {
    val dir = tmp()
    val store = FileVersions(dir)
    val base = recorded(june, calls, Rerun.Provenance(revision = "june"))
    store.put(base)

    val september = world(Map("read:a" -> "alpha", "read:b" -> "B2", "read:c" -> "C2"))
    val (run, h) = Rerun.live(base, september, Rerun.OnDiverge.ForkWithLiveModel,
      Rerun.Provenance(revision = "sept"), store)
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)
    val v2 = run.outcome.version

    val reread = FileVersions(dir).get(v2.id).get
    assertEquals(reread.parent, Some(base.id))
    assertEquals(reread.branchedAt, Some(1))
    assertEquals(reread.divergence.map(_.kind), Some(Rerun.Divergence.Kind.Answer))
    assertEquals(reread.divergence.map(_.recorded), Some("beta"))
    assertEquals(reread.divergence.map(_.got), Some("B2"))
    assertEquals(reread.entries.flatMap(_.answer), Vector("alpha", "B2", "C2"))
  }

  test("a lineage walks across a RESTART — the tree is in the parent pointers, not the layout") {
    val dir = tmp()
    val store = FileVersions(dir)
    val base = recorded(june, calls, Rerun.Provenance(revision = "june"))
    store.put(base)

    val sept = world(Map("read:a" -> "alpha", "read:b" -> "B2", "read:c" -> "gamma"))
    val (r1, h1) = Rerun.live(base, sept, Rerun.OnDiverge.ForkWithLiveModel,
      Rerun.Provenance(revision = "sept"), store)
    calls.foreach(c => h1.handle(Tool.Call(c)): Unit)
    val v2 = r1.outcome.version

    val oct = world(Map("read:a" -> "A3", "read:b" -> "B2", "read:c" -> "gamma"))
    val (r2, h2) = Rerun.live(v2, oct, Rerun.OnDiverge.ForkWithLiveModel,
      Rerun.Provenance(revision = "oct"), store)
    calls.foreach(c => h2.handle(Tool.Call(c)): Unit)
    val v3 = r2.outcome.version

    // a fresh store, as a later process sees it
    val later = FileVersions(dir)
    assertEquals(later.lineage(v3.id).map(_.provenance.revision), Vector("oct", "sept", "june"))
    assertEquals(later.all.size, 3)
  }

  test("a version read back from disk still replays, with no world at all") {
    val dir = tmp()
    val base = recorded(june, calls)
    FileVersions(dir).put(base)
    val reread = FileVersions(dir).get(base.id).get
    val replay = Durable.replaying(reread.journal)
    assertEquals(calls.map(c => replay.handle(Tool.Call(c))), Seq("alpha", "beta", "gamma"))
  }

  test("the file is legible: a person with `cat` can read what a version says") {
    val dir = tmp()
    val base = recorded(june, calls, Rerun.Provenance(revision = "june", note = "the first run"))
    FileVersions(dir).put(base)
    val text = Files.readString(dir.resolve(s"${base.id}.json"))
    assert(text.contains("\"revision\":\"june\""), text)
    assert(text.contains("\"the first run\""), text)
    assert(text.contains("alpha"), text)
  }

  test("a stray file does not make the rest of the tree unreadable") {
    val dir = tmp()
    val store = FileVersions(dir)
    val base = recorded(june, calls)
    store.put(base)
    Files.writeString(dir.resolve("not-a-version.json"), "{ this is not json"): Unit
    Files.writeString(dir.resolve("README.txt"), "notes to self"): Unit

    assertEquals(FileVersions(dir).all.map(_.id), Vector(base.id))
    assertEquals(FileVersions(dir).get(base.id).map(_.id), Some(base.id))
  }

  test("an unknown id is None, not a failure") {
    assertEquals(FileVersions(tmp()).get("v00000000"), None)
  }

  test("a rewritten version replaces its file rather than accumulating") {
    val dir = tmp()
    val store = FileVersions(dir)
    val base = recorded(june, calls)
    store.put(base)
    store.put(base)
    assertEquals(store.all.size, 1)
    assertEquals(Files.list(dir).count(), 1L)
  }
}
