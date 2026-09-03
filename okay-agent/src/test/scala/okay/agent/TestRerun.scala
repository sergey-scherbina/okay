package okay.agent

import okay.Handler
import okay.codec.Json

/**
 * A journal rerun against a LIVE world (specs/llm-agentic.md,
 * "Journal versions"): what happens when the world has moved since
 * the recording, and why a divergence branches a version instead of
 * patching one.
 */
class TestRerun extends munit.FunSuite {

  def call(name: String, arg: String): ToolCall =
    ToolCall("id", name, Json.JObj(Vector(("q", Json.JStr(arg)))))

  /** a world that answers from a table, and can be swapped for a
   * different world between the recording and the rerun */
  def world(answers: Map[String, String]): Handler[Tool] = new Handler[Tool]:
    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(c) =>
        val q = c.args match
          case Json.JObj(fs) => fs.find(_._1 == "q").map(_._2).collect { case Json.JStr(s) => s }.getOrElse("")
          case _ => ""
        answers.getOrElse(s"${c.name}:$q", s"no answer for ${c.name}:$q")

  /** record a journal by running the calls against `w` */
  def recorded(w: Handler[Tool], calls: Seq[ToolCall],
               provenance: Rerun.Provenance = Rerun.Provenance()): Rerun.Version =
    val j = Durable.MemoryJournal()
    val h = Durable.tools(w, j)()
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)
    Rerun.Version.root(j.all, provenance)

  val june = world(Map("read:a" -> "alpha", "read:b" -> "beta", "read:c" -> "gamma"))
  val calls = Seq(call("read", "a"), call("read", "b"), call("read", "c"))

  test("a world that has not moved reproduces, and leaves no new version") {
    val base = recorded(june, calls)
    val store = Rerun.MemoryVersions()
    val (run, h) = Rerun.live(base, june, Rerun.OnDiverge.Loud, versions = store)
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)

    val out = run.outcome
    assert(out.reproduced, out.divergence.toString)
    assertEquals(out.version.id, base.id, "a rerun that reproduced has nothing new to say")
    assertEquals(store.all, Vector.empty, "and stores nothing")
  }

  test("LOUD: the first divergence stops the run and names both sides") {
    val base = recorded(june, calls)
    val september = world(Map("read:a" -> "alpha", "read:b" -> "BETA-CHANGED", "read:c" -> "gamma"))
    val (_, h) = Rerun.live(base, september, Rerun.OnDiverge.Loud)

    h.handle(Tool.Call(calls(0))): Unit
    val e = intercept[Rerun.Diverged](h.handle(Tool.Call(calls(1))): Unit)
    assertEquals(e.at.seq, 1)
    assertEquals(e.at.kind, Rerun.Divergence.Kind.Answer)
    assertEquals(e.at.recorded, "beta")
    assertEquals(e.at.got, "BETA-CHANGED")
    assert(e.getMessage.contains("no longer reproduces"), e.getMessage)
  }

  test("FORK: a divergence branches a version — shared prefix, live tail") {
    val base = recorded(june, calls)
    val september = world(Map("read:a" -> "alpha", "read:b" -> "BETA-CHANGED", "read:c" -> "GAMMA-TOO"))
    val store = Rerun.MemoryVersions()
    val prov = Rerun.Provenance(revision = "sept", model = "m", tools = "read")
    val (run, h) = Rerun.live(base, september, Rerun.OnDiverge.ForkWithLiveModel, prov, store)
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)

    val out = run.outcome
    assert(!out.reproduced)
    assertEquals(out.divergence.map(_.seq), Some(1))
    assertEquals(out.version.parent, Some(base.id))
    assertEquals(out.version.branchedAt, Some(1))
    assertEquals(out.version.provenance, prov)

    // the prefix before the divergence is what the parent had
    assertEquals(out.version.entries.head.answer, base.entries.head.answer)
    // and everything from the divergence on is the LIVE world's, not the journal's
    assertEquals(out.version.entries(1).answer, Some("BETA-CHANGED"))
    assertEquals(out.version.entries(2).answer, Some("GAMMA-TOO"),
      "past a divergence the run is live — the journal's 'gamma' must not come back")
    assertEquals(store.all.map(_.id), Vector(out.version.id))
  }

  test("a fork is not silent: the divergence is on the version and in the outcome") {
    val base = recorded(june, calls)
    val september = world(Map("read:a" -> "MOVED", "read:b" -> "beta", "read:c" -> "gamma"))
    val (run, h) = Rerun.live(base, september, Rerun.OnDiverge.ForkWithLiveModel)
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)

    val out = run.outcome
    assert(out.divergence.isDefined, "a fork must still report")
    assertEquals(out.version.divergence, out.divergence,
      "and the version carries it, so a reader of the store alone still learns it")
    assert(out.divergence.get.describe.contains("the world answered differently"),
      out.divergence.get.describe)
  }

  test("only the FIRST divergence is the branch point, later ones do not move it") {
    val base = recorded(june, calls)
    val september = world(Map("read:a" -> "alpha", "read:b" -> "B2", "read:c" -> "C2"))
    val (run, h) = Rerun.live(base, september, Rerun.OnDiverge.ForkWithLiveModel)
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)
    assertEquals(run.outcome.version.branchedAt, Some(1),
      "the branch is where the runs parted, not where they last differed")
  }

  test("a program that asks something ELSE is a Call divergence, told apart from an Answer one") {
    val base = recorded(june, calls)
    val store = Rerun.MemoryVersions()
    val (run, h) = Rerun.live(base, june, Rerun.OnDiverge.ForkWithLiveModel, versions = store)
    h.handle(Tool.Call(calls(0))): Unit
    h.handle(Tool.Call(call("read", "z"))): Unit   // the code changed: a different question

    val d = run.outcome.divergence.get
    assertEquals(d.kind, Rerun.Divergence.Kind.Call)
    assertEquals(d.seq, 1)
    assert(d.describe.contains("the program asked something else"), d.describe)
  }

  test("running past the end of the journal is a divergence too, not a quiet success") {
    val base = recorded(june, calls.take(2))
    val (run, h) = Rerun.live(base, june, Rerun.OnDiverge.ForkWithLiveModel)
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)
    val d = run.outcome.divergence.get
    assertEquals(d.seq, 2)
    assertEquals(d.kind, Rerun.Divergence.Kind.Call)
    assertEquals(d.recorded, "(no entry)")
  }

  test("versions form a tree: a lineage walks back to the root") {
    val store = Rerun.MemoryVersions()
    val base = recorded(june, calls, Rerun.Provenance(revision = "june"))
    store.put(base)

    val september = world(Map("read:a" -> "alpha", "read:b" -> "B2", "read:c" -> "gamma"))
    val (run1, h1) = Rerun.live(base, september, Rerun.OnDiverge.ForkWithLiveModel,
      Rerun.Provenance(revision = "sept"), store)
    calls.foreach(c => h1.handle(Tool.Call(c)): Unit)
    val v2 = run1.outcome.version

    val october = world(Map("read:a" -> "A3", "read:b" -> "B2", "read:c" -> "gamma"))
    val (run2, h2) = Rerun.live(v2, october, Rerun.OnDiverge.ForkWithLiveModel,
      Rerun.Provenance(revision = "oct"), store)
    calls.foreach(c => h2.handle(Tool.Call(c)): Unit)
    val v3 = run2.outcome.version

    assertEquals(store.lineage(v3.id).map(_.provenance.revision), Vector("oct", "sept", "june"))
    assertEquals(v3.parent, Some(v2.id))
    assertEquals(v2.parent, Some(base.id))
    assertEquals(base.parent, None)
  }

  /**
   * The rule the fork's NAME exists to carry, pinned as behaviour: past the
   * fork point the journal is abandoned, so a caller who kept scripting the
   * model from the recording would be feeding it replies written against the
   * OLD tool answer. The handler cannot see the caller's model, so what it
   * can do — and does — is stop consulting the journal at all from the fork,
   * and say where that fork is, so the caller knows from which step its model
   * must be live.
   */
  test("past the fork the journal is abandoned entirely, and the outcome says where") {
    val base = recorded(june, calls)
    val september = world(Map("read:a" -> "alpha", "read:b" -> "B2", "read:c" -> "C2"))
    val (run, h) = Rerun.live(base, september, Rerun.OnDiverge.ForkWithLiveModel)
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)

    val out = run.outcome
    assertEquals(out.version.branchedAt, Some(1),
      "the fork point is what tells a caller from which step its model must be live")
    // every entry from the fork on is the live world's, never the journal's
    assertEquals(out.version.entries.drop(1).flatMap(_.answer), Vector("B2", "C2"))
    assert(!out.version.entries.drop(1).flatMap(_.answer).contains("beta"),
      "the journal's answers must not reappear after the fork")
  }

  test("Loud is the mode a scripted model needs: it stops before the first stale reply") {
    val base = recorded(june, calls)
    val september = world(Map("read:a" -> "alpha", "read:b" -> "MOVED", "read:c" -> "gamma"))
    val (run, h) = Rerun.live(base, september, Rerun.OnDiverge.Loud)
    h.handle(Tool.Call(calls(0))): Unit
    intercept[Rerun.Diverged](h.handle(Tool.Call(calls(1))): Unit): Unit
    // nothing past the divergence ran, so no recorded model reply was ever
    // consumed against an answer it was not written for
    assertEquals(run.divergence, None, "Loud throws rather than recording a branch")
  }

  test("a version IS a journal: Durable.replaying reads one back with no world at all") {
    val base = recorded(june, calls)
    val replay = Durable.replaying(base.journal)
    assertEquals(calls.map(c => replay.handle(Tool.Call(c))), Seq("alpha", "beta", "gamma"))
  }

  test("a branched version replays as ITS OWN run, not the parent's") {
    val base = recorded(june, calls)
    val september = world(Map("read:a" -> "alpha", "read:b" -> "B2", "read:c" -> "C2"))
    val (run, h) = Rerun.live(base, september, Rerun.OnDiverge.ForkWithLiveModel)
    calls.foreach(c => h.handle(Tool.Call(c)): Unit)

    val replay = Durable.replaying(run.outcome.version.journal)
    assertEquals(calls.map(c => replay.handle(Tool.Call(c))), Seq("alpha", "B2", "C2"))
  }
}
