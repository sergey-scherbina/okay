package okay.demo

import okay.agent.ToolCall
import okay.codec.Json
import okay.rag.Source

/**
 * The repository agent's two tools, without a model and without
 * indexing this repository — `TestRepoAgent` does that and is `Live`
 * tagged for it, which is exactly why the CONTRACT of the tools had no
 * test in the default gate.
 *
 * What is pinned here is the thing stage 2 of specs/optics-outside.md
 * made a rule and this module had not adopted: a tool that cannot do
 * the thing answers with DATA. A model handed prose where every other
 * tool answers `{"error": ...}` has to guess which shape it got, and
 * the two shapes were decided by which module happened to declare the
 * tool — `BoardTools` through `Toolbox`, `RepoAgent` by hand.
 */
class TestRepoTools extends munit.FunSuite {

  /** a two-file repository, built in memory: enough to dispatch */
  private lazy val repo: RepoAgent.Repo = RepoAgent.index(Seq(
    Source("A.scala", "package p\n\nobject A:\n  def twice(n: Int): Int = n * 2\n"),
    Source("B.scala", "package p\n\nobject B:\n  val greeting = \"hi\"\n")))

  private def call(name: String, args: (String, Json)*): String =
    RepoAgent.tools(repo)(name)(ToolCall("c", name, Json.JObj(args.toVector)))

  /** the `{"error": ...}` a tool answers with when it cannot do the
   * thing. `Json.parse` is lossless on a non-JSON string, so prose
   * comes back as a `JStr` and falls through to `None` — which is
   * precisely the distinction under test */
  private def errorOf(answer: String): Option[String] =
    Json.parse(answer) match
      case Json.JObj(fs) => fs.collectFirst { case ("error", Json.JStr(m)) => m }
      case _ => None

  test("a tool called with the wrong arguments answers with data, not prose") {
    // `name` is required, so this is the shape a model gets wrong
    val answer = call("definition")
    assert(errorOf(answer).isDefined,
      s"a decode failure answered with prose a model cannot read: $answer")
    assert(errorOf(answer).exists(_.contains("definition")),
      s"the error does not say which tool failed: $answer")
  }

  test("the same holds for the other tool") {
    val answer = call("read_file")
    assert(errorOf(answer).isDefined, answer)
  }

  test("what the agent declares is exactly what it dispatches") {
    // RepoMcp hands Server.Serving these two structures separately, so
    // a tool declared and undispatched is a "tool not found" the model
    // was invited to call. Both now come from one vector
    assertEquals(RepoAgent.specs(repo).map(_.name).toSet, RepoAgent.tools(repo).keySet)
    assertEquals(RepoAgent.toolbox(repo).duplicates, Vector.empty)
  }

  test("a tool that works still answers the thing itself") {
    val found = call("definition", "name" -> Json.JStr("twice"))
    assert(found.contains("def twice"), found)
    val read = call("read_file", "path" -> Json.JStr("B.scala"))
    assert(read.contains("greeting"), read)
  }
}
