package okay.intent

import okay.agent.{FileVersions, Rerun}

import okay.given
import okay.codec.Schema
import okay.llm.{OpenAi, Transports}
import okay.agent.Durable.Entry
import java.nio.file.{Files, Paths}

/**
 * Make the recording that `TestEvalJournal` replays.
 *
 * Live-tagged and run by hand, because it is the only part that costs
 * a model. One reply per fixture message, stored as a `Rerun.Version`
 * through the store that already exists — a recording IS a journal, so
 * nothing new had to be invented to hold it.
 *
 * Each entry carries the PROMPT'S fingerprint, which is what makes the
 * replay honest: change the prompt and the replay refuses rather than
 * scoring old answers against a new question.
 */
class TestClassifyRecord extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(1800, "s")

  import IntentFixture.Meeting
  private given sMeeting: Schema[Meeting] = summon[Schema[Meeting]]

  val url = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  val model = sys.env.getOrElse("OKAY_LLM_MODEL",
    "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  val key = sys.env.getOrElse("OKAY_LLM_KEY", "none")

  lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(url.replace("/chat/completions", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def ask(prompt: String): String =
    OpenAi.complete(Transports.http(), key,
      OpenAi.request(model, Seq(OpenAi.message("user", prompt)), maxTokens = Some(1200)), url)
      .runWith.choices.headOption.flatMap(_.message).flatMap(_.content).getOrElse("")

  test("live: record the model's replies for the whole fixture") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    val dir = Paths.get("okay-agent/src/test/resources/intent-journal")
    Files.createDirectories(dir): Unit

    val probe = Classify.prompt[Meeting]("PROBE", IntentFixture.meetingExamples)
    val fp = f"${probe.hashCode}%08x"

    val entries = IntentFixture.labelled.zipWithIndex.map { case ((message, _), i) =>
      val reply = ask(Classify.prompt[Meeting](message, IntentFixture.meetingExamples))
      Entry(i, "classify", fp, message, Some(reply))
    }.toVector

    val version = Rerun.Version.root(entries, Rerun.Provenance(
      model = model, note = "intent classification over IntentFixture.labelled"))
    // one version per recording: the store keys by id, so re-recording
    // after a prompt change leaves the old one beside the new rather
    // than overwriting the evidence
    for old <- new FileVersions(dir).all do
      Files.deleteIfExists(dir.resolve(s"${old.id}.json")): Unit
    new FileVersions(dir).put(version)
    println(s"recorded ${entries.length} replies as ${version.id} under $dir")
    assertEquals(entries.count(_.answer.exists(_.nonEmpty)), entries.length)
  }
}
