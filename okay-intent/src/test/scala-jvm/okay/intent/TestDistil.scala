package okay.intent

import okay.given
import okay.codec.{Json, Schema}
import okay.llm.{OpenAi, Transports}
import java.nio.file.{Files, Paths}

/**
 * Distil a corpus with the model, then classify without it
 * (specs/intent-classify.md).
 *
 * Scoped by the learning curve rather than by ambition: the probe is
 * flat past 32 examples, so this is NOT for it. The tiers that were
 * still climbing when the fixture ran out are the ones with no network
 * at all — chargrams (30 → 65%) and the static table (63.3%) — and
 * they are the only candidates for a classifier that needs nothing.
 *
 * The model is used ONCE, offline, in two passes. It writes messages
 * for a class, and then a second pass classifies them back with the
 * shipped prompt; only the ones where generation and classification
 * AGREE are kept. A label the model contradicts the moment it is asked
 * again is not worth training on, and self-consistency is the cheapest
 * filter that says so.
 *
 * EVALUATION NEVER TOUCHES GENERATED DATA. The held-out half of the
 * human-labelled fixture is the only thing scored, so the number
 * cannot be inflated by the corpus that produced it.
 */
class TestDistil extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(3600, "s")

  private val url = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  private val model = sys.env.getOrElse("OKAY_LLM_MODEL",
    "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  private val store = Paths.get("okay-agent/src/test/resources/intent-distilled.json")

  private lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(url.replace("/chat/completions", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def ask(prompt: String, maxTokens: Int = 800): String =
    OpenAi.complete(Transports.http(), "none",
      OpenAi.request(model, Seq(OpenAi.message("user", prompt)), maxTokens = Some(maxTokens)), url)
      .runWith.choices.headOption.flatMap(_.message).flatMap(_.content).getOrElse("")

  /** the generated corpus, as data, so the expensive half runs once */
  final case class Corpus(rows: Vector[Phrasing]) derives Schema

  private def lines(reply: String): Vector[String] =
    reply.split("\n").iterator
      .map(_.trim.replaceAll("^[-*\\d.)\\s]+", "").trim)
      .filter(l => l.length > 12 && !l.startsWith("{") && !l.startsWith("#"))
      .toVector

  private def readCorpus: Vector[Phrasing] =
    if !Files.exists(store) then Vector.empty
    else Json.decode(summon[Schema[Corpus]])(Json.parseValue(Files.readString(store)))
      .map(_.rows).getOrElse(Vector.empty)

  private def writeCorpus(rows: Vector[Phrasing]): Unit =
    Files.createDirectories(store.getParent): Unit
    Files.writeString(store, Json.write(Corpus(rows))): Unit

  /**
   * Generation, RESUMABLE.
   *
   * Thirty-two model calls do not fit in one command's budget, and the
   * first version of this lost the whole corpus when the run was cut
   * off — a batch is written the moment it arrives, so every run adds
   * to what the last one left and the work is never spent twice. The
   * time budget is what makes the exit clean rather than a kill.
   */
  test("live: generate a corpus, adding to whatever is already there") {
    assume(reachable, s"no endpoint at $url")
    val target = sys.env.get("OKAY_DISTIL_TARGET").map(_.toInt).getOrElse(320)
    val budgetMs = sys.env.get("OKAY_DISTIL_BUDGET_MS").map(_.toLong).getOrElse(420000L)
    val started = System.currentTimeMillis()
    var rows = readCorpus
    println(s"\n[distil] starting from ${rows.length} messages, target $target")

    val kinds = Vector(
      "Proposal" -> "proposes a time or a meeting, or offers to move one",
      "Request" -> "asks the reader to do something or send something",
      "Notification" -> "informs, with nothing for the reader to do",
      "Other" -> "is not about arranging anything at all")

    var batch = rows.length / 10
    while rows.length < target && System.currentTimeMillis() - started < budgetMs do
      val (cls, described) = kinds(batch % kinds.length)
      val reply = ask(
        s"Write 8 short work emails, one per line, no numbering and no commentary. " +
        s"Each one $described. Vary the wording, the length and the politeness; " +
        s"this is batch ${batch / kinds.length}, so avoid what an obvious first batch would say.")
      val fresh = lines(reply).take(8).map(Phrasing(_, cls))
      rows = (rows ++ fresh).distinctBy(_.text)
      writeCorpus(rows)
      batch += 1
    println(f"  now ${rows.length} messages after ${(System.currentTimeMillis() - started) / 1000}s")
    assert(rows.nonEmpty, "generation produced nothing at all")
  }

  /**
   * Keep only what the model agrees with on second sight, then train
   * the no-network tiers and score them on HUMAN data.
   */
  test("live: filter by self-consistency and train on the result") {
    assume(reachable, s"no endpoint at $url")
    val corpus = readCorpus
    assume(corpus.length >= 100, s"only ${corpus.length} generated so far — run the generator again")

    val kept = corpus.grouped(10).flatMap { batch =>
      val numbered = batch.zipWithIndex.map((p, i) => s"${i + 1}. ${p.text}").mkString("\n")
      val reply = ask(
        s"For each numbered message, answer with its class on its own line as " +
        s"'<number>. <class>', nothing else. The classes are: " +
        s"${IntentFixture.classes.mkString(", ")}.\n\n$numbered", maxTokens = 400)
      val said = reply.split("\n").iterator.flatMap { l =>
        raw"(\d+)\.\s*(\w+)".r.findFirstMatchIn(l).map(x => x.group(1).toInt -> x.group(2))
      }.toMap
      batch.zipWithIndex.collect { case (p, i) if said.get(i + 1).contains(p.cls) => p }
    }.toVector
    println(f"\n[distil] ${kept.length} of ${corpus.length} survived self-consistency " +
            f"(${kept.length * 100.0 / corpus.length}%.0f%%)")

    val (humanTrain, humanTest) = IntentFixture.labelled.zipWithIndex
      .partition(_._2 % 2 == 1) match
        case (a, b) => (a.map(_._1), b.map(_._1))
    val distilled = kept.map(p => p.text -> p.cls)

    def chargrams(rows: Seq[(String, String)], name: String): Unit =
      if rows.nonEmpty then
        val m = CharGrams.train(rows)
        val right = humanTest.count((t, g) => CharGrams.score(m, t).exists(_.best == g))
        println(f"  chargrams $name%-26s ${right * 100.0 / humanTest.length}%5.1f%% on held-out HUMAN data (${rows.length} rows)")

    chargrams(humanTrain, "fixture alone")
    chargrams(distilled, "distilled alone")
    chargrams(humanTrain ++ distilled, "both")
    assert(kept.nonEmpty)
  }
}
