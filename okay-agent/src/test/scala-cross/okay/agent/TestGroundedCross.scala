package okay.agent

import okay.{!, +, Async}
import okay.given
import okay.rag.*

/**
 * Grounded recall where retrieval must WAIT — on both platforms, from
 * one source, and JS is the half that matters.
 *
 * `Grounded.context` builds a `Handler[Context]`, which is
 * `Context ==> Id`. An operation interpreted into `Id` must produce a
 * value, so it must finish; on the JVM it can at least park a thread
 * to get there, and on JS there is nothing to park. That is the same
 * finding `TestAgentCross` records for `Handler[Model]`, met again
 * one layer along — and the same answer: do not HANDLE the effect,
 * interpret it into a row and let the driver finish the job.
 *
 * `Grounded.translating` is `Context ==> ([X] =>> X ! F)`. `Recall()`
 * answers with a program, `!.translate` forwards `F` outward, and
 * `Async.runAsync` runs it: the event loop on JS, the callback drive
 * on the JVM. The grounding policy is byte-for-byte the same code.
 */
class TestGroundedCross extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  val files = Seq(
    Source("Greeter.scala",
      "/** Greets people by name. */\n" +
        "class Greeter(name: String) {\n" +
        "  def hello: String = \"Hello, \" + name\n" +
        "}\n"),
    Source("Http.scala",
      "/** Sends requests over the network. */\n" +
        "class HttpClient(timeout: Int) {\n" +
        "  def get(url: String): String = fetch(url)\n" +
        "}\n"))

  val postings: Postings =
    Keyword.index(files.flatMap(f => Ingest.segment(f, 400)(_.length)))

  val ask: Seq[Turn] ! (Context + Async) =
    okay.effect[Context + Async, Unit](
      Context.Remember(Turn.User("how do I greet someone by name?")))
      .flatMap(_ => okay.effect[Context + Async, Seq[Turn]](Context.Recall()))

  /**
   * A retriever that cannot answer now: it rides the platform timer,
   * which is a virtual thread's park on the JVM and a `setTimeout` on
   * JS. Nothing valued in `Id` can express either.
   */
  val remote: Retriever[Async] = new Retriever[Async]:
    def retrieve(query: String, k: Int): Seq[Scored] ! Async =
      Async.sleep(10).map(_ => Keyword.search(postings, query, k))

  test("retrieval that must wait still grounds recall, on either platform") {
    var seen: Seq[Turn] = Seq.empty
    val (_, nt) = Grounded.translating(
      Compact.window(4000)(Compact.chars), remote,
      budget = 4000, share = 0.6, k = 2, onRecall = v => seen = v)(Compact.chars)

    Async.runAsync(okay.!.translate(ask)(nt)).map { turns =>
      assert(turns.exists(t => Compact.text(t).contains("Greeter")),
        s"the code never reached recall: ${turns.map(t => Compact.text(t).take(40))}")
      // the passage carries its provenance header, as it does in the
      // comonadic form — the assembly is the same function
      assert(turns.exists(t => Compact.text(t).contains("Greeter.scala")),
        "the provenance header was lost on the way through the row")
      assertEquals(seen.map(Compact.text), turns.map(Compact.text))
    }
  }

  test("the budget still binds when retrieval arrives from the row") {
    // share = 0.5 of a budget too small for both passages: the policy
    // is unchanged by where the hits came from
    val tiny = 220
    val (_, nt) = Grounded.translating(
      Compact.window(tiny)(Compact.chars), remote,
      budget = tiny, share = 0.5, k = 2)(Compact.chars)

    Async.runAsync(okay.!.translate(ask)(nt)).map { turns =>
      val used = turns.map(Compact.chars).sum
      assert(used <= tiny, s"the assembly spent $used of $tiny")
      assert(turns.nonEmpty, "the budget starved everything")
    }
  }
}
