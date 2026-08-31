package okay.agent

import okay.{!, +, ==>, Async, Handler}
import okay.given
import okay.rag.*
import java.util.concurrent.atomic.AtomicInteger

/**
 * Grounding through a NATURAL TRANSFORMATION instead of a comonadic
 * handler, and the one thing that proves it was worth doing: a
 * retriever that CANNOT answer immediately.
 *
 * `Handler[Context]` is `Context ==> Id`. An operation interpreted
 * into `Id` must produce a value, so it must finish — which is why
 * `Grounded.context` demands `Retriever[Pure]`, and why a retriever
 * that waits on anything could not be used for grounded recall at
 * all. `Grounded.translating` is `Context ==> ([X] =>> X ! F)`: the
 * operation answers with a PROGRAM, `!.translate` forwards `F` to the
 * surrounding row, and it runs out where a driver exists.
 */
class TestGroundedTranslating extends munit.FunSuite {

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

  val segments: Seq[Segment] = files.flatMap(f => Ingest.segment(f, 400)(_.length))
  val postings: Postings = Keyword.index(segments)

  /** ask, then recall — written in the row it needs and no more */
  val ask: Seq[Turn] ! (Context + Async) =
    okay.effect[Context + Async, Unit](
      Context.Remember(Turn.User("how do I greet someone by name?")))
      .flatMap(_ => okay.effect[Context + Async, Seq[Turn]](Context.Recall()))

  /**
   * A retriever that answers from ANOTHER THREAD, through the Async
   * callback. Nothing in `Id` can express this: at the moment the
   * operation is interpreted, the value does not exist yet.
   */
  def remote(calls: AtomicInteger): Retriever[Async] = new Retriever[Async]:
    def retrieve(query: String, k: Int): Seq[Scored] ! Async =
      okay.effect(Async.Await[Seq[Scored]] { cb =>
        val t = Thread.ofVirtual().start(() =>
          Thread.sleep(20)
          calls.incrementAndGet()
          cb(Right(Keyword.search(postings, query, k))))
        () => t.interrupt()
      })

  test("a retriever that must suspend grounds recall through translate") {
    val calls = AtomicInteger(0)
    var seen: Seq[Turn] = Seq.empty

    val (_, nt) = Grounded.translating(
      Compact.window(4000)(Compact.chars), remote(calls),
      budget = 4000, share = 0.6, k = 2, onRecall = v => seen = v)(Compact.chars)

    // Context is interpreted INTO Async, which then leaves through the
    // ordinary async driver — the point of the whole exercise
    val grounded: Seq[Turn] ! Async = okay.!.translate(ask)(nt)

    Async.runAsync(grounded).map { turns =>
      assertEquals(calls.get, 1, "the retrieval did not actually run")
      assert(turns.exists(t => Compact.text(t).contains("Greeter")),
        s"the code never reached recall: ${turns.map(t => Compact.text(t).take(40))}")
      assertEquals(seen.map(Compact.text), turns.map(Compact.text),
        "onRecall saw something other than the model would")
    }
  }

  test("the same policy, the same assembly — only where retrieval runs moved") {
    // the pure form and the translating form agree on the same corpus:
    // the natural transformation is a WIDER interpretation of the same
    // grounding, not a different grounding
    var pureSeen: Seq[Turn] = Seq.empty
    var ntSeen: Seq[Turn] = Seq.empty

    val (_, handler) = Grounded.context(
      Compact.window(4000)(Compact.chars), Retrieve.keyword(postings),
      budget = 4000, share = 0.6, k = 2,
      onRecall = v => pureSeen = v)(Compact.chars)

    val (_, nt) = Grounded.translating(
      Compact.window(4000)(Compact.chars), remote(AtomicInteger(0)),
      budget = 4000, share = 0.6, k = 2, onRecall = v => ntSeen = v)(Compact.chars)

    given Handler[Context] = handler
    given Handler[Context + Async] = okay.Handler.union[Context, Async]
    val viaHandler = ask.runWith

    Async.runAsync(okay.!.translate(ask)(nt)).map { viaTranslate =>
      assertEquals(viaTranslate.map(Compact.text), viaHandler.map(Compact.text))
      assertEquals(ntSeen.map(Compact.text), pureSeen.map(Compact.text))
    }
  }

  test("a failing retrieval is an error in the row, not a thrown exception") {
    // the other thing Id cannot hold. Through the natural
    // transformation the failure is the row's, and it surfaces where
    // the program was RUN rather than inside context assembly.
    val boom = new Retriever[Async]:
      def retrieve(query: String, k: Int): Seq[Scored] ! Async =
        okay.effect(Async.Await[Seq[Scored]] { cb =>
          cb(Left(RuntimeException("the index is offline")))
          () => ()
        })

    val (_, nt) = Grounded.translating(
      Compact.window(4000)(Compact.chars), boom, budget = 4000)(Compact.chars)

    Async.runAsync(okay.!.translate(ask)(nt)).failed.map { e =>
      assertEquals(e.getMessage, "the index is offline")
    }
  }
}
