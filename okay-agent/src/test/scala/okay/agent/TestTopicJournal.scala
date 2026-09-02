package okay.agent

import okay.{!, +, Async, Handler}
import okay.given
import okay.codec.Json
import okay.agent.Durable.OnRepeat
import okay.persist.{Ack, MemoryStore, Topic}
import scala.collection.mutable

/**
 * `Durable.Journal` over a keyed topic (specs/persist.md, stage 1):
 * the same crash-window scenarios TestDurable proves for the memory
 * journal, now against the disk-shaped one — plus what only a topic
 * can show: intent and completion as SEPARATE records, many runs in
 * one topic, and recovery as a refold of the partition.
 */
class TestTopicJournal extends munit.FunSuite {

  def run[A](prog: A ! Agent)(model: Handler[Model], tool: Handler[Tool],
                              ctx: Handler[Context]): A =
    given Handler[Model] = model
    given Handler[Tool] = tool
    given Handler[Context] = ctx
    given rowMA: Handler[Model + Async] = okay.Handler.union[Model, Async]
    given rowCMA: Handler[Context + (Model + Async)] =
      okay.Handler.union[Context, Model + Async]
    given rowAll: Handler[Agent] = okay.Handler.union[Tool, Context + (Model + Async)]
    prog.runWith

  val charge = ToolCall("c1", "charge",
    Json.JObj(Vector("amount" -> Json.JNum(100))))

  def payments(log: mutable.Buffer[ToolCall]): Handler[Tool] =
    Handlers.recording(Handlers.tools(Map(
      "charge" -> (_ => "receipt-1"),
      "read" -> (_ => "data"))))(log)

  def script = Handlers.scripted(Seq(
    Reply("paying", Seq(charge)), Reply("paid", Nil)))

  test("a clean run writes intent and completion as separate records") {
    val topic = MemoryStore().topic("journal", partitions = 4)
    val j = TopicJournal(topic, "run-1")
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx) = Handlers.context(Compact.all)
    val answer = run(Agent.converse("pay"))(
      script, Durable.tools(payments(touched), j)(), ctx)

    assertEquals(answer, "paid")
    assertEquals(touched.length, 1)
    assertEquals(j.all.length, 1)
    assertEquals(j.all.head.answer, Some("receipt-1"))

    // the raw partition holds TWO records for the one step — the
    // intent physically precedes the answer, which is the point
    val p = Topic.route("run-1".getBytes("UTF-8"), 4)
    topic.read(p, 0L, 10) match
      case Topic.Read.Records(rs) => assertEquals(rs.length, 2)
      case other => fail(s"unexpected $other")
  }

  test("recovery is a refold: a complete journal does not touch the world again") {
    val topic = MemoryStore().topic("journal", partitions = 1)
    val first = mutable.Buffer[ToolCall]()
    val (_, ctx1) = Handlers.context(Compact.all)
    val _ = run(Agent.converse("pay"))(
      script, Durable.tools(payments(first), TopicJournal(topic, "run-1"))(), ctx1): Unit
    assertEquals(first.length, 1)

    // the process died and came back: a FRESH journal instance over
    // the same topic and run refolds the records
    val second = mutable.Buffer[ToolCall]()
    val (_, ctx2) = Handlers.context(Compact.all)
    val answer = run(Agent.converse("pay"))(
      script, Durable.tools(payments(second), TopicJournal(topic, "run-1"))(), ctx2)
    assertEquals(answer, "paid")
    assertEquals(second.length, 0, "the payment was made a second time")
  }

  test("the crash window survives the disk shape: intent, no completion, policy decides") {
    val topic = MemoryStore().topic("journal", partitions = 1)
    // the crash: intent appended, the process died before complete
    TopicJournal(topic, "run-1").append(Durable.Entry(0, "charge",
      s"charge(${Json.print(charge.args)})", "charge-0-key", None))

    // Fail refuses rather than pay twice
    val refused = mutable.Buffer[ToolCall]()
    val (_, ctx1) = Handlers.context(Compact.all)
    val _ = intercept[Durable.Unresolved] {
      val _ = run(Agent.converse("pay"))(script,
        Durable.tools(payments(refused), TopicJournal(topic, "run-1"))(_ => OnRepeat.Fail), ctx1)
    }
    assertEquals(refused.length, 0, "it charged despite the Fail policy")

    // WithKey retries carrying the FIRST attempt's key
    val retried = mutable.Buffer[ToolCall]()
    val (_, ctx2) = Handlers.context(Compact.all)
    val answer = run(Agent.converse("pay"))(script,
      Durable.tools(payments(retried), TopicJournal(topic, "run-1"))(_ => OnRepeat.WithKey), ctx2)
    assertEquals(answer, "paid")
    assertEquals(retried.length, 1)
    val sent = retried.head.args match
      case Json.JObj(fs) => fs.toMap.get(Durable.KeyField)
      case _ => None
    assertEquals(sent, Some(Json.JStr("charge-0-key")))
  }

  test("many runs share one topic without seeing each other") {
    val topic = MemoryStore().topic("journal", partitions = 4)
    val a = TopicJournal(topic, "run-a")
    a.append(Durable.Entry(0, "charge", "charge({})", "charge-0", None))
    a.complete(0, "receipt-a")

    assertEquals(TopicJournal(topic, "run-b").all, Vector.empty)
    val again = TopicJournal(topic, "run-a").all
    assertEquals(again.length, 1)
    assertEquals(again.head.answer, Some("receipt-a"))
  }

  test("a record that does not decode ends the fold at the damage") {
    val topic = MemoryStore().topic("journal", partitions = 1)
    val j = TopicJournal(topic, "run-1")
    j.append(Durable.Entry(0, "charge", "charge({})", "charge-0", None))
    // garbage lands in the partition (a foreign writer, a torn record)
    topic.append(0, "run-1".getBytes("UTF-8"), Array[Byte](1, 2), Ack.Durable): Unit
    j.complete(0, "receipt-1")

    // the completion sits BEYOND the damage: everything before the
    // damage serves, nothing after it is guessed at — the entry
    // surfaces as the crash window, which is the honest reading
    val entries = TopicJournal(topic, "run-1").all
    assertEquals(entries.length, 1)
    assertEquals(entries.head.answer, None)
  }
}
