package okay.agent

import okay.{!, +, Async, Handler}
import okay.given
import okay.codec.Json
import okay.agent.Durable.OnRepeat
import scala.collection.mutable

/**
 * The tests that decide whether a durability feature can be trusted:
 * not the happy path, but the crash window and the code drift. A
 * feature that silently charges a card twice is worse than none.
 */
class TestDurable extends munit.FunSuite {

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

  /** a payment tool that counts how many times the world was touched */
  def payments(log: mutable.Buffer[ToolCall]): Handler[Tool] =
    Handlers.recording(Handlers.tools(Map(
      "charge" -> (_ => "receipt-1"),
      "read" -> (_ => "data"))))(log)

  def script = Handlers.scripted(Seq(
    Reply("paying", Seq(charge)), Reply("paid", Nil)))

  test("a clean run journals every call, intent first then answer") {
    val j = Durable.MemoryJournal()
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx) = Handlers.context(Compact.all)
    val answer = run(Agent.converse("pay"))(
      script, Durable.tools(payments(touched), j)(), ctx)

    assertEquals(answer, "paid")
    assertEquals(touched.length, 1)
    assertEquals(j.all.length, 1)
    assertEquals(j.all.head.answer, Some("receipt-1"))
    assert(j.all.head.key.startsWith("charge-"), j.all.head.key)
  }

  test("recovery from a COMPLETE journal does not touch the world again") {
    val j = Durable.MemoryJournal()
    val first = mutable.Buffer[ToolCall]()
    val (_, ctx1) = Handlers.context(Compact.all)
    val _ = run(Agent.converse("pay"))(script, Durable.tools(payments(first), j)(), ctx1): Unit
    assertEquals(first.length, 1)

    // the process died and came back; the same program, the same journal
    val second = mutable.Buffer[ToolCall]()
    val (_, ctx2) = Handlers.context(Compact.all)
    val answer = run(Agent.converse("pay"))(
      script, Durable.tools(payments(second), j)(), ctx2)

    assertEquals(answer, "paid")
    assertEquals(second.length, 0, "the payment was made a second time")
  }

  /** the crash window: an intent with no answer */
  def crashed: Durable.MemoryJournal =
    val j = Durable.MemoryJournal()
    j.append(Durable.Entry(0, "charge",
      s"charge(${Json.print(charge.args)})", "charge-0-key", None))
    j

  test("crash window, Fail: the workflow refuses rather than pay twice") {
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx) = Handlers.context(Compact.all)
    val _ = intercept[Durable.Unresolved] {
      val _ = run(Agent.converse("pay"))(script,
        Durable.tools(payments(touched), crashed)(_ => OnRepeat.Fail), ctx)
    }
    assertEquals(touched.length, 0, "it charged despite the Fail policy")
  }

  test("crash window, Redo: a safe call simply runs again") {
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx) = Handlers.context(Compact.all)
    val answer = run(Agent.converse("pay"))(script,
      Durable.tools(payments(touched), crashed)(_ => OnRepeat.Redo), ctx)
    assertEquals(answer, "paid")
    assertEquals(touched.length, 1)
  }

  test("crash window, WithKey: the retry carries the FIRST attempt's key") {
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx) = Handlers.context(Compact.all)
    val _ = run(Agent.converse("pay"))(script,
      Durable.tools(payments(touched), crashed)(_ => OnRepeat.WithKey), ctx)

    assertEquals(touched.length, 1)
    val sent = touched.head.args match
      case Json.JObj(fs) => fs.toMap.get(Durable.KeyField)
      case _ => None
    assertEquals(sent, Some(Json.JStr("charge-0-key")),
      "the far end cannot deduplicate without the original key")
  }

  test("crash window, Reconcile: ask the far end, never repeat the call") {
    val touched = mutable.Buffer[ToolCall]()
    val asked = mutable.Buffer[String]()
    val (_, ctx) = Handlers.context(Compact.all)
    val j = crashed
    val answer = run(Agent.converse("pay"))(script,
      Durable.tools(payments(touched), j)(
        policy = _ => OnRepeat.Reconcile,
        reconcile = (_, key) => { asked += key; Some("receipt-1") }), ctx)

    assertEquals(answer, "paid")
    assertEquals(touched.length, 0, "reconcile must not re-execute")
    assertEquals(asked.toList, List("charge-0-key"))
    assertEquals(j.all.head.answer, Some("receipt-1"))   // the journal is settled
  }

  test("crash window, Reconcile that cannot answer: refuse, do not guess") {
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx) = Handlers.context(Compact.all)
    val _ = intercept[Durable.Unresolved] {
      val _ = run(Agent.converse("pay"))(script,
        Durable.tools(payments(touched), crashed)(
          policy = _ => OnRepeat.Reconcile,
          reconcile = (_, _) => None), ctx)
    }
    assertEquals(touched.length, 0)
  }

  test("policy is per operation: read redoes while charge refuses") {
    val call2 = ToolCall("c2", "read", Json.JObj(Vector.empty))
    val twoCalls = Handlers.scripted(Seq(
      Reply("working", Seq(call2)), Reply("done", Nil)))
    val j = Durable.MemoryJournal()
    j.append(Durable.Entry(0, "read", s"read(${Json.print(call2.args)})", "read-0", None))

    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx) = Handlers.context(Compact.all)
    val policy: String => OnRepeat = {
      case "charge" => OnRepeat.Fail
      case _ => OnRepeat.Redo
    }
    val answer = run(Agent.converse("go"))(twoCalls,
      Durable.tools(payments(touched), j)(policy), ctx)
    assertEquals(answer, "done")
    assertEquals(touched.map(_.name).toList, List("read"))
  }

  test("code drift stops the replay LOUDLY instead of answering wrongly") {
    val j = Durable.MemoryJournal()
    j.append(Durable.Entry(0, "charge",
      "charge({\"amount\":50})", "charge-0", Some("receipt-old")))
    // the program now charges 100, not 50 — the journal is about
    // another program and must not be trusted
    val (_, ctx) = Handlers.context(Compact.all)
    val e = intercept[Durable.Drift] {
      val _ = run(Agent.converse("pay"))(script,
        Durable.tools(payments(mutable.Buffer()), j)(), ctx)
    }
    assert(e.expected.contains("50") && e.got.contains("100"), e.getMessage)
  }

  test("deterministic replay: the incident again, offline, world untouched") {
    val j = Durable.MemoryJournal()
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx1) = Handlers.context(Compact.all)
    val _ = run(Agent.converse("pay"))(script, Durable.tools(payments(touched), j)(), ctx1): Unit

    // replay needs no tools at all — and no model either, if the
    // model's answers are journalled the same way (here the script
    // stands in for that)
    val (_, ctx2) = Handlers.context(Compact.all)
    val again = run(Agent.converse("pay"))(script, Durable.replaying(j), ctx2)
    assertEquals(again, "paid")
    assertEquals(touched.length, 1, "replay touched the world")
  }

  /** a fake span sink that records what identity each operation
   * carried — the obs seam without okay-obs (that join is proven with
   * the real Tracer in okay-obs's TestOverlay) */
  final class FakeTrace extends OpTrace:
    val spans = mutable.Buffer[(String, Map[String, String])]()
    def span[A](name: String, attrs: (String, String)*)(body: => A): A =
      spans += (name -> attrs.toMap)
      body

  test("the overlay identity: a journaled operation's span carries the entry key") {
    val j = Durable.MemoryJournal()
    val trace = FakeTrace()
    val (_, ctx) = Handlers.context(Compact.all)
    val _ = run(Agent.converse("pay"))(
      script, Durable.tools(payments(mutable.Buffer()), j)(trace = Some(trace)), ctx): Unit

    val entry = j.all.head
    val span = trace.spans.find(_._2.get("durable.op").contains("charge"))
      .getOrElse(fail("no span for the charge operation"))
    assertEquals(span._2("durable.key"), entry.key)   // the join
    assertEquals(span._2("durable.seq"), entry.seq.toString)
    assert(!span._2.contains("durable.replay"), "a first run is not a replay")
  }

  test("the overlay holds across replay: same key, marked as the re-run") {
    val j = Durable.MemoryJournal()
    val (_, ctx1) = Handlers.context(Compact.all)
    val _ = run(Agent.converse("pay"))(
      script, Durable.tools(payments(mutable.Buffer()), j)(), ctx1): Unit
    val entryKey = j.all.head.key

    val replayTrace = FakeTrace()
    val (_, ctx2) = Handlers.context(Compact.all)
    val _ = run(Agent.converse("pay"))(
      script, Durable.replaying(j, Some(replayTrace)), ctx2): Unit

    val span = replayTrace.spans.find(_._2.get("durable.op").contains("charge"))
      .getOrElse(fail("no replay span for the charge operation"))
    assertEquals(span._2("durable.key"), entryKey)          // overlays the original
    assertEquals(span._2.get("durable.replay"), Some("true"))
  }

  // ---- waiting on a person (durable-waiting-on-a-person) -------------
  //
  // An answerless entry had one meaning: the crash window. This is the
  // other one — a question asked and not yet answered, which is not a
  // failure and whose right response is to wait.

  val ask = ToolCall("q1", "ask",
    Json.JObj(Vector("question" -> Json.JStr("what is your budget?"))))

  def asking = Handlers.scripted(Seq(
    Reply("asking", Seq(ask)), Reply("done", Nil)))

  /** a table that would answer the question if it were ever reached —
   * which is the point: it must not be */
  def answering(log: mutable.Buffer[ToolCall]): Handler[Tool] =
    Handlers.recording(Handlers.tools(Map(
      "ask" -> (_ => "THE INNER HANDLER ANSWERED, WHICH IT MUST NOT"),
      "charge" -> (_ => "receipt-1"))))(log)

  def waits(name: String): OnRepeat =
    if name == "ask" then OnRepeat.Await else OnRepeat.Redo

  test("a question parks the program instead of answering it") {
    val j = Durable.MemoryJournal()
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx) = Handlers.context(Compact.all)

    val parked = intercept[Durable.Awaiting] {
      run(Agent.converse("ask me"))(asking, Durable.tools(answering(touched), j)(waits), ctx)
    }
    assertEquals(parked.op, "ask")
    assertEquals(parked.seq, 0)
    // asking a person touches no world
    assertEquals(touched.toVector, Vector.empty)
    // and the question itself is in the log, so a restart can find it
    assertEquals(j.all.map(e => (e.op, e.answer)), Vector(("ask", None)))
    assertEquals(Durable.awaiting(j).map(_.op), Some("ask"))
  }

  test("the answer arrives later, and re-running resumes where it stopped") {
    val j = Durable.MemoryJournal()
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx1) = Handlers.context(Compact.all)
    intercept[Durable.Awaiting] {
      run(Agent.converse("ask me"))(asking, Durable.tools(answering(touched), j)(waits), ctx1)
    }: Unit

    // a person answers, possibly days later and in another process
    j.complete(0, "3000")

    val (_, ctx2) = Handlers.context(Compact.all)
    val out = run(Agent.converse("ask me"))(
      asking, Durable.tools(answering(touched), j)(waits), ctx2)
    assert(out.nonEmpty, "the program did not run to completion")
    assertEquals(Durable.awaiting(j), None, "still parked after being answered")
    // the recorded answer was used; the inner handler was never asked
    assertEquals(touched.toVector, Vector.empty)
    assertEquals(j.all.map(e => (e.op, e.answer)), Vector(("ask", Some("3000"))))
  }

  test("a second question parks at the second one, not the first") {
    // a FRESH script per run: `Handlers.scripted` carries its own
    // position, and reusing one makes the second run start at the
    // second reply — which `Drift` catches, correctly, as the program
    // asking something other than what the journal recorded
    def twice = Handlers.scripted(Seq(
      Reply("first", Seq(ask)),
      Reply("second", Seq(ToolCall("q2", "ask",
        Json.JObj(Vector("question" -> Json.JStr("and for how long?")))))),
      Reply("done", Nil)))
    val j = Durable.MemoryJournal()
    val (_, ctx1) = Handlers.context(Compact.all)
    intercept[Durable.Awaiting] {
      run(Agent.converse("ask me"))(twice, Durable.tools(answering(mutable.Buffer()), j)(waits), ctx1)
    }: Unit
    j.complete(0, "3000")

    val (_, ctx2) = Handlers.context(Compact.all)
    val second = intercept[Durable.Awaiting] {
      run(Agent.converse("ask me"))(twice, Durable.tools(answering(mutable.Buffer()), j)(waits), ctx2)
    }
    assertEquals(second.seq, 1, "resumed at the wrong question")
    assertEquals(Durable.awaiting(j).map(_.seq), Some(1))
    // the first answer was not asked for again
    assertEquals(j.all.map(_.answer), Vector(Some("3000"), None))
  }

  // the program's own order decides what runs next, not the order
  // answers happened to arrive in
  test("an answer to a later question does not skip an earlier one") {
    val j = Durable.MemoryJournal()
    j.append(Durable.Entry(0, "ask", "ask({})", "k0", None))
    j.append(Durable.Entry(1, "ask", "ask({})", "k1", Some("later")))
    assertEquals(Durable.awaiting(j).map(_.seq), Some(0))
  }

  // Await is the ONLY case that is read before an entry exists: the
  // others answer a recovery question, and there is nothing to recover
  test("an awaiting operation is recognised on its first encounter") {
    val j = Durable.MemoryJournal()
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx) = Handlers.context(Compact.all)
    intercept[Durable.Awaiting] {
      run(Agent.converse("ask me"))(asking, Durable.tools(answering(touched), j)(waits), ctx)
    }: Unit
    // journalled by parking, not by executing
    assertEquals(j.all.length, 1)
    assertEquals(touched.length, 0)
  }
}
