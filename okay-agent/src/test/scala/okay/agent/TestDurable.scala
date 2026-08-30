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
    given rowCA: Handler[Context + Async] = okay.Handler.union[Context, Async]
    given rowTCA: Handler[Tool + (Context + Async)] =
      okay.Handler.union[Tool, Context + Async]
    given rowAll: Handler[Agent] = okay.Handler.union[Model, Tool + (Context + Async)]
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
    run(Agent.converse("pay"))(script, Durable.tools(payments(first), j)(), ctx1)
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
    intercept[Durable.Unresolved] {
      run(Agent.converse("pay"))(script,
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
    run(Agent.converse("pay"))(script,
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
    intercept[Durable.Unresolved] {
      run(Agent.converse("pay"))(script,
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
      run(Agent.converse("pay"))(script,
        Durable.tools(payments(mutable.Buffer()), j)(), ctx)
    }
    assert(e.expected.contains("50") && e.got.contains("100"), e.getMessage)
  }

  test("deterministic replay: the incident again, offline, world untouched") {
    val j = Durable.MemoryJournal()
    val touched = mutable.Buffer[ToolCall]()
    val (_, ctx1) = Handlers.context(Compact.all)
    run(Agent.converse("pay"))(script, Durable.tools(payments(touched), j)(), ctx1)

    // replay needs no tools at all — and no model either, if the
    // model's answers are journalled the same way (here the script
    // stands in for that)
    val (_, ctx2) = Handlers.context(Compact.all)
    val again = run(Agent.converse("pay"))(script, Durable.replaying(j), ctx2)
    assertEquals(again, "paid")
    assertEquals(touched.length, 1, "replay touched the world")
  }
}
