package okay.agent

import okay.Answers
import okay.codec.Json
import okay.persist.MemoryStore

/** The old API and the neutral API share types and one implementation. */
class TestDurableCompatibility extends okay.testkit.Munit.Diagnosed:
  test("legacy journal, trace and key helper interoperate with the neutral handler") {
    val journal: Durable.Journal = new Durable.MemoryJournal
    val neutral: okay.durable.Durable.Journal = journal
    var spans = 0
    val trace: OpTrace = new OpTrace:
      def span[A](name: String, attrs: (String, String)*)(body: => A): A =
        spans += 1
        note(s"$name ${attrs.toVector}")
        body
    val inner = new Answers[Tool]:
      def handle[A](op: Tool[A]): A = op match
        case Tool.Call(_) => "answer"
    val call = ToolCall("call", "read", Json.JNull)
    val op = Tool.Call(call)
    onFailure(journal.all.toString)
    assertEquals(Durable.keyFor(0, call), okay.durable.Durable.keyFor(0, op))
    assertEquals(Durable.keyFor(journal, 0, call), okay.durable.Durable.keyFor(neutral, 0, op))
    assertEquals(Durable.MemoryJournal("explicit-run").runId, Some("explicit-run"))
    assertEquals(Durable.tools(inner, journal)(trace = Some(trace)).handle(op), "answer")
    assertEquals(okay.durable.Durable.replayingOver[Tool](neutral).handle(op), "answer")
    assertEquals(spans, 1)
    val policy: okay.durable.Durable.OnRepeat = Durable.OnRepeat.Redo
    assertEquals(policy, okay.durable.Durable.OnRepeat.Redo)
  }

  test("old constructor forms and Rec companion use the neutral journal") {
    val topic = MemoryStore().topic("compatibility", partitions = 1)
    val constructed: TopicJournal = new TopicJournal(topic, "run")
    val applied: TopicJournal = TopicJournal(topic, "run")
    val neutral: okay.durable.persist.TopicJournal = constructed
    note("new/type alias, apply facade and Rec case names")
    neutral.append(Durable.Entry(0, "read", "read(null)", "old-key", None))
    applied.complete(0, "answer")
    onFailure(constructed.all.toString)
    assertEquals(constructed.all, applied.all)
    assertEquals(constructed.all.head.answer, Some("answer"))
    val rec: TopicJournal.Rec = TopicJournal.Rec.Complete(0, "answer")
    assertEquals(rec, okay.durable.persist.TopicJournal.Rec.Complete(0, "answer"))
  }
