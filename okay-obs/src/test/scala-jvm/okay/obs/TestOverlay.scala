package okay.obs

import okay.codec.{Cbor, Json}
import okay.persist.{MemoryStore, Policy, Topic}
import okay.agent.{Durable, Tool, ToolCall, OpTrace}

/**
 * The overlay join with a REAL Tracer (obs-durable-overlay, specs/
 * obs.md "The Durable resonance"): a journaled operation's span and
 * its journal entry carry the same operation identity, so an incident
 * replayed offline lays its spans over the originals. This is the test
 * okay.obs.md's open box waited on — it needs both a Tracer and a
 * Durable journal, so okay-agent joins okay-obs's TEST scope (okay-obs
 * is a leaf; no cycle). The bridge from Tracer to the neutral OpTrace
 * seam is the one-liner below.
 */
class TestOverlay extends munit.FunSuite:

  /** the whole coupling: a Tracer IS an OpTrace, mapping tuples to Attr */
  private def opTrace(t: Tracer): OpTrace = new OpTrace:
    def span[A](name: String, attrs: (String, String)*)(body: => A): A =
      t.span(name, attrs.map((k, v) => Attr(k, v))*)(body)

  private def traceTopic(): Topic = MemoryStore().topic("__trace", 1, Policy())
  private def spansOf(topic: Topic): Vector[Span] =
    topic.read(0, 0, 1000) match
      case Topic.Read.Records(rs) => rs.flatMap(r => Cbor.read[Span](r.value).toOption)
      case _ => Vector.empty

  private val charge = ToolCall("c1", "charge",
    Json.JObj(Vector("amount" -> Json.JNum(100))))

  /** a tool that touches the "world" once and answers */
  private def payTool: okay.Handler[Tool] = new okay.Handler[Tool]:
    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(_) => "receipt-1"

  private def chargeSpan(spans: Vector[Span]): Span =
    spans.find(_.attrs.contains(Attr("durable.op", "charge")))
      .getOrElse(fail(s"no charge span among ${spans.map(_.name)}"))

  test("a journaled operation's span carries the entry key, under the live trace") {
    val topic = traceTopic()
    val tracer = Tracer(topic)
    val j = Durable.MemoryJournal()
    val h = Durable.tools(payTool, j)(trace = Some(opTrace(tracer)))

    val answer = tracer.root("incident") { h.handle(Tool.Call(charge)) }
    assertEquals(answer, "receipt-1")

    val entry = j.all.head
    val span = chargeSpan(spansOf(topic))
    assertEquals(span.attrs.find(_.key == "durable.key").map(_.value), Some(entry.key)) // the join
    assert(span.parentId.isDefined, "the operation span sits under the incident root")
    assert(!span.attrs.exists(_.key == "durable.replay"), "a first run is not a replay")
  }

  test("a replay lays over the original: same key, same trace shape, marked as re-run") {
    // first run records the journal
    val j = Durable.MemoryJournal()
    val liveTopic = traceTopic()
    val liveTracer = Tracer(liveTopic)
    liveTracer.root("incident") {
      Durable.tools(payTool, j)(trace = Some(opTrace(liveTracer))).handle(Tool.Call(charge))
    }
    val liveKey = chargeSpan(spansOf(liveTopic)).attrs.find(_.key == "durable.key").map(_.value)

    // replay offline, no tools touched, under a fresh trace
    val replayTopic = traceTopic()
    val replayTracer = Tracer(replayTopic)
    val answer = replayTracer.root("incident-replay") {
      Durable.replaying(j, Some(opTrace(replayTracer))).handle(Tool.Call(charge))
    }
    assertEquals(answer, "receipt-1")

    val replaySpan = chargeSpan(spansOf(replayTopic))
    assertEquals(replaySpan.attrs.find(_.key == "durable.key").map(_.value), liveKey) // overlays
    assertEquals(replaySpan.attrs.find(_.key == "durable.replay").map(_.value), Some("true"))
  }
