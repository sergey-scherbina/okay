package okay.obs

import okay.*
import okay.given
import okay.codec.{Cbor, Json}
import okay.persist.{MemoryStore, Topic}

/**
 * Log lines as values, correlated by the handler (specs/obs.md, "The
 * third leg"). Nothing here needs a clock, a socket or a thread: the
 * program is a Writer program and the handler is comonadic, so the
 * suite runs on every platform.
 */
class TestLog extends munit.FunSuite:
  import Log.*

  var now = 1_000L
  val clock: () => Long = () => now

  /** run a logging program under a handler, answering its value */
  def run[A](prog: A ! Says)(using Handler[Says]): A = prog.runWith

  test("the program says a level, a message and fields — and nothing else") {
    val (h, lines) = collecting(clock = clock)
    given Handler[Says] = h
    val answer = run(
      info("order placed", "id" -> "o1", "total" -> "10")
        .flatMap(_ => debug("cache warm"))
        .flatMap(_ => warn("slow upstream", "took" -> "900ms"))
        .map(_ => 42))
    assertEquals(answer, 42)
    assertEquals(lines().map(l => (l.level, l.message)), Vector(
      (Level.Info, "order placed"), (Level.Debug, "cache warm"), (Level.Warn, "slow upstream")))
    assertEquals(lines().head.fields, Vector(Attr("id", "o1"), Attr("total", "10")))
    assertEquals(lines().head.at, 1000L)          // the handler's clock, not the program's
    assert(lines().forall(_.traceId.isEmpty))     // no tracer in scope: no ids, no guess
  }

  test("a line is written WHEN IT IS TOLD, not when the program ends") {
    val (h, lines) = collecting(clock = clock)
    given Handler[Says] = h
    val boom = intercept[RuntimeException](
      run(info("before the fall").flatMap(_ => okay.pure[Says, Unit](throw RuntimeException("fell")))))
    assertEquals(boom.getMessage, "fell")
    assertEquals(lines().map(_.message), Vector("before the fall"))
  }

  test("the level filter drops what is below it and keeps the rest") {
    val (h, lines) = collecting(min = Level.Warn, clock = clock)
    given Handler[Says] = h
    run(debug("no").flatMap(_ => info("no")).flatMap(_ => warn("yes")).flatMap(_ => error("yes")))
    assertEquals(lines().map(_.message), Vector("yes", "yes"))
    assert(Level.Error.atLeast(Level.Debug) && !Level.Debug.atLeast(Level.Error))
  }

  test("failure carries the throwable's class and message as fields, and is an error line") {
    val (h, lines) = collecting(clock = clock)
    given Handler[Says] = h
    run(failure("could not ship", IllegalStateException("no stock"), "order" -> "o1"))
    val l = lines().head
    assertEquals(l.level, Level.Error)
    assertEquals(l.fields, Vector(Attr("order", "o1"),
      Attr("error", "java.lang.IllegalStateException"), Attr("error.message", "no stock")))
  }

  // ── the correlation, which is the whole point ────────────────────

  test("inside a span, every line carries THAT span's ids — the program never mentions them") {
    val store = MemoryStore()
    val tracer = Tracer(store.topic("traces"), clock = clock)
    val (h, lines) = collecting(tracer = Some(tracer), clock = clock)
    given Handler[Says] = h

    // the inbound edge, then a child region — a domain program in the
    // middle that knows nothing about either
    tracer.root("POST /orders", traceparent = Some("00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")) {
      run(info("received"))
      tracer.span("charge") { run(info("charged", "amount" -> "10")) }
      run(info("answered"))
    }
    run(info("outside any span"))

    val ids = lines().map(l => (l.message, l.traceId, l.spanId))
    val trace = "4bf92f3577b34da6a3ce929d0e0e4736"
    assertEquals(ids.map(_._2), Vector(Some(trace), Some(trace), Some(trace), None))
    val Vector(received, charged, answered, outside) = lines()
    assertEquals(received.spanId, answered.spanId, "the root's two lines share the root span")
    assert(charged.spanId != received.spanId, "the child region's line carries the CHILD span")
    assert(charged.spanId.isDefined)
    assertEquals(outside.spanId, None)

    // and the span the line points at is really in the trace topic
    val spans = store.topic("traces").read(0, 0, 10) match
      case Topic.Read.Records(rs) => rs.map(r => Cbor.read[Span](r.value).toOption.get)
      case _ => fail("no spans")
    assertEquals(spans.map(_.name).sorted, Vector("POST /orders", "charge"))
    assert(spans.exists(s => Some(s.spanId) == charged.spanId && s.name == "charge"))
    assert(spans.forall(_.traceId == trace))
  }

  test("Sample.Never: no span, and therefore no ids on the lines — the line still gets written") {
    val store = MemoryStore()
    val tracer = Tracer(store.topic("traces"), sample = Sample.Never, clock = clock)
    val (h, lines) = collecting(tracer = Some(tracer), clock = clock)
    given Handler[Says] = h
    tracer.root("GET /health") { run(info("probed")) }
    assertEquals(lines().map(l => (l.message, l.traceId)), Vector(("probed", None)))
  }

  // ── the wires ────────────────────────────────────────────────────

  test("console: one JSON object per line, the ids at the top level, a reserved field name kept but prefixed") {
    var out = Vector.empty[String]
    given Handler[Says] = console(out = s => out :+= s, clock = clock, logger = "orders")
    run(info("placed", "id" -> "o1", "message" -> "shadowing on purpose"))
    val line = out.head
    assertEquals(line,
      """{"level":"info","at":1000,"message":"placed","logger":"orders","id":"o1","field.message":"shadowing on purpose"}""")
    // and it is JSON a collector can actually read back
    Json.parse(line) match
      case Json.JObj(fs) =>
        assertEquals(fs.toMap.get("level"), Some(Json.JStr("info")))
        assertEquals(fs.toMap.get("field.message"), Some(Json.JStr("shadowing on purpose")))
      case other => fail(s"not an object: $other")
  }

  test("console: a message with quotes and newlines stays one line and one object") {
    var out = Vector.empty[String]
    given Handler[Says] = console(out = s => out :+= s, clock = clock)
    run(info("he said \"no\"\nand left"))
    assertEquals(out.size, 1)
    assert(!out.head.drop(1).contains("\n"), out.head)
    Json.parse(out.head) match
      case Json.JObj(fs) => assertEquals(fs.toMap.get("message"), Some(Json.JStr("he said \"no\"\nand left")))
      case other => fail(s"not an object: $other")
  }

  test("topic: the lines are records keyed by traceId, and they read back as Lines") {
    val store = MemoryStore()
    val tracer = Tracer(store.topic("traces"), clock = clock)
    val logs = store.topic("logs")
    given Handler[Says] = topic(logs, tracer = Some(tracer), clock = clock, logger = "orders")
    tracer.root("POST /orders") {
      run(info("received").flatMap(_ => error("failed", "why" -> "no stock")))
    }
    val (keys, read) = logs.read(0, 0, 10) match
      case Topic.Read.Records(rs) => (rs.map(r => new String(r.key, "UTF-8")), rs.map(r => Cbor.read[Line](r.value).toOption.get))
      case _ => fail("no records")
    assertEquals(read.map(l => (l.level, l.message, l.logger)),
      Vector((Level.Info, "received", "orders"), (Level.Error, "failed", "orders")))
    assertEquals(keys.distinct.size, 1, "one request, one key")
    assertEquals(keys.head, read.head.traceId.get)
    assertEquals(read.last.fields, Vector(Attr("why", "no stock")))
  }

  test("the Writer algebra still owns the program: run collects the same lines the handler would write") {
    val told = Writer.run[Line, Int, Pure](
      info("a").flatMap(_ => warn("b")).map(_ => 7))
    val (lines, answer) = !.run(told)
    assertEquals(answer, 7)
    assertEquals(lines.map(_.message), Vector("a", "b"))     // no handler, no stamping: a plain fold
    assert(lines.forall(_.at == 0L))
  }
