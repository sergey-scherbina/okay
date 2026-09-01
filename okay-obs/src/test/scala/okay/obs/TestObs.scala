package okay.obs

import okay.codec.{Cbor, Json, Schema}
import okay.persist.{MemoryStore, Policy, Topic}

/** the pure claims: total parse, propagation, the wrapping
 * combinator, sampling as a write decision — shared, all platforms */
class TestObs extends munit.FunSuite {

  def tracer(sample: Sample = Sample.Always): (Tracer, () => Vector[Span]) =
    val topic = MemoryStore().topic("__trace", 1, Policy())
    val t = Tracer(topic, sample)
    def spans(): Vector[Span] =
      topic.read(0, 0, 1000) match
        case Topic.Read.Records(rs) => rs.flatMap(r => Cbor.read[Span](r.value).toOption)
        case _ => Vector.empty
    (t, () => spans())

  test("traceparent parses totally: the valid shape and nothing else") {
    val p = Trace.parse("00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01").get
    assertEquals(p.traceId, "4bf92f3577b34da6a3ce929d0e0e4736")
    assertEquals(p.spanId, "00f067aa0ba902b7")
    assert(p.sampled)
    for bad <- Seq("", "garbage", "00-short-00f067aa0ba902b7-01",
      "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7", // three parts
      "00-" + "0" * 32 + "-00f067aa0ba902b7-01",              // all-zero trace
      "00-4BF92F3577B34DA6A3CE929D0E0E4736-00f067aa0ba902b7-01", // uppercase is not W3C
      "ff-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01") // version ff reserved
    do assertEquals(Trace.parse(bad), None, bad)
  }

  test("an inbound parent continues the trace; damage starts a NAMED fresh root") {
    val (t, spans) = tracer()
    t.root("in", Some("00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")) {()}
    t.root("damaged", Some("not-a-traceparent")) {()}
    val Vector(cont, fresh) = spans(): @unchecked
    assertEquals(cont.traceId, "4bf92f3577b34da6a3ce929d0e0e4736")
    assertEquals(cont.parentId, Some("00f067aa0ba902b7"))
    assertNotEquals(fresh.traceId, cont.traceId)
    assertEquals(fresh.attrs, Vector(Attr("traceparent.damaged", "not-a-traceparent")))
  }

  test("children share the traceId with correct parentage; outbound renders the current") {
    val (t, spans) = tracer()
    var out: Option[String] = None
    t.root("req") {
      t.span("step") { out = t.outbound }
    }
    val Vector(step, req) = spans(): @unchecked   // children close first
    assertEquals(step.traceId, req.traceId)
    assertEquals(step.parentId, Some(req.spanId))
    assertEquals(req.parentId, None)
    assertEquals(out, Some(Trace.render(req.traceId, step.spanId)))
  }

  test("the tracing handler wraps ANY handler without its knowledge") {
    enum Box[A] { case Get(k: String) extends Box[String] }
    val plain = new okay.Handler[Box]:
      def handle[A](e: Box[A]): A = e match
        case Box.Get(k) => s"value-of-$k"
    val (t, spans) = tracer()
    val wrapped = t.traced[Box](plain, [X] => (e: Box[X]) => e match
      case Box.Get(k) => s"box get $k")
    val got = t.root("req") { wrapped.handle(Box.Get("alpha")) }
    assertEquals(got, "value-of-alpha")
    assertEquals(spans().map(_.name), Vector("box get alpha", "req"))
    assertEquals(spans()(0).parentId, Some(spans()(1).spanId))
  }

  test("a throw closes the span with an error status and rethrows") {
    val (t, spans) = tracer()
    intercept[RuntimeException](t.root("boom") { throw RuntimeException("nope") })
    assertEquals(spans().map(_.status), Vector("error: nope"))
  }

  test("sampling is a write decision: Never writes nothing, RootOnly writes roots") {
    val (never, ns) = tracer(Sample.Never)
    never.root("r") { never.span("c") {()} }
    assertEquals(ns(), Vector.empty)
    val (roots, rs) = tracer(Sample.RootOnly)
    roots.root("r") { roots.span("c") {()} }
    assertEquals(rs().map(_.name), Vector("r"))
  }

  test("tracestate passes through opaquely, outbound as it arrived") {
    val (t, _) = tracer()
    var st: Option[String] = None
    t.root("in", Some("00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
      Some("vendor=opaque,other=x")) { st = t.outboundState }
    assertEquals(st, Some("vendor=opaque,other=x"))
  }

  test("Never costs near-nothing: ten thousand unwritten spans under a coarse bound") {
    val (t, spans) = tracer(Sample.Never)
    val t0 = System.nanoTime
    var i = 0
    while i < 10000 do { t.span("s") { i += 1 } }
    val ms = (System.nanoTime - t0) / 1000000
    assertEquals(spans(), Vector.empty)
    assert(ms < 500, s"10k Never spans took ${ms}ms")   // generous: the claim is 'no I/O', not a benchmark
  }

  test("spans round-trip both wires: JSON to look at, CBOR on the topic") {
    val s = Span("t" * 32, "s" * 16, Some("p" * 16), "op", 1, 2,
      Vector(Attr("k", "v")), "ok")
    assertEquals(Json.read[Span](Json.write(s)), Right(s))
    assertEquals(Cbor.read[Span](Cbor.write(s)), Right(s))
  }
}
