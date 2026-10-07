package okay.semantic.data


import okay.{Source, Async}
import okay.freer.*
import okay.std.*
import okay.given
import okay.freer.given
import okay.std.given
import okay.codec.Json
import okay.semantic.*
import java.time.Instant

class TestSemanticSources extends okay.testkit.Munit.Diagnosed:
  import SemanticSamples.*
  test("Source and chunked asynchronous Source agree without collecting input rows") {
    var pulled = 0
    val rows: Source[Event] = effect[Writer % Event + Async, Unit](Async.Run(() => { pulled += 1 })).flatMap(_ => Source.of(events.toList))
    note(plan.explain)
    assertEquals(Data.source(plan, rows).runWith, plan.run(events))
    assertEquals(pulled, 1)
    val chunks = events.grouped(2).map(xs => scala.collection.immutable.ArraySeq.from(xs)).toVector
    assertEquals(Data.chunks(plan, Source.of(chunks.toList)).runWith, plan.run(events))
    assertEquals(Data.source(plan, Source.of(List.empty[Event])).runWith, plan.run(Vector.empty))
  }
  test("source failure propagates and does not fabricate a partial result") {
    val rows: Source[Event] = Source(events.head).flatMap(_ => effect[Writer % Event + Async, Unit](Async.Run(() => throw IllegalStateException("source failed"))))
    note("The semantic interpreter retains Source's failure/cancellation contract")
    val failure = intercept[IllegalStateException](Data.source(plan, rows).runWith)
    assertEquals(failure.getMessage, "source failed")
  }
  test("API validation and explain precede source access; query uses the same plan") {
    var reads = 0
    val endpoint = Api.local(model, () => { reads += 1; events })
    note(Json.print(endpoint.describe))
    val invalid = Wire.request(Request(Vector("missing")))
    assert(Json.print(endpoint.query(invalid).runWith).contains("errors"))
    assertEquals(reads, 0)
    val request = Wire.request(plan.request)
    assert(Json.print(endpoint.explain(request)).contains("explain"))
    assertEquals(reads, 0)
    assertEquals(endpoint.query(request).runWith, Wire.response(plan.run(events)))
    assertEquals(reads, 1)
  }
  private def stamp(text: String): Long = Instant.parse(text).getEpochSecond * 1000000L
  test("civil calendar month, quarter, leap day, negative epoch and DST") {
    note("Civil calendar uses explicit zones and java.time; weeks start Monday")
    assertEquals(JdkCalendar.start(stamp("2024-02-29T12:00:00Z"), Grain.Month, "UTC"), Right(stamp("2024-02-01T00:00:00Z")))
    assertEquals(JdkCalendar.start(stamp("2026-05-12T12:00:00Z"), Grain.Quarter, "UTC"), Right(stamp("2026-04-01T00:00:00Z")))
    assertEquals(JdkCalendar.start(stamp("2026-03-29T12:00:00Z"), Grain.Day, "Europe/Warsaw"), Right(stamp("2026-03-28T23:00:00Z")))
    assertEquals(JdkCalendar.start(stamp("2026-10-25T00:30:00Z"), Grain.Hour, "Europe/Warsaw"), Right(stamp("2026-10-25T00:00:00Z")))
    assertEquals(JdkCalendar.start(stamp("2026-10-25T01:30:00Z"), Grain.Hour, "Europe/Warsaw"), Right(stamp("2026-10-25T01:00:00Z")))
    assertEquals(JdkCalendar.start(-1L, Grain.Day, "UTC"), Right(stamp("1969-12-31T00:00:00Z")))
    assert(JdkCalendar.start(0L, Grain.Month, "not/a/zone").isLeft)
    val d = JdkCalendar.dimension[Long]("month", "Month", t => Some(t), Grain.Month, "UTC").toOption.get
    assertEquals(d.read(stamp("2026-05-12T12:00:00Z")), Value.Number(BigDecimal(stamp("2026-05-01T00:00:00Z"))))
  }
  test("a distributed aggregate and plan serialize without a vendor API") {
    note("Java serialization exercises the contract Spark's object Bulk uses")
    val buffer = java.io.ByteArrayOutputStream()
    val out = java.io.ObjectOutputStream(buffer)
    out.writeObject(Data.aggregator(plan)); out.close()
    assert(buffer.size > 0)
    val loader = getClass.getClassLoader
    val in = new java.io.ObjectInputStream(java.io.ByteArrayInputStream(buffer.toByteArray)):
      override def resolveClass(description: java.io.ObjectStreamClass): Class[?] =
        Class.forName(description.getName, false, loader)
    try assert(in.readObject().isInstanceOf[Aggregator[?, ?, ?]])
    finally in.close()
  }
