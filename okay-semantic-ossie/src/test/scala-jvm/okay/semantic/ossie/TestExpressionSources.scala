package okay.semantic.ossie

import okay.*
import okay.given
import okay.semantic.Request

class TestExpressionSources extends okay.testkit.Munit.Diagnosed:
  import Samples.*
  val doc = Document.fromJson(replace(raw,"metrics",arr(metric("median","MEDIAN(amount)")))).toOption.get
  val model = Execution.bind(doc,"orders",bindings.copy(units = Map("median" -> "EUR"))).toOption.get
  val plan = model.plan(Request(Vector("median"))).toOption.get
  test("Source and chunks use the same expression evaluator and enforce the input budget") {
    note(plan.explain)
    assertEquals(plan.source(Source.of(rows.toList)).runWith,plan.run(rows))
    val chunks = rows.grouped(2).map(xs => scala.collection.immutable.ArraySeq.from(xs)).toList
    assertEquals(plan.chunks(Source.of(chunks)).runWith,plan.run(rows))
    val limited = model.plan(Request(Vector("median")),maxRows = 1).toOption.get
    assert(limited.source(Source.of(rows.toList)).runWith.isLeft)
    assert(limited.chunks(Source.of(chunks)).runWith.isLeft)
  }
  test("transport failure propagates without a fabricated semantic result") {
    val source: Source[Sale] = Source(rows.head).flatMap(_ => effect[Writer % Sale + Async,Unit](Async.Run(() => throw IllegalStateException("source failed"))))
    assertEquals(intercept[IllegalStateException](plan.source(source).runWith).getMessage,"source failed")
  }
