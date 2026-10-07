package okay.semantic.ossie


import okay.{Source, Async}
import okay.freer.*

import okay.given
import okay.freer.given
import okay.semantic.Request

final case class ExpressionJob[A](plan: ExpressionPlan[A], rows: Vector[A]):
  def execute: Either[Vector[String],okay.semantic.Result] = plan.run(rows)

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

  test("portable expression plans serialize for distributed Bulk workers") {
    val bytes = new java.io.ByteArrayOutputStream()
    val output = new java.io.ObjectOutputStream(bytes)
    try output.writeObject(ExpressionJob(plan,rows)) finally output.close()
    val loader = getClass.getClassLoader
    val input = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bytes.toByteArray)):
      override def resolveClass(description: java.io.ObjectStreamClass): Class[?] =
        Class.forName(description.getName,false,loader)
    val restored = try input.readObject() finally input.close()
    restored match
      case job: ExpressionJob[?] =>
        note(s"serialized ${bytes.size()} bytes")
        assertEquals(job.plan.explain,plan.explain)
        assertEquals(job.plan.request,plan.request)
        assertEquals(job.execute,plan.run(rows))
      case _ => fail("restored object is not a typed expression job")
  }
