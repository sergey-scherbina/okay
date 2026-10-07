package okay.semantic.arrow



import okay.{Bulk}
import okay.codec.Schema
import okay.arrow.{Rows, ArrowCodec}
import okay.parquet.{ParquetCodec, ParquetFormat, ReadAt}
import okay.semantic.*
import okay.semantic.data.Data

class TestSemanticArrow extends okay.testkit.Munit.Diagnosed:
  case class Event(segment: String, cents: Long) derives Schema
  val rows = Vector(Event("A", 100), Event("B", 200), Event("A", 300))
  val model = Model.build[Event]("events", Origin("file", "1"), "one event",
    Vector(Dimension("segment", "Segment", Kind.Text, e => Value.Text(e.segment))),
    Vector(Measure("amount", "EUR cents", e => Some(BigDecimal(e.cents)))),
    Vector(Metric("sum", "Sum", "cent", Calculation.Sum("amount")), Metric("avg", "Average", "cent", Calculation.Average("amount")))).toOption.get
  val plan = model.plan(Request(Vector("sum", "avg"), Vector("segment"))).toOption.get
  test("Arrow columns and IPC yield the same semantic result") {
    val table = Rows.table(rows)
    note(plan.explain)
    assertEquals(ArrowData.table(plan, table), plan.run(rows))
    assertEquals(ArrowData.ipc(plan, summon[ArrowCodec].write(table)), plan.run(rows))
    assert(ArrowData.ipc(plan, Array[Byte](1, 2, 3)).isLeft)
  }
  test("Parquet row groups run through the ordinary file/Bulk interpreter") {
    val bytes = summon[ParquetCodec].write(Rows.table(rows), groupRows = 1)
    val format = ParquetFormat[Event](_ => ReadAt.of(bytes))
    val backend = Bulk.local(_ => Iterator.empty)
    note(s"Parquet groups ${format.splits("events.parquet").size}")
    assertEquals(Data.file(plan, "events.parquet", format)(using backend), plan.run(rows))
  }

  test("expression median executes over Arrow columns and IPC through typed rows") {
    import okay.semantic.ossie.{Bindings, Document, Execution, FieldKey}
    val document = Document.readJson("""{"version":"0.2.0.dev0","name":"events","datasets":[{"name":"events","source":"ignored","fields":[{"name":"segment","datatype":"String","expression":{"dialects":[{"dialect":"ANSI_SQL","expression":"segment"}]}},{"name":"amount","datatype":"Decimal","expression":{"dialects":[{"dialect":"ANSI_SQL","expression":"amount"}]}}]}],"metrics":[{"name":"median","expression":{"dialects":[{"dialect":"ANSI_SQL","expression":"MEDIAN(amount)"}]}}]}""").toOption.get
    val bindings = Bindings(model.origin,model.grain,Map("median" -> "cent"),
      model.dimensions.map(d => FieldKey("events",d.id) -> d).toMap,
      model.measures.map(m => FieldKey("events",m.id) -> m).toMap)
    val expression = Execution.bind(document,"events",bindings).toOption.get.plan(Request(Vector("median"),Vector("segment"))).toOption.get
    note(expression.explain)
    val table = Rows.table(rows)
    assertEquals(ArrowData.table(expression,table),expression.run(rows))
    assertEquals(ArrowData.ipc(expression,summon[ArrowCodec].write(table)),expression.run(rows))
    assert(ArrowData.ipc(expression,Array[Byte](1,2,3)).isLeft)
    val limited = Execution.bind(document,"events",bindings).toOption.get.plan(Request(Vector("median")),maxRows = 1).toOption.get
    assert(ArrowData.table(limited,table).isLeft)
  }
