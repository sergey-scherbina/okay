package okay.semantic.ossie

import okay.{Bulk, Tables}
import okay.freer.*


import okay.codec.{Schema, Json}
import okay.semantic.{Dimension, Kind, Measure, Origin, Request, Value}

class TestExpressionData extends okay.testkit.Munit.Diagnosed:
  case class Event(segment: String, amount: Long) derives Schema
  val rows = Vector(Event("A",1),Event("A",3),Event("B",10))
  val document = Document.readJson("""{"version":"0.2.0.dev0","name":"events","datasets":[{"name":"events","source":"ignored","fields":[{"name":"segment","datatype":"String","expression":{"dialects":[{"dialect":"ANSI_SQL","expression":"segment"}]}},{"name":"amount","datatype":"Decimal","expression":{"dialects":[{"dialect":"ANSI_SQL","expression":"amount"}]}}]}],"metrics":[{"name":"median","expression":{"dialects":[{"dialect":"ANSI_SQL","expression":"MEDIAN(amount)"}]}}]}""").toOption.get
  val bindings = Bindings(Origin("events","v1"),"one event",Map("median" -> "EUR"),
    Map(FieldKey("events","segment") -> Dimension[Event]("segment","Segment",Kind.Text,e => Value.Text(e.segment))),
    Map(FieldKey("events","amount") -> Measure[Event]("amount","Amount",e => Some(BigDecimal(e.amount)))))
  val model = Execution.bind(document,"events",bindings).toOption.get
  val plan = model.plan(Request(Vector("median"),Vector("segment"))).toOption.get
  val backend = Bulk.local(_ => Iterator.empty)
  test("partitioned Bulk, Tables and schema-decoded JSON agree with collections") {
    note(plan.explain)
    val expected = plan.run(rows)
    assertEquals(plan.bulk(backend.of(rows))(using backend),expected)
    assertEquals(Tables.run(backend)(Tables.of(rows).flatMap(t => plan.table(t))),expected)
    assertEquals(plan.json(rows.map(e => Json.parse(Json.encode(summon[Schema[Event]])(e)))),expected)
    assert(plan.json(Vector(Json.JStr("bad"))).isLeft)
    val format = new Bulk.Format[Event]:
      def name: String = "events"
      def splits(path: String): Vector[Int] = { note(path); Vector(0,1) }
      def read(path: String, split: Int): Iterator[Event] = { note(path); if split == 0 then rows.take(2).iterator else rows.drop(2).iterator }
    assertEquals(plan.bulk(backend.read("events",format))(using backend),expected)
    val limited = model.plan(Request(Vector("median")),maxRows = 2).toOption.get
    assert(limited.bulk(backend.of(rows))(using backend).isLeft)
  }
