package okay.kafka

import okay.persist.{Ack, Topic}
import munit.FunSuite

/**
 * The interop engine against a REAL Kafka (live pattern: skips
 * where the broker is absent — the okay-kafka docker container on
 * 9092). The core log battery holds; retention and compaction stay
 * the ENGINE's own and our calls refuse by name.
 */
class TestKafkaStore extends FunSuite:

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(120, "s")

  val bootstrap = sys.env.getOrElse("OKAY_KAFKA", "127.0.0.1:9092")

  lazy val available: Boolean =
    TestKafkaSupport.reachable(bootstrap) && (
      try
        val s = KafkaStore(bootstrap)
        try { s.topics: Unit; true } finally s.close(): Unit
      catch case _: Throwable => false)

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  private def fresh(): String = s"okay-test-${System.nanoTime()}"

  private def records(r: Topic.Read): Vector[okay.persist.Record] = r match
    case Topic.Read.Records(rs) => rs
    case Topic.Read.TooEarly(b) => fail(s"unexpected TooEarly($b)")

  test("append then read round-trips through the broker: order, offsets, bytes") {
    assume(available, s"no Kafka at $bootstrap — the live suite skips")
    val store = KafkaStore(bootstrap)
    try
      val t = store.topic(fresh(), partitions = 2)
      val offs = (0 until 5).map(i => t.append(i % 2, bytes(s"k$i"), bytes(s"v$i"), Ack.Durable))
      assertEquals(offs.grouped(2).toVector.head.head, 0L)
      val rs = records(t.read(0, 0L, 10))
      assertEquals(rs.map(r => str(r.value)), Vector("v0", "v2", "v4"))
      assertEquals(rs.map(_.offset), Vector(0L, 1L, 2L))
      assertEquals(t.end(0), 3L)
      assertEquals(t.begin(0), 0L)
    finally store.close()
  }

  test("poll-on-end: a caught-up reader sees the later append — the tail contract holds remotely") {
    assume(available, s"no Kafka at $bootstrap — the live suite skips")
    val store = KafkaStore(bootstrap)
    try
      val t = store.topic(fresh())
      t.append(0, Array.empty, bytes("v0"), Ack.Durable): Unit
      val caughtUp = t.end(0)
      assertEquals(records(t.read(0, caughtUp, 10)), Vector.empty)
      t.append(0, Array.empty, bytes("late"), Ack.Durable): Unit
      assertEquals(records(t.read(0, caughtUp, 10)).map(r => str(r.value)), Vector("late"))
    finally store.close()
  }

  test("the engine keeps its own ops: compact refuses by name; partition mismatch refuses") {
    assume(available, s"no Kafka at $bootstrap — the live suite skips")
    val store = KafkaStore(bootstrap)
    try
      val name = fresh()
      val t = store.topic(name, partitions = 2)
      val e = intercept[UnsupportedOperationException](t.compact(0))
      assert(e.getMessage.contains("cleanup.policy"), e.getMessage)
      intercept[IllegalArgumentException](store.topic(name, partitions = 5))
    finally store.close()
  }

  test("a persist consumer runs unchanged over the broker: the Typed view decodes") {
    assume(available, s"no Kafka at $bootstrap — the live suite skips")
    val store = KafkaStore(bootstrap)
    try
      final case class Ev(id: String, n: Int)
      given okay.codec.Schema[Ev] = okay.codec.Schema.derived
      val t = store.topic(fresh())
      val typed = okay.persist.Typed[Ev](t, version = 1, upcasts = Map.empty)
      typed.append(0, bytes("k"), Ev("a", 1), Ack.Durable): Unit
      typed.append(0, bytes("k"), Ev("b", 2), Ack.Durable): Unit
      typed.read(0, 0L, 10) match
        case okay.persist.Typed.Read.Records(ds) =>
          assertEquals(ds.collect { case okay.persist.Typed.Decoded.Ok(_, _, _, e) => e },
            Vector(Ev("a", 1), Ev("b", 2)))
        case other => fail(s"unexpected $other")
    finally store.close()
  }
