package okay.kafka

import okay.persist.{Ack, Topic}
import munit.FunSuite

/**
 * Exactly-once on the Kafka interop (kafka-eos), against a REAL
 * broker (live; skips where absent — the okay-kafka docker on 9092).
 * The engine has transactions; the interop inherits them. Proven:
 * a committed transaction is atomic and visible, across topics; an
 * aborted one leaves NOTHING a read-committed reader can see.
 */
class TestKafkaEos extends FunSuite:

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(120, "s")

  val bootstrap = sys.env.getOrElse("OKAY_KAFKA", "127.0.0.1:9092")

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")
  private def fresh(p: String): String = s"okay-eos-$p-${System.nanoTime()}"

  private def records(r: Topic.Read): Vector[okay.persist.Record] = r match
    case Topic.Read.Records(rs) => rs
    case Topic.Read.TooEarly(b) => fail(s"unexpected TooEarly($b)")

  /** a reader right after commit must tolerate the read-committed
   * last-stable-offset propagating: retry until `want` records show
   * or the deadline passes (the LSO is eventually consistent) */
  private def readUntil(t: Topic, partition: Int, want: Int): Vector[okay.persist.Record] =
    val deadline = System.nanoTime() + 8_000_000_000L
    var rs = records(t.read(partition, 0L, want + 5))
    while rs.size < want && System.nanoTime() < deadline do
      Thread.sleep(200)
      rs = records(t.read(partition, 0L, want + 5))
    rs

  /** the broker must have its transaction coordinator reachable (a
   * single-node dev broker with the txn-state log at RF=1); if not,
   * the EOS battery skips rather than fails on the environment */
  lazy val txnReady: Option[KafkaStore] =
    if !TestKafkaSupport.reachable(bootstrap) then None
    else
      try
        val s = KafkaStore(bootstrap)
        // force the coordinator handshake once; a broker without txn
        // support throws here and the battery skips
        s.topic(fresh("probe"), 1): Unit
        s.transaction(fresh("probe-txn"))(_ => ())
        Some(s)
      catch case _: Throwable => None

  test("a committed transaction is atomic: all its records become visible together, in order") {
    assume(txnReady.isDefined, s"no transactional Kafka at $bootstrap — the EOS suite skips")
    val store = txnReady.get
    val name = fresh("commit")
    val t = store.topic(name, 1)
    store.transaction(fresh("txn-commit")) { tx =>
      tx.append(name, 0, bytes("k0"), bytes("a")): Unit
      tx.append(name, 0, bytes("k1"), bytes("b")): Unit
      tx.append(name, 0, bytes("k2"), bytes("c")): Unit
    }: Unit
    assertEquals(readUntil(t, 0, 3).map(r => str(r.value)), Vector("a", "b", "c"))
  }

  test("an aborted transaction unwrites itself: a read-committed reader sees none of it") {
    assume(txnReady.isDefined, s"no transactional Kafka at $bootstrap — the EOS suite skips")
    val store = txnReady.get
    val name = fresh("abort")
    val t = store.topic(name, 1)
    t.append(0, Array.empty, bytes("v0"), Ack.Durable): Unit // a committed baseline

    val e = intercept[RuntimeException](
      store.transaction(fresh("txn-abort")) { tx =>
        tx.append(name, 0, Array.empty, bytes("doomed")): Unit
        throw RuntimeException("boom")
      })
    assert(e.getMessage.contains("boom"), e.getMessage)

    // only the baseline survives; the doomed record is invisible
    assertEquals(records(t.read(0, 0L, 10)).map(r => str(r.value)), Vector("v0"))
  }

  test("one transaction spans topics: cross-topic writes commit atomically") {
    assume(txnReady.isDefined, s"no transactional Kafka at $bootstrap — the EOS suite skips")
    val store = txnReady.get
    val a = fresh("cross-a")
    val b = fresh("cross-b")
    val ta = store.topic(a, 1)
    val tb = store.topic(b, 1)
    store.transaction(fresh("txn-cross")) { tx =>
      tx.append(a, 0, Array.empty, bytes("in-a")): Unit
      tx.append(b, 0, Array.empty, bytes("in-b")): Unit
    }: Unit
    assertEquals(readUntil(ta, 0, 1).map(r => str(r.value)), Vector("in-a"))
    assertEquals(readUntil(tb, 0, 1).map(r => str(r.value)), Vector("in-b"))
  }

  override def afterAll(): Unit = txnReady.foreach(_.close())
