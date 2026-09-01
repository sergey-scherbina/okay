package okay.persist

import munit.FunSuite
import scala.collection.mutable

/**
 * The queue bridges (specs/data.md, "Queues"): proven against an
 * in-memory fake broker, because the SPIs ARE the whole coupling —
 * a real RabbitMQ/SQS adapter is a named deployment, not tested here.
 * The properties that matter: ingress is at-least-once (append then
 * ack, so a lost ack redelivers and re-appends, never drops), the
 * duplicate collapses one hop downstream by message id, and egress
 * is resumable by offset with the broker's id-dedup giving
 * exactly-once OUTCOME.
 */
class TestQueues extends FunSuite:

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  private def records(r: Topic.Read): Vector[Record] = r match
    case Topic.Read.Records(rs) => rs
    case Topic.Read.TooEarly(b) => fail(s"unexpected TooEarly($b)")

  /** a broker that acks properly: an acked message is gone */
  private class Broker(msgs: Seq[Queues.Incoming]) extends Queues.Source:
    private val pending = mutable.Queue.from(msgs)
    val acked = mutable.Buffer.empty[String]
    def poll(): Option[Queues.Incoming] =
      if pending.isEmpty then None else Some(pending.dequeue())
    def ack(id: String): Unit = acked += id

  /** a broker whose ack is LOST in transit: it keeps redelivering the
   * same message, so the consumer sees it more than once */
  private class LostAckBroker(msg: Queues.Incoming) extends Queues.Source:
    var polled = 0
    def poll(): Option[Queues.Incoming] = { polled += 1; Some(msg) }
    def ack(id: String): Unit = ()   // the broker never sees it

  /** a sink that dedups on message id — a broker with idempotent
   * publish, which is where at-least-once becomes exactly-once */
  private class FakeSink extends Queues.Sink:
    val raw = mutable.Buffer.empty[String]                 // every call, dups included
    val distinct = mutable.LinkedHashMap.empty[String, String]
    var suppressed = 0
    def publish(id: String, value: Array[Byte]): Unit =
      raw += id
      if distinct.contains(id) then suppressed += 1
      else distinct(id) = str(value)

  test("ingress drains the queue into the topic, keyed by id, acking AFTER the append") {
    val broker = Broker(Seq(
      Queues.Incoming("m0", bytes("a")),
      Queues.Incoming("m1", bytes("b")),
      Queues.Incoming("m2", bytes("c"))))
    val topic = MemoryStore().topic("ingest", 1, Policy())

    assertEquals(Queues.ingress(broker, topic), 3)
    val rs = records(topic.read(0, 0, 10))
    assertEquals(rs.map(r => str(r.key)), Vector("m0", "m1", "m2"))
    assertEquals(rs.map(r => str(r.value)), Vector("a", "b", "c"))
    assertEquals(broker.acked.toVector, Vector("m0", "m1", "m2"))  // all confirmed
  }

  test("a lost ack redelivers: the message is appended twice, and dedup collapses it one hop in") {
    val broker = LostAckBroker(Queues.Incoming("m1", bytes("pay")))
    val topic = MemoryStore().topic("ingest", 1, Policy())

    // two ingress passes, each bounded — the broker never saw the ack
    // so it handed the same message back the second time
    Queues.ingress(broker, topic, max = 1)
    Queues.ingress(broker, topic, max = 1)

    val rs = records(topic.read(0, 0, 10))
    assertEquals(rs.size, 2, "at-least-once: the redelivery IS in the log")
    assertEquals(rs.map(r => str(r.key)), Vector("m1", "m1"))
    // the consumer dedups by id: effectively-once, one hop downstream
    val once = Queues.dedup(rs)
    assertEquals(once.map(r => str(r.value)), Vector("pay"))
  }

  test("egress publishes a topic outward, resumable by the returned offset") {
    val topic = MemoryStore().topic("out", 1, Policy())
    (0 until 5).foreach(i => topic.append(0, bytes(s"id$i"), bytes(s"v$i"), Ack.Durable))
    val sink = FakeSink()

    val mid = Queues.egress(topic, sink, from = 0, max = 3)
    assertEquals(mid, 3L)
    val end = Queues.egress(topic, sink, from = mid, max = 3)
    assertEquals(end, 5L)

    assertEquals(sink.distinct.keys.toVector, Vector("id0", "id1", "id2", "id3", "id4"))
    assertEquals(sink.suppressed, 0)   // a clean resume publishes each once
  }

  test("egress after a lost offset re-publishes; the sink's id-dedup makes it exactly-once outcome") {
    val topic = MemoryStore().topic("out", 1, Policy())
    (0 until 3).foreach(i => topic.append(0, bytes(s"id$i"), bytes(s"v$i"), Ack.Durable))
    val sink = FakeSink()

    val next = Queues.egress(topic, sink, from = 0, max = 3)
    assertEquals(next, 3L)
    // the crash: the offset was never journaled, so the resume starts
    // from 0 again and re-publishes what it already sent
    Queues.egress(topic, sink, from = 0, max = 3)

    assertEquals(sink.raw.size, 6, "at-least-once: it published all three twice")
    assertEquals(sink.distinct.keys.toVector, Vector("id0", "id1", "id2"))  // exactly-once outcome
    assertEquals(sink.suppressed, 3)   // the broker's dedup absorbed the replay
  }
