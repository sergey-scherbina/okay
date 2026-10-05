package okay.durable.persist

import okay.Answers
import okay.codec.Journalled
import okay.durable.Durable
import okay.persist.{Ack, MemoryStore, Topic}

/** Fixed version-1 bytes, independent of the moved Schema derivation. */
class TestTopicJournalWire extends okay.testkit.Munit.Diagnosed:
  private def bytes(hex: String): Array[Byte] =
    hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private val intent = bytes("00000001a166496e74656e74a46373657100626f70636164646b66696e6765727072696e746861646428322c3329636b65796a6c65676163792d6b6579")
  private val complete = bytes("00000001a168436f6d706c657465a2637365710066616e73776572623d35")
  private val key = "legacy-run".getBytes("UTF-8")
  private val entry = Durable.Entry(0, "add", "add(2,3)", "legacy-key", Some("=5"))

  test("old version-1 records refold into the same completed entry") {
    val topic = MemoryStore().topic("legacy", partitions = 4)
    val partition = Topic.route(key, topic.partitions)
    topic.append(partition, key, intent, Ack.Durable): Unit
    topic.append(partition, key, complete, Ack.Durable): Unit
    val journal = TopicJournal(topic, "legacy-run")
    note("read explicit pre-extraction envelope and CBOR field/case names")
    onFailure(journal.all.toString)
    assertEquals(journal.runId, Some("legacy-run"))
    assertEquals(journal.all, Vector(entry))
    assertEquals(TopicJournal(topic, "other-run").all, Vector.empty)
  }

  test("new writes retain the exact version-1 wire bytes") {
    val topic = MemoryStore().topic("current", partitions = 4)
    val journal = TopicJournal(topic, "legacy-run")
    journal.append(entry.copy(answer = None))
    journal.complete(0, "=5")
    note("compare stored bytes against fixed legacy fixtures")
    onFailure(journal.all.toString)
    topic.read(Topic.route(key, topic.partitions), 0L, 10) match
      case Topic.Read.Records(rs) =>
        assertEquals(rs.map(_.value.toVector), Vector(intent.toVector, complete.toVector))
      case other => fail(s"unexpected $other")
    assertEquals(TopicJournal(topic, "legacy-run").all, Vector(entry))
  }

  enum Op[+A]:
    case Ask extends Op[String]

  given Journalled[Op] with
    def name[A](op: Op[A]): String = "ask"
    def fingerprint[A](op: Op[A]): String = "ask()"
    def withKey[A](op: Op[A], key: String): Op[A] = op
    def perform[A](op: Op[A], inner: Answers[Op]): (A, String) = op match
      case Op.Ask =>
        val answer = inner.handle(Op.Ask)
        (answer, answer)
    def decode[A](op: Op[A], written: String): A = op match
      case Op.Ask => written

  test("topic run identity preserves new keys across refold and separates runs") {
    val topic = MemoryStore().topic("scoped", partitions = 4)
    val first = TopicJournal(topic, "tenant-a/run-1")
    val key = Durable.keyFor(first, 0, Op.Ask)
    first.append(Durable.Entry(0, "ask", "ask()", key, None))
    val restarted = TopicJournal(topic, "tenant-a/run-1")
    onFailure(s"first=${first.all} restarted=${restarted.all}")
    assertEquals(Durable.keyFor(restarted, 0, Op.Ask), key)
    assertEquals(restarted.all.head.key, key)
    assertNotEquals(Durable.keyFor(TopicJournal(topic, "tenant-b/run-1"), 0, Op.Ask), key)
  }
