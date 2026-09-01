package okay.kafka

import okay.persist.{Ack, Policy, Record, Store, Topic}
import org.apache.kafka.clients.admin.{AdminClient, NewTopic}
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import org.apache.kafka.common.TopicPartition
import scala.jdk.CollectionConverters.*

/**
 * The durable log over Kafka (specs/persist.md, stage 3): the
 * interop hatch for the business that cannot wait — partitions,
 * replication, election all INHERITED from the engine that did the
 * twenty years, behind the same `Store` trait, so consumers bound
 * at stage 0 never hear about it.
 *
 * The two stage-3 rules, obeyed: the sync `Topic` SPI BLOCKS
 * honestly on the client's futures (virtual threads make it real);
 * and the engine KEEPS its own operations — Kafka manages
 * retention and compaction itself, so `compact()` here REFUSES by
 * name ("configure the topic"), and `begin` is whatever Kafka's
 * own retention has made true.
 *
 * Acks: `Received` maps to fire-and-forget, `Durable` and
 * `Replicated` both block on the send whose producer is configured
 * `acks=all` — at least as durable as asked, stated rather than
 * approximated downward.
 *
 * Exactly-once (kafka-eos), inherited from the engine that has it:
 *  - the producer is IDEMPOTENT (`enable.idempotence`), so a retry
 *    after a lost ack does not duplicate the record — effectively-
 *    once TO Kafka, the sink half, always on.
 *  - the consumer reads `read_committed`, so a reader never observes
 *    an aborted transaction's records nor an open one's — the reader
 *    half. `end` is then the last STABLE offset, which is exactly the
 *    end a read-committed consumer can reach.
 *  - `transaction(transactionalId) { tx => tx.append(...) }` runs a
 *    set of appends across partitions and topics ATOMICALLY: commit
 *    on a normal return, abort on a throw. The `transactionalId`
 *    fences a zombie producer of the same id (Kafka's own EOS). This
 *    is the "concrete case" specs/persist.md kept transactions out of
 *    scope pending — and it costs no new machinery, because Kafka has
 *    transactions and an interop inherits the engine's ops.
 */
final class KafkaStore(bootstrap: String, group: String = "okay-persist")
  extends Store:
  import KafkaStore.*

  private val admin = AdminClient.create(
    Map[String, AnyRef]("bootstrap.servers" -> bootstrap).asJava)
  private val producer = KafkaProducer[Array[Byte], Array[Byte]](
    Map[String, AnyRef](
      "bootstrap.servers" -> bootstrap,
      "acks" -> "all",
      // effectively-once to Kafka: a producer retry cannot duplicate
      "enable.idempotence" -> "true",
      "key.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
      "value.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
    ).asJava)
  private val consumer = KafkaConsumer[Array[Byte], Array[Byte]](
    Map[String, AnyRef](
      "bootstrap.servers" -> bootstrap,
      "group.id" -> group,
      "enable.auto.commit" -> "false",
      "auto.offset.reset" -> "none",
      // the reader half of EOS: aborted and in-flight records are invisible
      "isolation.level" -> "read_committed",
      "key.deserializer" -> "org.apache.kafka.common.serialization.ByteArrayDeserializer",
      "value.deserializer" -> "org.apache.kafka.common.serialization.ByteArrayDeserializer",
    ).asJava)

  private var open = Vector.empty[KTopic]
  // one transactional producer per id, initTransactions'd once and
  // reused; a transaction on one id is serialized (Kafka forbids
  // concurrent transactions on a producer, and fences by the id)
  private var txnProducers = Map.empty[String, KafkaProducer[Array[Byte], Array[Byte]]]

  def topic(name: String, partitions: Int, policy: Policy): Topic = synchronized {
    open.find(_.name == name) match
      case Some(t) =>
        if t.partitions != partitions then
          throw IllegalArgumentException(
            s"topic $name has ${t.partitions} partitions; asked for $partitions — " +
              "rerouting keys would break per-key order")
        t
      case None =>
        val existing = admin.describeTopics(java.util.List.of(name)).allTopicNames()
        val parts =
          try Some(existing.get().get(name).partitions().size())
          catch case _: Throwable => None
        parts match
          case Some(p) if p != partitions =>
            throw IllegalArgumentException(
              s"topic $name exists with $p partitions; asked for $partitions")
          case Some(_) => ()
          case None =>
            admin.createTopics(java.util.List.of(
              NewTopic(name, partitions, 1.toShort))).all().get()
            ()
        val t = KTopic(name, partitions)
        open :+= t
        t
  }

  def topics: Vector[String] = synchronized {
    admin.listTopics().names().get().asScala.toVector.filterNot(_.startsWith("__")).sorted
  }

  def stats: Store.Stats = synchronized {
    Store.Stats(open.map { t =>
      Store.TopicStats(t.name, Vector.tabulate(t.partitions) { p =>
        val (b, e) = t.range(p)
        Store.PartitionStats(p, b, e, 0L, 1) // bytes/segments are the engine's own
      })
    })
  }

  /**
   * Run a set of appends as ONE Kafka transaction (kafka-eos): the
   * records — across any partitions and topics — become visible to
   * read-committed consumers all at once on commit, or not at all on
   * abort. A normal return commits; any throw aborts and re-raises.
   * The `transactionalId` is a STABLE name for this logical producer:
   * it fences a zombie of the same id, so reuse the same id for the
   * same duty across restarts. Topics must already exist (call
   * `topic(name, partitions)` first); the block runs synchronously.
   */
  def transaction[A](transactionalId: String)(body: Txn => A): A =
    val p = txnProducer(transactionalId)
    p.synchronized {
      p.beginTransaction()
      val a =
        try body(new Txn(p))
        catch
          case e: Throwable =>
            try p.abortTransaction() catch case _: Throwable => ()
            throw e
      p.commitTransaction()
      a
    }

  private def txnProducer(id: String): KafkaProducer[Array[Byte], Array[Byte]] =
    synchronized {
      txnProducers.get(id) match
        case Some(p) => p
        case None =>
          val p = KafkaProducer[Array[Byte], Array[Byte]](
            Map[String, AnyRef](
              "bootstrap.servers" -> bootstrap,
              "acks" -> "all",
              "enable.idempotence" -> "true",
              "transactional.id" -> id,
              "key.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
              "value.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
            ).asJava)
          p.initTransactions()
          txnProducers = txnProducers.updated(id, p)
          p
    }

  def close(): Unit =
    producer.close()
    synchronized {
      txnProducers.values.foreach(p => try p.close() catch case _: Throwable => ())
      txnProducers = Map.empty
    }
    consumer.close()
    admin.close()

  private final class KTopic(val name: String, val partitions: Int) extends Topic:

    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      val record = ProducerRecord[Array[Byte], Array[Byte]](
        name, partition, if key.isEmpty then null else key, value)
      ack match
        case Ack.Received =>
          // fire-and-forget cannot know its offset; the log's next
          // is the honest answer for a caller that declined to wait
          val e = end(partition)
          producer.send(record)
          e
        case _ => producer.send(record).get().offset()

    def read(partition: Int, from: Long, max: Int): Topic.Read =
      KafkaStore.this.synchronized {
        val tp = TopicPartition(name, partition)
        val b = beginOf(tp)
        if from < b then Topic.Read.TooEarly(b)
        else
          consumer.assign(java.util.List.of(tp))
          consumer.seek(tp, from)
          val out = Vector.newBuilder[Record]
          var got = 0
          var polls = 0
          val e = endOf(tp)
          while got < max && polls < 10 && {
            val remaining = e - from - got
            remaining > 0
          } do
            val batch = consumer.poll(java.time.Duration.ofMillis(200))
            polls += 1
            for r <- batch.records(tp).asScala if got < max do
              out += Record(r.offset(), r.timestamp(),
                if r.key() == null then Array.empty else r.key(), r.value())
              got += 1
          Topic.Read.Records(out.result())
      }

    def begin(partition: Int): Long = beginOf(TopicPartition(name, partition))
    def end(partition: Int): Long = endOf(TopicPartition(name, partition))

    /** the engine owns compaction and retention (specs/persist.md,
     * stage 3): configure the TOPIC, do not call this */
    def compact(partition: Int): Unit =
      throw UnsupportedOperationException(
        s"Kafka owns compaction: set cleanup.policy=compact on topic $name — " +
          "an interop inherits the engine's ops")

    private[KafkaStore] def range(p: Int): (Long, Long) =
      val tp = TopicPartition(name, p)
      (beginOf(tp), endOf(tp))

    // the consumer is not thread-safe; every touch goes under the
    // store's lock
    private def beginOf(tp: TopicPartition): Long = KafkaStore.this.synchronized {
      consumer.beginningOffsets(java.util.List.of(tp)).get(tp).longValue()
    }
    private def endOf(tp: TopicPartition): Long = KafkaStore.this.synchronized {
      consumer.endOffsets(java.util.List.of(tp)).get(tp).longValue()
    }

object KafkaStore:
  def apply(bootstrap: String, group: String = "okay-persist"): KafkaStore =
    new KafkaStore(bootstrap, group)

  /**
   * The append surface inside a transaction (kafka-eos): partition-
   * addressed, across any topic. The returned offset is the one the
   * broker assigned — durable and visible to read-committed readers
   * only once the enclosing `transaction` commits; an abort unwrites
   * it (the offset is spent, a hole, the log's ordinary story).
   */
  final class Txn private[kafka] (producer: KafkaProducer[Array[Byte], Array[Byte]]):
    def append(topic: String, partition: Int, key: Array[Byte], value: Array[Byte]): Long =
      val record = ProducerRecord[Array[Byte], Array[Byte]](
        topic, partition, if key.isEmpty then null else key, value)
      producer.send(record).get().offset()
