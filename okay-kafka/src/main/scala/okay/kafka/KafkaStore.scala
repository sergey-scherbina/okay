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
      "key.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
      "value.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
    ).asJava)
  private val consumer = KafkaConsumer[Array[Byte], Array[Byte]](
    Map[String, AnyRef](
      "bootstrap.servers" -> bootstrap,
      "group.id" -> group,
      "enable.auto.commit" -> "false",
      "auto.offset.reset" -> "none",
      "key.deserializer" -> "org.apache.kafka.common.serialization.ByteArrayDeserializer",
      "value.deserializer" -> "org.apache.kafka.common.serialization.ByteArrayDeserializer",
    ).asJava)

  private var open = Vector.empty[KTopic]

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

  def close(): Unit =
    producer.close()
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
