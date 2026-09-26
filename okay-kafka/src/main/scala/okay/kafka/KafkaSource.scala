package okay.kafka

import okay.Chunks
import okay.cluster.{Flow, Scope}
import okay.codec.Schema
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.common.TopicPartition
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

/** one record of a Kafka source: where it was, and its bytes */
final case class KafkaRecord(partition: Int, offset: Long, timestamp: Long,
                             key: Array[Byte], value: Array[Byte])

/** a topic's partitions as a job's parameters: each partition's span
 * `[from, until)`, taken once at submission, so a position stays an
 * offset relative to a `from` that retention cannot move */
final case class KafkaSpans(bootstrap: String, topic: String, from: Vector[Long], until: Vector[Long],
                            contiguous: Boolean = true)

object KafkaSpans:
  given Schema[KafkaSpans] = Schema.derived

/**
 * A KAFKA TOPIC AS A SOURCE OF THE CLUSTER ENGINE (specs/dataflow.md,
 * stage 16).
 *
 * The topic's Kafka partitions are the flow's partitions, one for one.
 * A partition is opened positioned (`Flow.opened`): a session asked to
 * open `start` records in SEEKS to `from + start` — stage 11's seek on a
 * real broker — so a replacement worker or a resumed coordinator reads
 * exactly the records its journal has not folded, and the consumer is
 * closed with the partition's scope.
 *
 * THE POSITIONS ARE THE ENGINE'S, not a consumer group's: they are in
 * the coordinator's journal with the fold (stage 11), and nothing is
 * committed to Kafka — see the spec's Decision for why a second record
 * of the same fact is worse than none.
 *
 * `contiguous` (the default) promises that the partition's offsets have
 * no holes, which is true of a topic written without transactions and
 * never compacted; every record read is checked against it, and a hole
 * is refused by name rather than read past. `contiguous = false` makes a
 * seek a read that skips `start` records — correct on any topic, and
 * paid for in reading.
 */
object KafkaSource:

  /** the topic's partitions as they stand: from the first live offset
   * to the end, per partition */
  def spans(bootstrap: String, topic: String, contiguous: Boolean = true): KafkaSpans =
    withConsumer(bootstrap) { c =>
      val parts = c.partitionsFor(topic).asScala.map(_.partition).sorted.toVector
      require(parts.nonEmpty, s"the topic '$topic' has no partitions (does it exist?)")
      val tps = parts.map(TopicPartition(topic, _)).asJava
      val b = c.beginningOffsets(tps)
      val e = c.endOffsets(tps)
      KafkaSpans(bootstrap, topic, parts.map(p => b.get(TopicPartition(topic, p)).longValue()),
        parts.map(p => e.get(TopicPartition(topic, p)).longValue()), contiguous)
    }

  /** the flow: partition `i` of the job is Kafka partition `i` of the
   * topic, and the width must be the topic's */
  def flow(s: KafkaSpans, parts: Int, chunk: Int = 500): Flow[KafkaRecord] =
    require(s.from.length == s.until.length, "a span per partition")
    if parts != s.from.length then
      throw IllegalArgumentException(
        s"the topic '${s.topic}' has ${s.from.length} partitions and the job asked for $parts: a Kafka " +
          "source's partitions ARE the topic's (specs/dataflow.md, stage 16)")
    Flow.opened(parts)((i, start, scope) => partition(s, i, start, chunk, scope))

  /** records fetched from a broker by this process — how a test tells
   * a seek from a replay */
  val fetched: AtomicLong = AtomicLong(0)

  private[kafka] def partition(s: KafkaSpans, i: Int, start: Long, chunk: Int, scope: Scope): Chunks[KafkaRecord] =
    val tp = TopicPartition(s.topic, i)
    val until = s.until(i)
    lazy val consumer =
      val c = open(s.bootstrap)
      scope.onEnd(() => c.close())
      c.assign(java.util.List.of(tp))
      // a SEEK where the offsets are contiguous; otherwise the start,
      // and `start` records skipped below
      c.seek(tp, if s.contiguous then s.from(i) + start else s.from(i))
      c
    val it = new Iterator[KafkaRecord]:
      private var next0: Long = if s.contiguous then s.from(i) + start else s.from(i)
      private var skip: Long = if s.contiguous then 0L else start
      private var buf: Iterator[KafkaRecord] = Iterator.empty
      private var idle = 0
      private def fill(): Unit =
        while !buf.hasNext && next0 < until do
          val rs = consumer.poll(java.time.Duration.ofMillis(200)).records(tp).asScala
          if rs.isEmpty then
            idle += 1
            // the end may be a transaction's marker, which is an offset
            // with no record: the consumer's position steps over it
            if consumer.position(tp) >= until then next0 = until
            else if idle > 300 then
              throw IllegalStateException(s"${s.topic}/$i: nothing arrived for a minute at offset $next0 of $until")
          else
            idle = 0
            val out = Vector.newBuilder[KafkaRecord]
            for r <- rs if r.offset < until do
              if s.contiguous && r.offset != next0 then
                throw IllegalStateException(
                  s"${s.topic}/$i: expected offset $next0 and read ${r.offset} — the partition's offsets are " +
                    "not contiguous (a transactional or compacted topic), so a position is not an offset; " +
                    "read it with contiguous = false (specs/dataflow.md, stage 16)")
              next0 = r.offset + 1
              fetched.incrementAndGet(): Unit
              if skip > 0 then skip -= 1
              else out += KafkaRecord(i, r.offset, r.timestamp,
                if r.key == null then Array.emptyByteArray else r.key, r.value)
            if rs.exists(_.offset >= until) then next0 = until
            buf = out.result().iterator
      def hasNext: Boolean = { fill(); buf.hasNext }
      def next(): KafkaRecord = { fill(); buf.next() }
    Chunks.fromIterator(it, chunk)

  private def open(bootstrap: String): KafkaConsumer[Array[Byte], Array[Byte]] =
    KafkaConsumer[Array[Byte], Array[Byte]](Map[String, AnyRef](
      "bootstrap.servers" -> bootstrap,
      "enable.auto.commit" -> "false",
      "isolation.level" -> "read_committed",
      "max.poll.records" -> "2000",
      "key.deserializer" -> "org.apache.kafka.common.serialization.ByteArrayDeserializer",
      "value.deserializer" -> "org.apache.kafka.common.serialization.ByteArrayDeserializer",
    ).asJava)

  private def withConsumer[A](bootstrap: String)(f: KafkaConsumer[Array[Byte], Array[Byte]] => A): A =
    val c = open(bootstrap)
    try f(c) finally c.close()
