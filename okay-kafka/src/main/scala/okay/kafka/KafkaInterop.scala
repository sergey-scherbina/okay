package okay.kafka

import okay.{!, %, +, Async, Chunk, Chunks, Produce, async, effect}
import okay.given
import org.apache.kafka.clients.consumer.{Consumer, ConsumerRecord, KafkaConsumer}
import org.apache.kafka.clients.producer.{KafkaProducer, Producer as JProducer, ProducerRecord}
import scala.jdk.CollectionConverters.*

/**
 * Kafka as chunked async streams (specs/external-systems.md): the
 * consumer's poll returns a batch, which is exactly a Chunk — one
 * poll, one chunk, nothing re-buffered. The source is an effectful
 * chunked stream (`Chunk[Record] ! Produce + Async`): between
 * emissions the virtual thread parks in poll. Delivery is
 * at-least-once: commit after processing a chunk, and a supervised
 * consumer restarted from committed offsets re-reads only the
 * uncommitted tail — Kafka's offsets are what make the source
 * REPLAYABLE in the sense of specs/parallel-resilience.md.
 */
object KafkaInterop {

  /** the stream type of a kafka source: chunks of records, awaited */
  type KafkaChunks[K, V] = Chunk[ConsumerRecord[K, V]] ! (Produce + Async)

  /**
   * A subscribed/assigned consumer as an infinite chunked stream:
   * each emitted chunk is one poll's records (empty polls are not
   * emitted — the thread just parked for the timeout). The consumer
   * is NOT closed by the stream; scope it with Resource below.
   */
  def source[K, V](consumer: Consumer[K, V], pollMillis: Long = 1000): KafkaChunks[K, V] =
    type F = Produce + Async
    def go(): KafkaChunks[K, V] =
      effect[F, Chunk[ConsumerRecord[K, V]]](Async.Run { () =>
        val records = consumer.poll(java.time.Duration.ofMillis(pollMillis))
        Chunks.wrap[ConsumerRecord[K, V]](
          records.iterator.asScala.asInstanceOf[Iterator[AnyRef]].toArray)
      }).flatMap { chunk =>
        if chunk.isEmpty then go()
        else effect[F, Chunk[ConsumerRecord[K, V]]](chunk).flatMap(_ => go())
      }

    go()

  /** commit the consumer's position (call after a chunk is processed:
   * at-least-once delivery) */
  def commit[K, V](consumer: Consumer[K, V]): Unit ! Async =
    async(consumer.commitSync())

  /** send one chunk of records, flushed — one program per batch */
  def sink[K, V](producer: JProducer[K, V])(records: Chunk[ProducerRecord[K, V]]): Unit ! Async =
    async {
      records.foreach(producer.send(_))
      producer.flush()
    }

  /** a consumer as a Resource: subscribed on acquire, closed by the scope */
  def managedConsumer[K, V](props: Map[String, AnyRef], topics: Seq[String])
  : Consumer[K, V] ! okay.Resource =
    okay.Resource.acquire {
      val c = new KafkaConsumer[K, V](props.asJava)
      c.subscribe(topics.asJava)
      c
    }(_.close())

  /** a producer as a Resource: closed (flushing) by the scope */
  def managedProducer[K, V](props: Map[String, AnyRef]): JProducer[K, V] ! okay.Resource =
    okay.Resource.acquire(new KafkaProducer[K, V](props.asJava))(_.close())
}
