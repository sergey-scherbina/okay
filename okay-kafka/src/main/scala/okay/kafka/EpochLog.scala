package okay.kafka

import org.apache.kafka.clients.admin.{Admin, NewTopic}
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import org.apache.kafka.common.TopicPartition
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

/**
 * AN EPOCH OF OUTPUT LARGER THAN A RECORD, EXACTLY ONCE
 * (specs/dataflow.md, staging-large-epochs).
 *
 * Stage 9's staging writer appends ONE record per epoch, which makes a
 * half-written epoch impossible and caps an epoch at a record. This is
 * the writer for the epoch that does not fit: its rows are framed into
 * chunks of at most `chunkBytes` and every chunk of an epoch goes out
 * inside ONE Kafka transaction, committed when the epoch is final. A
 * `read_committed` reader — `epochs` below, or any Kafka consumer so
 * configured — therefore sees an epoch whole or not at all.
 *
 * A WRITER THAT DIES MID-EPOCH leaves a transaction open. The successor
 * is built with the SAME `transactionalId` (a stable name for the duty,
 * not for the process), and its `initTransactions` fences the dead one
 * and aborts what it left open — nothing of that epoch ever becomes
 * visible. It then learns the last COMMITTED epoch from the log, as
 * stage 9's writer does, and writes the epoch again.
 *
 * Plugged into a stream by the seam stage 9 left for it:
 *
 * {{{
 * val log = EpochLog(bootstrap, "scores", "scores-writer")
 * Wire.tumblingStaged(...)((epoch, panes) => log.move(epoch, panes.iterator.map(encode)): Unit)
 * }}}
 *
 * One partition, on purpose: the order of epochs is the order of the
 * log, and the recovery rule "the highest committed epoch" needs one.
 */
final class EpochLog(bootstrap: String, val topic: String, transactionalId: String,
                     chunkBytes: Int = EpochLog.ChunkBytes):
  require(chunkBytes > 0, "a chunk holds at least one byte")
  private val tp = TopicPartition(topic, 0)

  EpochLog.ensure(bootstrap, topic)

  private val producer: KafkaProducer[Array[Byte], Array[Byte]] =
    val p = KafkaProducer[Array[Byte], Array[Byte]](Map[String, AnyRef](
      "bootstrap.servers" -> bootstrap,
      "acks" -> "all",
      "enable.idempotence" -> "true",
      "transactional.id" -> transactionalId,
      "linger.ms" -> "5",
      "max.request.size" -> (chunkBytes + 64 * 1024).toString,
      "key.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
      "value.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
    ).asJava)
    // fences a dead writer of the same id and aborts its open epoch
    p.initTransactions()
    p

  /** the highest epoch already committed, learned from the log once */
  private var landed: Int = EpochLog.lastCommitted(bootstrap, tp)

  /** the highest committed epoch this writer knows of */
  def committed: Int = synchronized(landed)

  /**
   * Write `epoch`'s rows, ONCE: an epoch at or below the last committed
   * one is a repeat (stage 9 may move an epoch twice across a restart)
   * and is skipped — `false`. Otherwise every chunk goes out in one
   * transaction, committed after the last; a failure aborts it.
   */
  def move(epoch: Int, rows: Iterator[Array[Byte]]): Boolean = synchronized {
    if epoch <= landed then false
    else
      producer.beginTransaction()
      try
        val chunks = EpochLog.chunks(rows, chunkBytes)
        var i = 0
        while chunks.hasNext do
          producer.send(ProducerRecord(topic, 0, EpochLog.key(epoch, i), chunks.next())): Unit
          i += 1
        producer.commitTransaction()
        landed = epoch
        true
      catch case t: Throwable =>
        try producer.abortTransaction() catch case _: Throwable => ()
        throw t
  }

  def close(): Unit = producer.close()

object EpochLog:
  /** under Kafka's 1 MB default `max.message.bytes`, with room for the
   * record's own overhead */
  val ChunkBytes: Int = 900 * 1024

  /** the key of chunk `i` of `epoch` */
  private[kafka] def key(epoch: Int, i: Int): Array[Byte] =
    ByteBuffer.allocate(8).putInt(epoch).putInt(i).array()

  private[kafka] def epochOf(key: Array[Byte]): Int = ByteBuffer.wrap(key).getInt

  /**
   * THE FRAMING: rows, each prefixed by its length, packed into chunks
   * of at most `limit` bytes — a row larger than that alone in its chunk
   * (and then limited by the broker's record size, which says so).
   */
  def chunks(rows: Iterator[Array[Byte]], limit: Int): Iterator[Array[Byte]] = new Iterator[Array[Byte]]:
    private var held: Array[Byte] | Null = null
    def hasNext: Boolean = held != null || rows.hasNext
    def next(): Array[Byte] =
      val out = java.io.ByteArrayOutputStream()
      val d = java.io.DataOutputStream(out)
      @tailrec def fill(): Unit =
        val row: Array[Byte] | Null = if held != null then held else if rows.hasNext then rows.next() else null
        held = null
        if row != null then
          val r = row.nn
          if out.size() > 0 && out.size() + 4 + r.length > limit then held = r
          else
            d.writeInt(r.length); d.write(r)
            fill()
      fill()
      out.toByteArray

  /** a chunk's rows */
  def rows(chunk: Array[Byte]): Vector[Array[Byte]] =
    val b = ByteBuffer.wrap(chunk)
    val out = Vector.newBuilder[Array[Byte]]
    while b.remaining() >= 4 do
      val n = b.getInt
      val r = new Array[Byte](n)
      b.get(r)
      out += r
    out.result()

  /**
   * EVERY COMMITTED EPOCH, as a `read_committed` reader sees it: epoch
   * to its rows in order. An epoch whose transaction is open or aborted
   * is absent — that is the whole claim, and what the test reads with.
   */
  def epochs(bootstrap: String, topic: String): Map[Int, Vector[Array[Byte]]] =
    val tp = TopicPartition(topic, 0)
    withConsumer(bootstrap) { c =>
      c.assign(java.util.List.of(tp))
      c.seekToBeginning(java.util.List.of(tp))
      val end = c.endOffsets(java.util.List.of(tp)).get(tp).longValue()
      val out = scala.collection.mutable.LinkedHashMap.empty[Int, Vector[Array[Byte]]]
      var idle = 0
      while c.position(tp) < end && idle < 50 do
        val batch = c.poll(java.time.Duration.ofMillis(200)).records(tp).asScala
        if batch.isEmpty then idle += 1 else idle = 0
        for r <- batch do
          val e = epochOf(r.key())
          out.update(e, out.getOrElse(e, Vector.empty) ++ rows(r.value()))
      out.toMap
    }

  /**
   * THE LAST COMMITTED EPOCH, read from the tail backwards in widening
   * windows — an aborted epoch leaves its records and a marker behind
   * the end, which a `read_committed` reader steps over. Bounded: the
   * window doubles until it reaches the beginning of the log.
   */
  private def lastCommitted(bootstrap: String, tp: TopicPartition): Int =
    withConsumer(bootstrap) { c =>
      c.assign(java.util.List.of(tp))
      val begin = c.beginningOffsets(java.util.List.of(tp)).get(tp).longValue()
      val end = c.endOffsets(java.util.List.of(tp)).get(tp).longValue()
      @tailrec def scan(window: Long): Int =
        val from = math.max(begin, end - window)
        c.seek(tp, from)
        var high = 0
        var idle = 0
        while c.position(tp) < end && idle < 50 do
          val batch = c.poll(java.time.Duration.ofMillis(200)).records(tp).asScala
          if batch.isEmpty then idle += 1 else idle = 0
          for r <- batch do high = math.max(high, epochOf(r.key()))
        if high > 0 || from == begin then high else scan(window * 2)
      if end == begin then 0 else scan(256)
    }

  private def withConsumer[A](bootstrap: String)(f: KafkaConsumer[Array[Byte], Array[Byte]] => A): A =
    val c = KafkaConsumer[Array[Byte], Array[Byte]](Map[String, AnyRef](
      "bootstrap.servers" -> bootstrap,
      "isolation.level" -> "read_committed",
      "enable.auto.commit" -> "false",
      "max.partition.fetch.bytes" -> (4 * 1024 * 1024).toString,
      "key.deserializer" -> "org.apache.kafka.common.serialization.ByteArrayDeserializer",
      "value.deserializer" -> "org.apache.kafka.common.serialization.ByteArrayDeserializer",
    ).asJava)
    try f(c) finally c.close()

  /** the topic, one partition, created if it is not there */
  private def ensure(bootstrap: String, topic: String): Unit =
    val admin = Admin.create(Map[String, AnyRef]("bootstrap.servers" -> bootstrap).asJava)
    try
      if !admin.listTopics().names().get().contains(topic) then
        try admin.createTopics(java.util.List.of(NewTopic(topic, 1, 1.toShort))).all().get(): Unit
        catch case e: java.util.concurrent.ExecutionException
          if e.getCause.isInstanceOf[org.apache.kafka.common.errors.TopicExistsException] => ()
    finally admin.close()
