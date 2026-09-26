package okay.kafka

import okay.{Aggregator, given}
import okay.cluster.{Cluster, Feeds, Flow, Flows, Job, Jobs, Scope, Wire}
import okay.codec.Schema
import org.apache.kafka.clients.admin.{Admin, NewTopic}
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import java.nio.ByteBuffer
import scala.jdk.CollectionConverters.*

/** the count and the sum of the values of a Kafka topic */
object KafkaSumJob extends Job[KafkaSpans, (Long, Long)] {
  type A = KafkaRecord
  def name: String = "test.kafka.sum"
  def params: Schema[KafkaSpans] = summon[Schema[KafkaSpans]]
  def answer: Schema[(Long, Long)] = Schema.derived
  def flow(s: KafkaSpans, parts: Int): Flow[KafkaRecord] = KafkaSource.flow(s, parts)
  val both: Aggregator[KafkaRecord, (Long, Long), (Long, Long)] =
    Aggregator[KafkaRecord, (Long, Long), (Long, Long)]((0L, 0L))((a, r) =>
      (a._1 + 1, a._2 + ByteBuffer.wrap(r.value).getLong))((a, b) => (a._1 + b._1, a._2 + b._2))(identity)
  def sink(s: KafkaSpans): Wire[KafkaRecord, (Long, Long)] = Wire.fold(both)(using Schema.derived)
}

/**
 * KAFKA AS A SOURCE (specs/dataflow.md, stage 16). LIVE: Kafka on 9092
 * (`docker run -p 9092:9092 apache/kafka:3.9.0`); skips without one.
 */
class TestKafkaSource extends munit.FunSuite {
  Jobs.register(KafkaSumJob)

  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(600, "s")

  val bootstrap: String = sys.env.getOrElse("OKAY_KAFKA", "127.0.0.1:9092")

  def live(name: String)(body: => Any): Unit =
    test(name.tag(new munit.Tag("Live"))) {
      assume(TestKafkaSupport.reachable(bootstrap), s"no Kafka on $bootstrap")
      body
    }

  def value(i: Int): Long = math.floorMod(Feeds.mix(i.toLong), 1000L)

  def topicOf(name: String, parts: Int): String =
    val admin = Admin.create(Map[String, AnyRef]("bootstrap.servers" -> bootstrap).asJava)
    try admin.createTopics(java.util.List.of(NewTopic(name, parts, 1.toShort))).all().get(): Unit
    finally admin.close()
    name

  def producer(extra: (String, AnyRef)*): KafkaProducer[Array[Byte], Array[Byte]] =
    KafkaProducer[Array[Byte], Array[Byte]]((Map[String, AnyRef](
      "bootstrap.servers" -> bootstrap,
      "linger.ms" -> "20",
      "batch.size" -> (256 * 1024).toString,
      "key.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
      "value.serializer" -> "org.apache.kafka.common.serialization.ByteArraySerializer",
    ) ++ extra).asJava)

  def bytes(v: Long): Array[Byte] = ByteBuffer.allocate(8).putLong(v).array()

  live("1M records in 4 partitions, a worker killed mid-epoch: every record counted once, and the replacement SEEKS") {
    val n = 1000000
    val topic = topicOf(s"okay-source-${System.nanoTime()}", 4)
    val p = producer()
    for i <- 0 until n do p.send(ProducerRecord(topic, i % 4, bytes(i), bytes(value(i)))): Unit
    p.flush(); p.close()
    val spans = KafkaSource.spans(bootstrap, topic)
    assertEquals(spans.until.zip(spans.from).map(_ - _).sum, n.toLong)

    val dying = okay.cluster.Dying(5)
    val before = KafkaSource.fetched.get
    val take = 20000
    val got = Cluster.stream(KafkaSumJob, spans, 4, Vector(dying, Cluster.local, Cluster.local), take).runWith
    val read = KafkaSource.fetched.get - before

    assertEquals(got.value, (n.toLong, (0 until n).map(value).sum), "not every record counted once")
    assert(dying.dead && got.failed > 0, "no worker died — the test tested nothing")
    // a replay from zero would fetch the dead worker's partitions twice;
    // a seek fetches the topic once, plus the epochs the dead one had not
    // committed and what its consumers had buffered past them
    assert(read < n + 4 * (2 * take + 2 * 2000), s"$read records fetched for a topic of $n — a replay, not a seek")
  }

  live("offsets with holes are refused by name, and read by skipping when declared") {
    val topic = topicOf(s"okay-source-txn-${System.nanoTime()}", 1)
    val p = producer("transactional.id" -> s"$topic-w", "enable.idempotence" -> "true")
    p.initTransactions()
    for t <- 0 until 3 do
      p.beginTransaction()
      for i <- 0 until 100 do p.send(ProducerRecord(topic, 0, bytes(i), bytes(t * 100L + i))): Unit
      p.commitTransaction()
    p.close()
    val strict = KafkaSource.spans(bootstrap, topic)
    val refused = intercept[IllegalStateException](Flows.collect(KafkaSource.flow(strict, 1)).runWith)
    assert(refused.getMessage.contains("not contiguous"), refused.getMessage)

    val loose = KafkaSource.spans(bootstrap, topic, contiguous = false)
    assertEquals(Flows.collect(KafkaSource.flow(loose, 1)).runWith.length, 300)
    // a position of 150 skips 150 RECORDS, whatever the offsets
    val from150 = Scope.using(sc =>
      okay.Chunks.foldLeft(KafkaSource.partition(loose, 0, 150L, 500, sc))(Vector.empty[KafkaRecord])(_ :+ _))
    assertEquals(from150.map(r => ByteBuffer.wrap(r.value).getLong), (150L until 300L).toVector)
  }

  live("the job's width must be the topic's") {
    val topic = topicOf(s"okay-source-width-${System.nanoTime()}", 3)
    val e = intercept[IllegalArgumentException](KafkaSource.flow(KafkaSource.spans(bootstrap, topic), 4))
    assert(e.getMessage.contains("ARE the topic's"), e.getMessage)
  }
}
