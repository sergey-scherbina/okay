package okay.kafka

import okay.!
import okay.given
import KafkaInterop.*
import org.apache.kafka.clients.consumer.{ConsumerRecord, MockConsumer, OffsetResetStrategy}
import org.apache.kafka.clients.producer.{MockProducer, ProducerRecord}
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.StringSerializer
import scala.jdk.CollectionConverters.*

/** Kafka over mocks: one poll = one chunk, commits, resource scoping. */
class TestKafkaInterop extends munit.FunSuite {

  val tp = new TopicPartition("t", 0)

  def mockConsumer(n: Int): MockConsumer[String, String] =
    val c = MockConsumer[String, String](OffsetResetStrategy.EARLIEST)
    c.assign(java.util.List.of(tp))
    c.updateBeginningOffsets(java.util.Map.of(tp, java.lang.Long.valueOf(0L)))
    (0 until n).foreach(i => c.addRecord(ConsumerRecord("t", 0, i.toLong, s"k$i", s"v$i")))
    c

  test("one poll is one chunk; the values arrive in order") {
    val c = mockConsumer(10)
    val s = source(c, pollMillis = 10)
    // take one chunk: the mock returns everything scheduled in one poll
    val first = !.run(okay.Async.run[Chunk1, Nothing](firstChunk(s)))
    assertEquals(first.map(_.value).toList, (0 until 10).map(i => s"v$i").toList)
  }

  type Chunk1 = okay.Chunk[ConsumerRecord[String, String]]

  /** pull exactly one told chunk out of the async source (the source
   * never ends, so `uncons`, not a collect) */
  @scala.annotation.nowarn("msg=cannot be checked at runtime") // E092, Writer.run's caveat
  def firstChunk(s: KafkaChunks[String, String]): Chunk1 ! okay.Async =
    okay.Writer.uncons[Chunk1, Unit, okay.Async](s).map {
      case Right((c, _)) => c
      case Left(_) => fail("no chunk")
    }

  test("commit records the position; a restarted consumer resumes there") {
    val c = mockConsumer(5)
    c.poll(java.time.Duration.ofMillis(10))              // consume all five
    !.run(okay.Async.run[Unit, Nothing](commit(c)))
    assertEquals(c.committed(java.util.Set.of(tp)).get(tp).offset(), 5L)
  }

  test("sink sends a chunk and flushes") {
    val p = MockProducer[String, String](true, StringSerializer(), StringSerializer())
    val records = okay.ChunkBuf.of(
      (1 to 3).map(i => ProducerRecord[String, String]("t", s"k$i", s"v$i")))
    !.run(okay.Async.run[Unit, Nothing](sink(p)(records)))
    assertEquals(p.history().asScala.map(_.value()).toList, List("v1", "v2", "v3"))
  }

  test("managed consumer closes with its Resource scope") {
    val c = MockConsumer[String, String](OffsetResetStrategy.EARLIEST)
    val prog = okay.Resource.acquire(c)(_.close()).map(_ => ())
    !.run(okay.Resource.run[Unit, Nothing](prog))
    assert(c.closed())
  }
}
