package okay.kafka

import okay.cluster.StagingTopicSuite
import okay.persist.{Policy, Topic}

/**
 * EXACTLY-ONCE FROM LOG TO LOG, ON A REAL BROKER — the last box of
 * specs/dataflow.md stage 11, and the one that had been waiting on
 * hardware rather than on a decision.
 *
 * It asserts NOTHING NEW. `StagingTopicSuite` is the battery
 * okay-cluster already runs against a `MemoryStore` topic: a quiet
 * run writes every pane once; a coordinator that dies between the
 * append and the journal commit is succeeded by a process with NO
 * memory of its own, which learns what landed by reading the output's
 * tail; a death after the commit appends nothing again. This suite
 * supplies a Kafka topic and runs the same four.
 *
 * WHY THAT IS THE WHOLE TEST. The dedup state IS the output, so the
 * only thing a real log can break that a memory one cannot is the
 * reading back — offsets, `TooEarly`, a tail that is eventually
 * consistent. Everything else was proved in-process, and re-proving
 * it here would be re-running the same code against a slower store.
 *
 * LIVE: it skips in milliseconds when nothing is listening on 9092
 * (`TestKafkaSupport.reachable`, ahead of the Kafka client's own
 * generous timeouts), and it is out of the default gate into
 * `sbt integrationTest` like every suite that reaches outside the JVM.
 */
class TestDataflowKafka extends StagingTopicSuite:

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[munit.Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))

  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(300, "s")

  val bootstrap: String = sys.env.getOrElse("OKAY_KAFKA", "127.0.0.1:9092")

  private lazy val store: Option[KafkaStore] =
    if TestKafkaSupport.reachable(bootstrap) then Some(KafkaStore(bootstrap)) else None

  /** ONE PARTITION, like the memory battery: the writer keys its
   * appends by epoch and reads the whole tail back, so a second
   * partition would only add ordering questions this box is not
   * about */
  def output(): Topic =
    store match
      case Some(s) => s.topic(s"okay-dataflow-eos-${System.nanoTime()}", 1, Policy(compact = false))
      case None =>
        // a skip, not a failure: the box needs a broker and says so
        assume(false, s"no Kafka on $bootstrap — start one (docker run -p 9092:9092 apache/kafka:3.9.0)")
        throw IllegalStateException("unreachable")
