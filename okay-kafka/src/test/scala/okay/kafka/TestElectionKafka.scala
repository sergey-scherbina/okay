package okay.kafka

import okay.persist.{ElectionSuite, Topic}

/** the battery over KAFKA as the control log — the recommended
 * first road of specs/consensus.md: election delegated to the
 * engine whose KRaft did the twenty years. Live; skips absent. */
class TestElectionKafka extends ElectionSuite:

  val bootstrap = sys.env.getOrElse("OKAY_KAFKA", "127.0.0.1:9092")

  lazy val store: Option[KafkaStore] =
    if !TestKafkaSupport.reachable(bootstrap) then None
    else
      try
        val s = KafkaStore(bootstrap)
        s.topics
        Some(s)
      catch case _: Throwable => None

  def mkControl(): Topic =
    assume(store.isDefined, s"no Kafka at $bootstrap — the live battery skips")
    store.get.topic(s"okay-control-${System.nanoTime()}")

  override def afterAll(): Unit = store.foreach(_.close())
