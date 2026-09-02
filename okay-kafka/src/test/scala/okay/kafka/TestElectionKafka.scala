package okay.kafka

import okay.persist.{ElectionSuite, Topic}

/** the battery over KAFKA as the control log — the recommended
 * first road of specs/consensus.md: election delegated to the
 * engine whose KRaft did the twenty years. Live; skips absent. */
class TestElectionKafka extends ElectionSuite:

  // the live-check probe itself can stretch well past its own
  // 1s socket deadline under a loaded sbt test matrix (found
  // 2026-09-02, BACKLOG matrix-flake fifth sighting) — the three
  // sibling Kafka suites already carry this override; this one
  // didn't, so a slow-but-correct skip read as a hard 30s failure
  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(120, "s")

  val bootstrap = sys.env.getOrElse("OKAY_KAFKA", "127.0.0.1:9092")

  lazy val store: Option[KafkaStore] =
    if !TestKafkaSupport.reachable(bootstrap) then None
    else
      try
        val s = KafkaStore(bootstrap)
        s.topics: Unit
        Some(s)
      catch case _: Throwable => None

  def mkControl(): Topic =
    assume(store.isDefined, s"no Kafka at $bootstrap — the live battery skips")
    store.get.topic(s"okay-control-${System.nanoTime()}")

  override def afterAll(): Unit = store.foreach(_.close())
