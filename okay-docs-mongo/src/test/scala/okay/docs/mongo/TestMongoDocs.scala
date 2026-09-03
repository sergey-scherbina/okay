package okay.docs.mongo

import okay.docs.{Docs, DocsSuite, Person}
import java.util.concurrent.atomic.AtomicInteger

/**
 * The SAME contract as the own engine, over a real Mongo (live
 * pattern: skips where the dockerized node is absent) — which is
 * the whole claim of the seam: one contract, many engines.
 */
class TestMongoDocs extends DocsSuite:

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  // the availability probe's own 1.5s driver deadline can still
  // stretch well past 30s under a loaded sbt test matrix (found
  // 2026-09-02, BACKLOG matrix-flake fifth sighting; matches the
  // same override the kafka live suites already carry) — without
  // it, a slow-but-correct skip reads as a hard 30s failure
  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(120, "s")

  val uri = sys.env.getOrElse("OKAY_MONGO", "mongodb://127.0.0.1:27017")

  /** the DRIVER's own defaults make "nothing is listening" a 30s
   * failure (`serverSelectionTimeoutMS` default) — found 2026-09-02
   * when the live suite hung to that instead of skipping in
   * milliseconds. A short deadline on the PROBE alone (the
   * production `MongoDocs.client` keeps its generous retry policy;
   * only this availability check needs to be fast) answers "is
   * anything here" honestly and quickly either way. */
  lazy val availableClient: Option[com.mongodb.client.MongoClient] =
    try
      val settings = com.mongodb.MongoClientSettings.builder()
        .applyConnectionString(com.mongodb.ConnectionString(uri))
        .applyToClusterSettings(b => { b.serverSelectionTimeout(1500, java.util.concurrent.TimeUnit.MILLISECONDS): Unit })
        .applyToSocketSettings(b => { b.connectTimeout(1500, java.util.concurrent.TimeUnit.MILLISECONDS): Unit })
        .build()
      val c = com.mongodb.client.MongoClients.create(settings)
      c.listDatabaseNames().first() // forces a round-trip
      Some(c)
    catch case _: Throwable => None

  private val n = AtomicInteger(0)

  def mkDocs(): Docs[Person] =
    assume(availableClient.isDefined, s"no Mongo at $uri — the live suite skips")
    MongoDocs[Person](availableClient.get, "okay_test",
      s"docs_${System.nanoTime()}_${n.incrementAndGet()}", Person.indexes)

  override def afterAll(): Unit =
    availableClient.foreach { c =>
      try c.getDatabase("okay_test").drop() finally c.close()
    }
