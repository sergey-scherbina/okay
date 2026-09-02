package okay.docs.mongo

import okay.docs.{Docs, DocsSuite, Person}
import java.util.concurrent.atomic.AtomicInteger

/**
 * The SAME contract as the own engine, over a real Mongo (live
 * pattern: skips where the dockerized node is absent) — which is
 * the whole claim of the seam: one contract, many engines.
 */
class TestMongoDocs extends DocsSuite:

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
        .applyToClusterSettings(_.serverSelectionTimeout(1500, java.util.concurrent.TimeUnit.MILLISECONDS))
        .applyToSocketSettings(_.connectTimeout(1500, java.util.concurrent.TimeUnit.MILLISECONDS))
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
