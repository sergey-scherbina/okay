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

  lazy val availableClient: Option[com.mongodb.client.MongoClient] =
    try
      val c = MongoDocs.client(uri)
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
