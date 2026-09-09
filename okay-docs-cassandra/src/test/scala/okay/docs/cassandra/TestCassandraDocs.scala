package okay.docs.cassandra

import okay.docs.{Consistency, Docs, DocsSuite, Person}
import java.util.concurrent.atomic.AtomicInteger

/**
 * The SAME contract as TopicDocs, MongoDocs and DynamoDocs, over a
 * real Cassandra — the dockerized node (skips where absent):
 *
 *   docker run -p 9042:9042 cassandra:5
 */
class TestCassandraDocs extends DocsSuite:

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(120, "s")

  val host = sys.env.getOrElse("OKAY_CASSANDRA_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_CASSANDRA_PORT").flatMap(_.toIntOption).getOrElse(9042)

  lazy val session: Option[com.datastax.oss.driver.api.core.CqlSession] =
    try
      val s = java.net.Socket()
      s.connect(java.net.InetSocketAddress(host, port), 300); s.close()
      Some(CassandraDocs.session(host, port, "datacenter1", "okay_test"))
    catch case _: Throwable => None

  override def munitIgnore: Boolean = session.isEmpty

  private val n = AtomicInteger(0)

  def mkDocs(): Docs[Person] =
    val d = CassandraDocs[Person](session.get, "okay_test", s"docs_${System.nanoTime() % 1000000}_${n.incrementAndGet()}", Person.indexes)
    d.ensure()
    d

  test("grants: the dial is granted as asked — One, Quorum, Strong (ALL); a Quorum reader sees its own write on one node") {
    val d = mkDocs()
    assertEquals(d.grants(Consistency.One), Consistency.One)
    assertEquals(d.grants(Consistency.Quorum), Consistency.Quorum)
    assertEquals(d.grants(Consistency.Strong), Consistency.Strong)
    run(d.put("p", Person("ann", "kyiv"))): Unit
    assertEquals(run(d.get("p")).map(_.value), Some(Person("ann", "kyiv")))
  }

  override def afterAll(): Unit =
    session.foreach { s =>
      try s.execute("DROP KEYSPACE IF EXISTS okay_test"): Unit finally s.close()
    }
