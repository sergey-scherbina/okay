package okay.docs.dynamo

import okay.blob.SigV4
import okay.docs.{Consistency, Docs, DocsSuite, Person}
import java.util.concurrent.atomic.AtomicInteger

/**
 * The SAME contract as TopicDocs and MongoDocs, over DynamoDB — the
 * dockerized dynamodb-local (skips where absent):
 *
 *   docker run -p 8000:8000 amazon/dynamodb-local -jar DynamoDBLocal.jar -inMemory -sharedDb
 */
class TestDynamoDocs extends DocsSuite:

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val endpoint = sys.env.getOrElse("OKAY_DYNAMO", "http://127.0.0.1:8000")

  lazy val available: Boolean =
    try
      val u = java.net.URI(endpoint)
      val s = java.net.Socket()
      s.connect(java.net.InetSocketAddress(u.getHost, u.getPort), 300)
      s.close(); true
    catch case _: Exception => false

  override def munitIgnore: Boolean = !available

  private val n = AtomicInteger(0)

  def mkDocs(): Docs[Person] =
    val d = DynamoDocs[Person](endpoint, "us-east-1", SigV4.Creds("local", "local"),
      s"docs_${System.nanoTime()}_${n.incrementAndGet()}", Person.indexes)
    d.ensure()
    d

  test("grants: One is an eventual read, Quorum is granted Strong (named, not hidden); an eventual reader still sees its own write here") {
    val d = mkDocs()
    assertEquals(d.grants(Consistency.One), Consistency.One)
    assertEquals(d.grants(Consistency.Quorum), Consistency.Strong)
    assertEquals(d.grants(Consistency.Strong), Consistency.Strong)
    val eventual = DynamoDocs[Person](endpoint, "us-east-1", SigV4.Creds("local", "local"),
      s"docs_ev_${System.nanoTime()}", Person.indexes, consistent = false)
    eventual.ensure()
    run(eventual.put("p", Person("ann", "kyiv"))): Unit
    assertEquals(run(eventual.get("p")).map(_.value), Some(Person("ann", "kyiv")))
  }
