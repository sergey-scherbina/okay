package okay.pool

/**
 * A REAL CONSUL SESSION LOCK, OVER A REAL AGENT (specs/cluster-pool.md,
 * stage 5) — a real `consul` container (`docker run consul agent -dev`),
 * `Live`-tagged for the same reason every docker-dependent suite here
 * is: even a present service can flake on its own timing, and a
 * landing's gate must not depend on it. Skipped where port 18500
 * answers nothing.
 *
 * Consul's own minimum session TTL is 10s, so the expiry test below
 * waits past that plus its own check interval rather than a shorter
 * number this suite might wish for.
 */
object TestConsulLease:
  val base = "http://127.0.0.1:18500"
  lazy val up: Boolean =
    try
      val c = java.net.Socket()
      c.connect(java.net.InetSocketAddress("127.0.0.1", 18500), 300)
      c.close()
      true
    catch case _: Exception => false

class TestConsulLease extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !TestConsulLease.up

  private def freshKey(): String = s"okay-test-${java.util.UUID.randomUUID()}"

  test("take() creates a session, acquires the KV lock, and answers a fencing term") {
    val key = freshKey()
    val l = ConsulLease(TestConsulLease.base, key, "holder-a")
    try assert(l.take().isDefined)
    finally l.release(0)
  }

  test("a SECOND holder cannot take a lock the first one already holds") {
    val key = freshKey()
    val a = ConsulLease(TestConsulLease.base, key, "holder-a")
    val b = ConsulLease(TestConsulLease.base, key, "holder-b")
    try
      assert(a.take().isDefined)
      assertEquals(b.take(), None)
    finally a.release(0)
  }

  test("held() renews the session and keeps answering true while it is not destroyed") {
    val key = freshKey()
    val l = ConsulLease(TestConsulLease.base, key, "holder-a", ttlSeconds = 10)
    try
      val term = l.take().get
      assert(l.held(term))
      assert(l.held(term))
    finally l.release(0)
  }

  test("held() answers false once release() has destroyed the session") {
    val key = freshKey()
    val l = ConsulLease(TestConsulLease.base, key, "holder-a")
    val term = l.take().get
    l.release(term)
    assert(!l.held(term))
  }

  test("release() frees the lock, so a DIFFERENT holder can take it right after") {
    val key = freshKey()
    val a = ConsulLease(TestConsulLease.base, key, "holder-a")
    a.release(a.take().get)
    val b = ConsulLease(TestConsulLease.base, key, "holder-b")
    val term = b.take()
    try assert(term.isDefined, term.toString)
    finally b.release(0)
  }

  test("the fencing token (LockIndex) rises on every fresh acquisition of the same key") {
    val key = freshKey()
    val a = ConsulLease(TestConsulLease.base, key, "holder-a")
    val first = a.take().get
    a.release(first)
    val b = ConsulLease(TestConsulLease.base, key, "holder-b")
    val second = b.take().get
    try assert(second > first, s"$second was not greater than $first")
    finally b.release(second)
  }
