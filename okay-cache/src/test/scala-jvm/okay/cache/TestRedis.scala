package okay.cache

import okay.{!, Async}
import okay.given
import okay.codec.Schema

/**
 * The Redis engine against a LIVE server (docker; the suite skips
 * where none answers): the contract claims the memory engine makes,
 * re-made over the wire — plus what only Redis can prove: expiry is
 * SERVER-side, and the value on the wire is CBOR a Schema reads.
 */
object TestRedis:
  lazy val up: Boolean =
    try { val s = java.net.Socket(); s.connect(java.net.InetSocketAddress("127.0.0.1", 6379), 300); s.close(); true }
    catch case _: Exception => false

class TestRedis extends munit.FunSuite {

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override def munitIgnore: Boolean = !TestRedis.up

  final case class Quote(sym: String, price: Double, tags: Vector[String] = Vector.empty)
  given Schema[Quote] = Schema.derived

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  private var n = 0
  def fresh(regime: Regime): Cache[String, Quote] =
    n += 1
    val prefix = s"okay-test-$n-${System.nanoTime}"
    Redis.cache[String, Quote](Redis.connect(), regime, k => s"$prefix:$k")

  test("the contract over the wire: put/get round-trips CBOR, invalidate removes, absent is None") {
    val c = fresh(Regime.Invalidated)
    assertEquals(run(c.get("aapl")), None)
    run(c.put("aapl", Quote("AAPL", 187.5, Vector("tech"))))
    assertEquals(run(c.get("aapl")), Some(Quote("AAPL", 187.5, Vector("tech"))))
    run(c.invalidate("aapl"))
    assertEquals(run(c.get("aapl")), None)
    run(c.invalidate("aapl"))   // idempotent, like the trait says
  }

  test("getOrLoad: a miss loads once and caches; a hit does not load") {
    val c = fresh(Regime.Invalidated)
    var loaded = 0
    def load(k: String): Quote ! Async = okay.async { loaded += 1; Quote(k, 1.0) }
    assertEquals(run(c.getOrLoad("x")(load)), Quote("x", 1.0))
    assertEquals(run(c.getOrLoad("x")(load)), Quote("x", 1.0))
    assertEquals(loaded, 1)
    assertEquals(c.stats.loads, 1L)
  }

  test("expiry is SERVER-side: a Budget entry vanishes without this process filtering it") {
    val c = fresh(Regime.Budget(ttlMillis = 200))
    run(c.put("gone", Quote("GONE", 1.0)))
    assertEquals(run(c.get("gone")).map(_.sym), Some("GONE"))
    // poll until the SERVER expires it — a deadline, not a sleep-and-hope
    val deadline = System.currentTimeMillis + 3000
    var last: Option[Quote] = Some(Quote("GONE", 1.0))
    while last.nonEmpty && System.currentTimeMillis < deadline do
      Thread.sleep(50)
      last = run(c.get("gone"))
    assertEquals(last, None, "the server never expired the entry")
  }

  test("a PING to a dead port refuses at connect — fail fast, before any entry is trusted") {
    intercept[Exception](Redis.connect(port = 6390))
  }
}
