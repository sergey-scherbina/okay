package okay2

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

/** TestTRef's two tests that start REAL threads: JVM only
 * (okay2-cross — Scala.js has one thread, Scala Native's are not what
 * these measure) */
class TestTRefThreads extends munit.FunSuite {

  test("many threads, one cell: every increment lands exactly once (the CAS retries a lost race)") {
    val r = TRef(0)
    val pool = Executors.newFixedThreadPool(8)
    val done = new CountDownLatch(8)
    for (_ <- 1 to 8) pool.execute(() => { for (_ <- 1 to 10000) r.modify(n => (n + 1, ())); done.countDown() })
    assert(done.await(30, TimeUnit.SECONDS), "the workers did not finish")
    pool.shutdown()
    assertEquals(r.get, 80000)
    assertEquals(r.version, 80000L)
  }

  test("TDict and TList: one modify per operation, safe from many threads") {
    val d = TDict.empty[String, Int]
    val l = TList.empty[Int]
    val pool = Executors.newFixedThreadPool(4)
    val done = new CountDownLatch(4)
    for (t <- 1 to 4) pool.execute(() => {
      for (i <- 1 to 1000) { d.updateAt("n")(o => o.getOrElse(0) + 1); l.append(t * 10000 + i) }
      done.countDown()
    })
    assert(done.await(30, TimeUnit.SECONDS))
    pool.shutdown()
    assertEquals(d.get("n"), Some(4000))
    assertEquals(l.size, 4000)
    assertEquals(d.computeIfAbsent("m")(7), 7)
    assertEquals(d.computeIfAbsent("m")(8), 7)
    d.remove("m")
    assert(!d.contains("m"))
  }
}
