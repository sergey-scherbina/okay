package okay.foreign

/**
 * A worker over pipes that never says hello (foreign-pipe-hello-timeout):
 * refused by name within the limit, its process stopped — not a caller
 * blocked in a read for ever. Seen twice on 2026-09-26, when R's docker shim
 * started containers that never printed the handshake under load 32–35.
 */
class TestPipeHello extends munit.FunSuite:
  test("a worker that says nothing is refused within the limit, and its process stopped") {
    val p = ProcessBuilder("sh", "-c", "sleep 30; echo late").start()
    val t0 = System.nanoTime()
    val e = intercept[IllegalStateException](ForeignWorker.over(WireLink.pipes(p, helloMillis = 500), "the silent worker"))
    val ms = (System.nanoTime() - t0) / 1e6
    assert(ms < 10000, s"refused only after $ms ms")
    assert(e.getMessage.contains("said nothing for 500ms"), e.getMessage)
    assert(p.waitFor(5, java.util.concurrent.TimeUnit.SECONDS) && !p.isAlive, "the silent process was left running")
  }

  test("a worker that answers in time is served as before") {
    val p = ProcessBuilder("sh", "-c", "echo hi; cat").start()
    val link = WireLink.pipes(p, helloMillis = 5000)
    try assertEquals(link.hello(), Some("hi"))
    finally link.close()
  }
