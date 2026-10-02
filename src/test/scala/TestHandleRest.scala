package okay

/** handle-values-rest: every ready effect's handler as a value */
class TestHandleRest extends munit.FunSuite:

  test("Once.memo: a once runs at most once") {
    val p: Int ! Once = Once.once[Int, Pure](pure(21)).map(_ * 2)
    assertEquals(p.handle(Once.memo).run, Once.run(p).run)
  }

  test("Resource.region: released at the end") {
    var released = false
    val p: Int ! Resource = Resource.acquire(41)(_ => released = true).map(_ + 1)
    assertEquals(p.handle(Resource.region).run, 42)
    assert(released)
  }

  test("Fresh.counter and Supply.from") {
    val f: (Long, Long) ! Fresh = Fresh.next.flatMap(a => Fresh.next.map(b => (a, b)))
    assertEquals(f.handle(Fresh.counter).run, (0L, 1L))
    val s: Int ! Supply % Int = Supply.next[Int].flatMap(a => Supply.next[Int].map(b => a + b))
    assertEquals(s.handle(Supply.from(10)(_ + 1)).run, (12, 21))
  }

  test("Prob.exact and Chronicle.verdict") {
    val coin: Boolean ! Dist = Prob.dist(true -> 0.5, false -> 0.5)
    assertEquals(coin.handle(Prob.exact).run, Map(true -> 0.5, false -> 0.5))
    val c: Int ! Chronicle % String = Chronicle.dictate("careful").map(_ => 1)
    assertEquals(c.handle(Chronicle.verdict).run, Chronicle.run[String, Int, Pure](c).run)
  }
