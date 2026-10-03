package okay2

/**
 * PState's function answer on EVERY platform (okay2-cont-fun-answer, the Scala 3 core's cont-fun-answer): a
 * hundred thousand `get`/`set` steps, past Scala.js's engine stack, where no stack switch exists.
 */
class TestPStateDepth extends munit.FunSuite {

  type R = (Long, Long)

  def steps(n: Int): Cont[Long, Long => R, Long => R] =
    (1 to n).foldLeft(PState.get[Long, R])((m, _) =>
      m.flatMap(_ => PState.get[Long, R].flatMap(s => PState.set[Long, Long, R](s + 1))))

  test("a hundred thousand PState steps") {
    assertEquals(PState.run[Long, Long, Long](0L)(steps(100000)), (100000L, 99999L))
  }

  test("the answer stays a function: applied twice, it runs twice from each state") {
    val f = steps(3) / ((a: Long) => (s2: Long) => (s2, a))
    assertEquals(f(0L), (3L, 2L))
    assertEquals(f(10L), (13L, 12L))
  }
}
