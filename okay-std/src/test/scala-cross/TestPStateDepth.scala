package okay.freer



import okay.std.*
import okay.std.given
/**
 * PState's function answer on EVERY platform (cont-fun-answer): a hundred thousand `get`/`set` steps, past
 * Scala.js's engine stack (~10 800 frames on Node's default), where no stack switch exists.
 */
class TestPStateDepth extends munit.FunSuite:

  def steps(n: Int): Cps[Long, Long => (Long, Long), Long => (Long, Long)] =
    (1 to n).foldLeft(PState.get[Long, (Long, Long)])((m, _) => m.flatMap(_ => PState.get.flatMap(s => PState.set(s + 1))))

  test("a hundred thousand PState steps") {
    assertEquals(PState.run(0L)(steps(100000)), (100000L, 99999L))
  }

  test("the answer stays a function: applied twice, it runs twice from each state") {
    val f = steps(3) / (a => (s2: Long) => (s2, a))
    assertEquals(f(0L), (3L, 2L))
    assertEquals(f(10L), (13L, 12L))
  }

  test("set changes the state's type, and a body written with the helper keeps its meaning") {
    val p = PState.get[Int, (String, Int)].flatMap(i => PState.set[Int, String, (String, Int)](s"n=$i").map(_ => i))
    assertEquals(PState.run(7)(p), ("n=7", 7))
    assertEquals((Cps.shift[Int, Int => Int, Int => Int](k => (s: Int) => k(s + 1)(s * 2)) / (a => (s: Int) => a + s))(3), 10)
  }
