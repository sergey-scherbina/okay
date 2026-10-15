package okay.std.tests

import okay.*
import okay.std.*

class TestClockRandom extends munit.FunSuite:

  test("Clock.fixed answers every now with the same reading; ticking moves by step and answers the next reading"):
    val twice = for a <- Clock.now; b <- Clock.now yield (a, b)
    assertEquals(Clock.run(5L)(twice).run, (5L, 5L))
    assertEquals(twice.handle(Clock.ticking(10L, 3L)).run, (16L, (10L, 13L)))

  test("Clock.at reads the source on every now — the only door to a real clock"):
    var t = 0L
    val p = for a <- Clock.now; _ = { t = 100L }; b <- Clock.now yield (a, b)
    assertEquals(p.handle(Clock.at(() => t)).run, (0L, 100L))

  test("Random.seeded is repeatable, differs by seed, and answers the next seed"):
    val three = for a <- Random.nextLong; b <- Random.nextLong; c <- Random.nextLong yield List(a, b, c)
    val (s1, x1) = three.handle(Random.seeded(42L)).run
    val (s2, x2) = three.handle(Random.seeded(42L)).run
    assertEquals(x1, x2); assertEquals(s1, s2)
    assertNotEquals(x1, Random.run(43L)(three).run)
    assertEquals(x1.distinct.size, 3)

  test("nextDouble is in [0, 1) and nextInt(bound) in [0, bound), over a thousand draws"):
    val ds = Random.run(1L)(List.fill(1000)(Random.nextDouble).foldLeft(pure[List[Double], Random](Nil))((acc, d) => acc.flatMap(l => d.map(_ :: l)))).run
    assert(ds.forall(d => d >= 0.0 && d < 1.0))
    val is = Random.run(2L)(List.fill(1000)(Random.nextInt(7)).foldLeft(pure[List[Int], Random](Nil))((acc, d) => acc.flatMap(l => d.map(_ :: l)))).run
    assertEquals(is.toSet, (0 until 7).toSet)

  test("Random.at reads the source once per draw"):
    var n = 0L
    val two = for a <- Random.nextLong; b <- Random.nextLong yield (a, b)
    assertEquals(two.handle(Random.at(() => { n += 1; n })).run, (1L, 2L))
