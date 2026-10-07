package okay


import okay.freer.*
import okay.freer.given
class TestAmbient extends munit.FunSuite:

  test("Ambient.clock answers the wall clock; Ambient.random draws differ"):
    val before = System.currentTimeMillis()
    val t = !.run(Ambient.clock.run(Clock.now))
    assert(t >= before && t <= System.currentTimeMillis() + 1000)
    val (a, b) = !.run(Ambient.random.run(for a <- Random.nextLong; b <- Random.nextLong yield (a, b)))
    assertNotEquals(a, b)

  test("Ambient.uid() is unique and sorts by issue time; Ambient.stamp() never goes backwards"):
    val ids = Vector.fill(1000)(Ambient.uid())
    assertEquals(ids.distinct.size, 1000)
    assertEquals(ids.map(_.ulid), ids.map(_.ulid).sorted)
    val s1 = Ambient.stamp(); val s2 = Ambient.stamp()
    assert(s2.millis > s1.millis || (s2.millis == s1.millis && s2.counter > s1.counter))
