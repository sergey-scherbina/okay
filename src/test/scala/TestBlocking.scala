import okay.*
import okay.given

/** Blocking[A]: the parks-a-thread requirement as a stored VALUE,
 * forced only where a CanBlock is given (ambient on the JVM) */
class TestBlocking extends munit.FunSuite {
  test("a stored Blocking value forces where the capability is given") {
    val work: Blocking[Int] = 40 + summon[CanBlock].hashCode * 0 + 2
    val kept: Vector[Blocking[Int]] = Vector(work, work)   // storable, composable
    assertEquals(kept.map(b => b: Int).sum, 84)            // the JVM given forces in place
  }
}
