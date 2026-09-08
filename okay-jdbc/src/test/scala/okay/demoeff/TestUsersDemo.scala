package okay.demoeff

import okay.*
import okay.given
import UsersDemo.{Store, InMemory, rename, tracked}

/**
 * The demo's claims, as assertions: one program, several
 * interpretations, and a `Store` that is not tied to `Map`.
 */
class TestUsersDemo extends munit.FunSuite {

  def pureRun[S : Store](init: S): (S, (Seq[String], Option[String])) =
    State.run[S, (Seq[String], Option[String])](init)(
      Writer.run[String, Option[String], State % S](
        tracked[Option[String], S, Pure](rename(7L, "grace"))))

  test("two carriers, one interpreter: the answer and the log do not depend on the store") {
    val (asMap, mapOut) = pureRun(Map(7L -> "ada"))
    val (asVec, vecOut) = pureRun(Vector(7L -> "ada"))
    assertEquals(mapOut, vecOut)
    assertEquals(mapOut, (Seq("Find(7)", "Save(7,grace)"), Some("ada")))
    // only the representation differs
    assertEquals(asMap, Map(7L -> "grace"))
    assertEquals(asVec, Vector(7L -> "grace"))
  }

  test("a store answers its successor: put then get, on both carriers") {
    def law[S](empty: S)(using St: Store[S]): Unit =
      assertEquals(St.get(1L)(empty), None)
      assertEquals(St.get(1L)(St.put(1L, "ada")(empty)), Some("ada"))
      assertEquals(St.get(1L)(St.put(1L, "grace")(St.put(1L, "ada")(empty))), Some("grace"))
    law(Map.empty[Long, String])
    law(Vector.empty[(Long, String)])
  }

  test("the handler world agrees with the pure one") {
    val mem = InMemory(Map(7L -> "ada"))
    val log = scala.collection.mutable.ListBuffer[Any]()
    assertEquals(rename(7L, "grace").runWith(using mem.tracing(log += _)), Some("ada"))
    assertEquals(mem.state, Map(7L -> "grace"))
    assertEquals(log.map(_.toString).toSeq, Seq("Find(7)", "Save(7,grace)"))
    // a missing id: a find, no save, nothing written
    val miss = InMemory(Map.empty[Long, String])
    assertEquals(rename(99L, "hopper").runWith(using miss), None)
    assertEquals(miss.state, Map.empty[Long, String])
  }
}
