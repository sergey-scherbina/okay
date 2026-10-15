package okay.std.tests

import okay.std.*

/** PState, the type-changing state (its shift road okay-core's `Cont` (`okay.Cps`), its data road okay-freer's indexed `Freer`) */
class TestPState extends munit.FunSuite {

  test("PState: type-changing state, Int -> String -> Boolean") {
    val r = PState.run(41):
      for
        n <- PState.get                    // n: Int
        _ <- PState.set((n + 1).toString)  // the state is a String now
        s <- PState.get                    // s: String
        _ <- PState.set(s.length == 2)     // the state is a Boolean now
      yield s + "!"
    assertEquals(r, (true, "42!"))
  }

  test("PState.Threaded: the same protocol as data, the state threaded by the loop with its type moving") {
    import PState.Threaded.{get, put}
    val r = PState.Threaded.run(
      for
        n <- get[Int]                          // n: Int
        _ <- put[Int, String]((n + 1).toString) // the state is a String now
        s <- get[String]                       // s: String
        old <- put[String, Boolean](s.length == 2) // the old state is the answer, as set's is
      yield s + "!" + old)(41)
    assertEquals(r, (true, "42!42"))
  }

  test("PState.Threaded.get is ONE shared node for every state type") {
    assert(PState.Threaded.get[Int] eq PState.Threaded.get[String])
    assert(PState.Threaded.get[Int] eq SharedOps.getT)
  }

  test("PState.Threaded: a Put from a state the program is not in does not type") {
    val errors = compileErrors("PState.Threaded.get[Int].flatMap(n => PState.Threaded.put[String, Int](n))")
    assert(errors.nonEmpty, "String put after an Int get must be refused")
  }
}
