package okay

import okay.given
import okay.!.*
import okay.Direct.*

/**
 * The E20 pattern (specs/context-functions.md): the door OUTSIDE,
 * the direct block INSIDE. The capability arc answers "what is
 * available" (Env via wire/provide), the direct arc answers "how it
 * reads" (bare statements, no for) — three layers peeled by three
 * different machines: provide by the compiler, the block by the
 * macro, the row by handlers. None knows of the others.
 */
class TestDirectDoors extends munit.FunSuite {

  case class Env(user: String, uid: Int)

  def told: Env ?=> Int ! (Writer % String) = direct {
    Writer(s"hello ${wire[Env].user}")   // bare statement: do-notation
    Writer("bye")
    wire[Env].uid                        // the door, inside the block
  }

  test("door outside, direct inside: three layers, three machines") {
    val (log, uid) = provide(Env("ada", 7)):
      !.run(Writer.run[String, Int, okay.Pure](told))
    assertEquals(log, Vector("hello ada", "bye"))
    assertEquals(uid, 7)
  }

  test("the same program, another environment — the DI claim survives the block") {
    val (log, uid) = provide(Env("bob", 1)):
      !.run(Writer.run[String, Int, okay.Pure](told))
    assertEquals(log, Vector("hello bob", "bye"))
    assertEquals(uid, 1)
  }

  test("providing composes over the block too") {
    val base = providing[Env](Env("ada", 7))
    val (_, uid) = (base and providing[Env](Env("eve", 9))) {
      !.run(Writer.run[String, Int, okay.Pure](told))
    }
    assertEquals(uid, 9)
  }
}
