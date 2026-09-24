package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE SECOND MACHINE IS A COMPILE ERROR (delim-safety stage 0,
 * 2026-09-17). delim-nesting gave the nested forms; this is what
 * stops the un-nested one being written. The limit is pinned too: an
 * abstract row still compiles, because `NotGiven` reads "unknown" as
 * "absent" — a guard against the shape people write, not a proof.
 */
class TestDelimSafety extends munit.FunSuite {

  type P = okay.Pure

  test("collect inside a Delim row does not compile, and the message names `collecting`") {
    val e = compileErrors("""
      okay.Delim.collect[Int, okay.Delim + okay.Pure](okay.Direct.direct {
        !okay.Delim.emit(1)
      })""")
    assert(e.nonEmpty, "a second machine compiled")
    assert(e.contains("collecting"), s"the message does not name the fix: $e")
    assert(e.contains("SECOND machine"), s"the message does not say what is wrong: $e")
  }

  test("the same guard on delimited, resumable, reset and run") {
    assert(compileErrors(
      "okay.Delim.delimited[Int, okay.Delim + okay.Pure](okay.pure(1))").nonEmpty)
    assert(compileErrors(
      "okay.Delim.reset[Int, okay.Delim + okay.Pure](_ => okay.pure(1))").nonEmpty)
    assert(compileErrors(
      "okay.Delim.run[Int, okay.Delim + okay.Pure](okay.pure(1))").nonEmpty)
    assert(compileErrors("""
      okay.Delim.resumable[String, Int, Int, okay.Delim + okay.Pure](okay.pure(1))""").nonEmpty)
  }

  test("the nested forms still compile in a Delim row — that is what they are for") {
    // exactly the shape the guard refuses above, spelled the right way
    def half(using Delim.Asking[String, Int, List[Int], Delim + P]): List[Int] ! Delim + P =
      Delim.collecting[Int, P]:
        direct:
          !Delim.emit(1)
          !Delim.emit(!Delim.pause("more?"))
    val start = !.run(Delim.resumable[String, Int, List[Int], P](half))
    assertEquals(!.run(Delim.drive(start)(_ => okay.pure(2))), List(1, 2))
  }

}
