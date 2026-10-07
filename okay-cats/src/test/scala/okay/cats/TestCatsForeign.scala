package okay.cats

import okay.Async
import okay.freer.{!}
import okay.Direct.*
import okay.given
import okay.freer.given
import okay.std.given
import okay.cats.given
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global

/** specs/direct-foreign-mark.md: a cats IO marked inside a direct block */
class TestCatsForeign extends munit.FunSuite:

  test("an IO is marked with .?, .reflect and ! inside a block over Async") {
    val p: Int ! Async = direct {
      val a = IO(20).?
      val b = IO.pure(2).reflect
      val c = !IO(20)
      a + b + c
    }
    assertEquals(p.runWith, 42)
  }

  test("a failed IO fails the program with the same throwable") {
    val boom = RuntimeException("boom")
    val p: Int ! Async = direct { IO.raiseError[Int](boom).? + 1 }
    assertEquals(intercept[RuntimeException](p.runWith), boom)
  }
