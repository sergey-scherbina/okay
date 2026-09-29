package okay.zio

import okay.{!, %, +, Async, Reader, Throws, runEither}
import okay.Direct.*
import okay.given
import okay.zio.given
import _root_.zio.{ZEnvironment, ZIO}

/** specs/direct-foreign-mark.md: ZIO values marked inside direct blocks
 * over okay programs, each onto the narrowest row its type allows */
class TestZioForeign extends munit.FunSuite:

  final case class Greeting(word: String)

  test("a Task is one Async operation") {
    val p: Int ! Async = direct {
      val a = ZIO.attempt(20).?
      val b = ZIO.succeed(22).reflect
      a + b
    }
    assertEquals(p.runWith, 42)
  }

  test("a typed ZIO error raises Throws") {
    type Row = Throws % String + Async
    val nope: ZIO[Any, String, Int] = ZIO.fail("nope")
    val p: Int ! Row = direct {
      val a = nope.?
      a + 1
    }
    assertEquals(runEither[Int, Async, String](p).runWith, Left("nope"))
  }

  test("a ZIO with an environment reads the Reader") {
    type Row = Reader % ZEnvironment[Greeting] + Throws % String + Async
    val word: ZIO[Greeting, String, String] = ZIO.serviceWith[Greeting](_.word)
    val p: String ! Row = direct {
      val w = word.?
      w + "!"
    }
    val env = ZEnvironment(Greeting("hi"))
    assertEquals(runEither[String, Async, String](
      Reader.run[ZEnvironment[Greeting], String, Throws % String + Async](env)(p)).runWith, Right("hi!"))
  }

  test("a typed ZIO in a row without its Throws is refused") {
    val e = compileErrors("""
      import okay.Direct.*
      import okay.zio.given
      val nope: _root_.zio.ZIO[Any, String, Int] = _root_.zio.ZIO.fail("nope")
      val p: Int ! okay.Async = direct { nope.? }
    """)
    assert(e.contains("neither this block"), e)
  }
