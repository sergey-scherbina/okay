package okay


import okay.std.*
import okay.Direct.*

/**
 * Reader elimination (specs/context-functions.md, ctx-reader-bridge
 * — gate lifted): the row's environment moves into a using
 * parameter; the elaborator runs the Reader half at compile time,
 * the direct block runs the rest. The consumer the gate waited for.
 */
class TestCtxReaderElim extends munit.FunSuite {

  type W = Writer % String

  /** the row spelling: the environment is an effect */
  def viaReader: Int ! Reader % Int + W = direct {
    val env = Reader.ask[Int].?
    Writer.tell(s"env=$env").?
    env + 1
  }

  /** the elimination: the environment is a using parameter — the
   * Reader row is GONE, wire reads it, the elaborator wires it */
  def viaCtx: Int ?=> Int ! W = direct {
    Writer(s"env=${wire[Int]}"): Unit
    wire[Int] + 1
  }

  test("the same program, both spellings, the same answers") {
    val (ws1, a1) = (Writer.run(
      Reader.run(41)(viaReader))).run
    val (ws2, a2) = (Writer.run(
      provide(41)(viaCtx))).run
    assertEquals((ws1, a1), (Seq("env=41"), 42))
    assertEquals((ws2, a2), (ws1, a1))
  }

  test("nearest-wins overriding reaches through the effectful block") {
    def prog: Int ?=> Int ! W = viaCtx
    val (ws, a) = (Writer.run(
      provide(1)(provide(100)(prog)))).run
    assertEquals(ws, Seq("env=100"))
    assertEquals(a, 101)
  }

  test("the one-line bridges — from the library now (ctx-reader-bridge, 2026-09-23), the same two lines") {
    // ctx -> Reader program: a FUNCTION, never a Conversion (E10)
    val fromCtx: Int ! Reader % Int = Reader.lift((e: Int) ?=> e * 2)
    assertEquals((Reader.run(21)(fromCtx)).run, 42)

    val back: Int ?=> Int ! W = Reader.unlift[Int, Int, Reader % Int + W](viaReader)
    val (ws, a) = (Writer.run(provide(7)(back))).run
    assertEquals((ws, a), (Seq("env=7"), 8))
  }
}
