package okay

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
  def viaReader: Int ! (Reader % Int + W) = direct {
    val env = effect[Reader % Int + W, Int](Reader.Ask()).reflect
    effect[Reader % Int + W, Unit](Writer(s"env=$env")).reflect
    env + 1
  }

  /** the elimination: the environment is a using parameter — the
   * Reader row is GONE, wire reads it, the elaborator wires it */
  def viaCtx: Int ?=> Int ! W = direct {
    Writer(s"env=${wire[Int]}"): Unit
    wire[Int] + 1
  }

  test("the same program, both spellings, the same answers") {
    val (ws1, a1) = !.run(Writer.run[String, Int, okay.Pure](
      Reader.run[Int, Int, W](41)(viaReader)))
    val (ws2, a2) = !.run(Writer.run[String, Int, okay.Pure](
      provide(41)(viaCtx)))
    assertEquals((ws1, a1), (Seq("env=41"), 42))
    assertEquals((ws2, a2), (ws1, a1))
  }

  test("nearest-wins overriding reaches through the effectful block") {
    def prog: Int ?=> Int ! W = viaCtx
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](
      provide(1)(provide(100)(prog))))
    assertEquals(ws, Seq("env=100"))
    assertEquals(a, 101)
  }

  test("the one-line bridges, at the call site as the spec promised") {
    // ctx -> Reader program: a FUNCTION, never a Conversion (E10)
    def lift[E, A](cf: E ?=> A): A ! (Reader % E) =
      effect[Reader % E, E](Reader.Ask()).map(e => cf(using e))
    // Reader program -> ctx: run under the ambient environment
    def unlift[E, A, F[+_]](p: A ! (Reader % E + F)): E ?=> A ! F =
      Reader.run[E, A, F](wire[E])(p)

    val fromCtx: Int ! (Reader % Int) = lift((e: Int) ?=> e * 2)
    assertEquals(!.run(Reader.run[Int, Int, okay.Pure](21)(fromCtx)), 42)

    val back: Int ?=> Int ! W = unlift[Int, Int, W](viaReader)
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](provide(7)(back)))
    assertEquals((ws, a), (Seq("env=7"), 8))
  }
}
