package okay

import okay.Direct.*

/** BACKLOG direct-try-ctx — a minimal repro/probe: does `try` inside
 * `direct[[X] =>> E ?=> X]` compile with an honest, deferred-to-
 * application CanTry instance (Throws.scala, ctxFn)? */
class TestDirectTryCtx extends munit.FunSuite {
  case class Env(n: Int)

  test("try/catch inside a context-function direct block") {
    val prog: Env ?=> Int =
      direct[[X] =>> Env ?=> X] {
        try
          if wire[Env].n == 0 then throw RuntimeException("boom") else wire[Env].n
        catch case _: Throwable => -1
      }
    assertEquals(provide(Env(5))(prog), 5)
    assertEquals(provide(Env(0))(prog), -1)
  }
}
