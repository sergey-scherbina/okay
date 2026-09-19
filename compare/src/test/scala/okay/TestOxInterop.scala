package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * CAN okay and Ox be made to work together, and where exactly
 * does the seam fail? Four directions, run rather than reasoned about.
 */
class TestOxInterop extends munit.FunSuite {

  type R = Delim + Pure

  // ── 1. an okay PROGRAM run inside an Ox scope ─────────────────
  test("1. okay program inside an Ox supervised scope") {
    import ox.*
    val out = supervised {
      val a = !.run(Delim.collect[Int, Pure](direct:
        !Delim.emit(1); !Delim.emit(2)))
      val b = fork(40).join()
      (a, b)
    }
    assertEquals(out, (List(1, 2), 40))
  }

  // ── 2. an Ox FORK consumed from inside an okay program ────────
  test("2. an Ox fork joined inside okay.async") {
    import ox.*
    val prog: Int ! Async = direct:
      val forked = !okay.async {
        supervised { fork(7).join() }
      }
      forked * 6
    assertEquals(!.run(Async.run[Int, Pure](prog)), 42)
  }

  // ── 3. a ONE-SHOT capture around an Ox fork ───────────────────
  test("3. a one-shot capture whose body forks") {
    import ox.*
    var forks = 0
    val out = supervised {
      !.run(Delim.collect[Int, Pure](direct:
        !Delim.emit(1)
        val f = fork { forks += 1; 2 }
        !Delim.emit(f.join())))
    }
    assertEquals(out, List(1, 2))
    assertEquals(forks, 1, "the fork ran a number of times the test did not expect")
  }

  // ── 4. THE SEAM: a MULTI-SHOT capture over an Ox fork ─────────
  test("4. a capture invoked TWICE over an Ox fork — what happens?") {
    import ox.*
    var forks = 0
    var joins = 0
    val out = scala.util.Try {
      supervised {
        !.run(Delim.delimited[Int, Pure]:
          direct:
            val x = !Delim.shift[Int, Int, Pure](k => direct { !k(1) + !k(10) })
            val f = fork { forks += 1; x }
            joins += 1
            f.join())
      }
    }
    println(s"[probe] multi-shot over an Ox fork -> $out  (forks=$forks joins=$joins)")
    assert(out.isSuccess || out.isFailure, "recorded either way")
  }
}
