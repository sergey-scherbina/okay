package okay

import okay.Direct.*
import okay.Row.at
import scala.language.implicitConversions

/**
 * WHAT A CAPTURE DOES TO EVERYTHING ELSE (delim-limits, 2026-09-17).
 * The four patterns say what to write; this suite says what happens
 * when a capture meets state, resources, errors, a `finally`, a
 * second machine, and depth. Every answer here was a question nothing
 * in the tree could answer before it was run, and docs/continuations-
 * in-practice.md's "when a capture makes code worse" quotes these
 * numbers rather than reasoning about them.
 */
class TestDelimLimits extends munit.FunSuite {

  type P = okay.Pure

  // ==== STATE ======================================================

  test("state: a handler OUTSIDE the delimiter is SHARED by the branches") {
    // the second invocation of k sees what the first one wrote — the
    // state handler is outside the machine, so the branches are one
    // timeline, not a fork. Backtracking (Logic/Choice) is the effect
    // that gives the other semantics.
    type S = State % Int
    type Row = Delim + S
    val prog: Int ! S = Delim.delimited[Int, S]:
      direct:
        val x = !Delim.shift[Int, Int, S](k => direct { !k(1) + !k(10) })
        !State.modify[Int](_ + x).at[Row]
    val (s, a) = State.run[Int, Int](0)(prog)
    assertEquals(s, 11, "the second branch did not see the first branch's write")
    assertEquals(a, 12, "k(1) answered 1 and k(10) answered 11")
  }

  // ==== RESOURCES ==================================================

  test("resource: an abandoned continuation still releases") {
    // `exit` drops the rest of the block, so nothing the block wrote
    // after that point runs — but Resource's handler is outside the
    // machine and closes what was opened.
    var log = List.empty[String]
    type Row = Delim + Resource
    val prog: Int ! Resource = Delim.delimited[Int, Resource]:
      direct:
        val r = !Resource.acquire { log = log :+ "acquire"; 7 }
                                  { _ => log = log :+ "release" }.at[Row]
        !Delim.exit(r * 2)
        99
    assertEquals(!.run(Resource.run[Int, P](prog)), 14)
    assertEquals(log, List("acquire", "release"))
  }

  test("resource: a multi-shot capture opens one per branch, and closes them ALL AT THE END") {
    // the cost to know about: two branches hold two handles at once.
    // Releases are LIFO at the end of the program, not between the
    // branches — a capture invoked n times is n open resources.
    var log = List.empty[String]
    type Row = Delim + Resource
    val prog: Int ! Resource = Delim.delimited[Int, Resource]:
      direct:
        val x = !Delim.shift[Int, Int, Resource](k => direct { !k(1) + !k(2) })
        val r = !Resource.acquire { log = log :+ s"acquire$x"; x }
                                  { n => log = log :+ s"release$n" }.at[Row]
        r * 10
    assertEquals(!.run(Resource.run[Int, P](prog)), 30)
    assertEquals(log, List("acquire1", "acquire2", "release2", "release1"))
  }

  test("cleanup written by hand after a capture point does NOT run") {
    // the pair to the two above: `Resource` survives a capture
    // because its handler is outside the machine; a line of ordinary
    // Scala is just part of the continuation that was dropped.
    var cleaned = false
    val prog: Int ! P = Delim.delimited[Int, P]:
      direct:
        !Delim.exit(1)
        cleaned = true
        0
    assertEquals(!.run(prog), 1)
    assert(!cleaned, "the dropped continuation ran its cleanup line")
  }

  test("bracket is REFUSED in a Delim row — the unsafe mix cannot be written") {
    // `bracket` runs its body to completion inside one suspension,
    // which is exactly what a capture breaks. It needs a Handler for
    // the row, and Delim has none: the compiler says no.
    val e = compileErrors(
      "okay.bracket[Int, Int, okay.Delim + okay.Pure](1)(_ => ())(r => okay.pure(r))")
    assert(e.nonEmpty, "bracket compiled under Delim")
    assert(e.contains("Handler"), s"refused for the wrong reason: $e")
  }

  // ==== ERRORS =====================================================

  test("Throws: a raise from inside a captured continuation reaches the handler") {
    type T = Throws % String
    type Row = Delim + T
    val prog: Int ! T = Delim.delimited[Int, T]:
      direct:
        val x = !Delim.shift[Int, Int, T](k => k(1))
        if x == 1 then !okay.raise[String, Int]("boom").at[Row] else x
    assertEquals(!.run(okay.runEither[Int, P, String](prog)), Left("boom"))
  }

  test("Throws: a handler that raises instead of resuming leaves the rest unrun") {
    type T = Throws % String
    type Row = Delim + T
    var ran = false
    val prog: Int ! T = Delim.delimited[Int, T]:
      direct:
        val x = !Delim.shift[Int, Int, T](_ => okay.raise[String, Int]("cut").at[Row])
        ran = true
        x
    assertEquals(!.run(okay.runEither[Int, P, String](prog)), Left("cut"))
    assert(!ran, "the abandoned continuation ran")
  }

  test("`try/finally` around a mark is a COMPILE error, not a silent one") {
    val e = compileErrors("""
      okay.Delim.delimited[Int, okay.Pure](okay.Direct.direct {
        var closed = false
        try { !okay.Delim.exit(1); 0 } finally { closed = true }
      })""")
    assert(e.nonEmpty, "a finalizer around a capture compiled")
    assert(e.contains("finalizer"), s"refused for the wrong reason: $e")
  }

  test("`try/catch` around a mark compiles, and catches NOTHING the interpreter throws") {
    // the trap this pins: a catch in a `direct` block guards the
    // BUILDING of the program, and the throw happens when the program
    // is RUN, one stack away. Handle failure with `Throws`, which is
    // in the row and therefore in the program.
    val prog: Int ! Async = direct:
      try !okay.async[Int](throw new RuntimeException("boom"))
      catch case _: RuntimeException => -1
    val got = scala.util.Try(!.run(Async.run[Int, P](prog)))
    assert(got.isFailure, s"the catch caught it after all: $got — update this test and the docs")
  }

  // ==== A SECOND MACHINE ===========================================

  test("a second machine in one row is a COMPILE error (delim-safety stage 0)") {
    // `Prompted` proves a delimiter was installed, not that THIS
    // machine holds it — which used to be a runtime NoPrompt for an
    // ordinary nesting. The row guard refuses the shape instead.
    val e = compileErrors("""
      okay.Delim.delimited[Int, okay.Delim + okay.Pure](okay.pure(1))""")
    assert(e.nonEmpty, "a second machine compiled")
    assert(e.contains("SECOND machine"), s"the message does not say what is wrong: $e")
  }

  test("THE LIMIT: an ABSTRACT row is not caught, and still throws") {
    // `NotGiven` reads an unknown F as "absent", so a row-polymorphic
    // helper compiles — and NoPrompt is still what happens when it is
    // instantiated at a Delim row. Stages 1 and 2 of
    // specs/delim-safety.md exist for this line.
    // no witness in ITS signature: the guard is summoned for an
    // abstract F, where it succeeds — that is the hole. A helper that
    // DOES take `using Delim.OneMachine[F]` propagates the obligation
    // and its call site is refused, which is the fix available today.
    def generic[F[+_]](p: Int ! Delim + F): Int ! F = Delim.run(p)
    val outer = Delim.prompt[Int]
    def prog: Int ! P = Delim.delimited[Int, P]:
      direct:
        100 + !generic[Delim + P](Delim.shift[Int, Int, Delim + P](outer)(k => k(5)))
    intercept[NoPrompt](!.run(prog))
  }

  // ==== DEPTH ======================================================

  test("depth: ten thousand emits, three thousand pauses, and a replay of them") {
    def many(n: Int)(using Delim.Emitting[Int]): Unit ! Delim + P = direct:
      var i = 0
      while i < n do
        !Delim.emit(i)
        i += 1
    assertEquals(!.run(Delim.collect[Int, P](many(10000))).size, 10000)

    def asks(n: Int)(using Delim.Asking[Int, Int, Int, Delim + P]): Int ! Delim + P = direct:
      var acc = 0
      var i = 0
      while i < n do
        acc += !Delim.pause(i)
        i += 1
      acc
    val driven = !.run(Delim.drive[Int, Int, Int, P](
      !.run(Delim.resumable[Int, Int, Int, P](asks(3000))))(q => okay.pure(q)))
    assertEquals(driven, (0 until 3000).sum)
    assertEquals(!.run(Delim.replay[Int, Int, Int, P](asks(3000))((0 until 3000).toList)).finished,
      Some((0 until 3000).sum))
  }

  // ==== SHAPES THAT DO WORK ========================================

  test("exit leaves from inside a lambda the block does not own") {
    val prog: Int ! P = Delim.delimited[Int, P]:
      direct:
        val xs = List(1, 2, 3).map(n => if n == 2 then !Delim.exit(n * 100) else ())
        xs.size
    assertEquals(!.run(prog), 200)
  }

  test("a dialogue pauses across an async operation") {
    type Row = Delim + Async
    def body(using Delim.Asking[String, Int, Int, Row]): Int ! Row = direct:
      val a = !Delim.pause("q1")
      val b = !okay.async(a * 2).at[Row]
      val c = !Delim.pause(s"q2:$b")
      b + c
    assertEquals(!.run(Async.run[Int, P](
      Delim.resumable[String, Int, Int, Async](body).flatMap(p =>
        Delim.drive[String, Int, Int, Async](p)(q => okay.pure(q.length))))), 8)
  }
}
