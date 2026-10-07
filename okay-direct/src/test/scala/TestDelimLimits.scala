package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.Direct.*
import okay.freer.Row.at
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

  type P = okay.freer.Pure

  // ==== STATE ======================================================

  test("state: a handler OUTSIDE the delimiter is SHARED by the branches") {
    // the second invocation of k sees what the first one wrote — the
    // state handler is outside the machine, so the branches are one
    // timeline, not a fork. Backtracking (Logic/Choice) is the effect
    // that gives the other semantics.
    type S = State % Int
    type Row = Shift % ? + S
    val prog: Int ! S = Shift.delimited[Int, S]:
      direct:
        val x = !Shift.shift[Int, Int, S](k => direct { !k(1) + !k(10) })
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
    type Row = Shift % ? + Resource
    val prog: Int ! Resource = Shift.delimited[Int, Resource]:
      direct:
        val r = !Resource.acquire { log = log :+ "acquire"; 7 }
                                  { _ => log = log :+ "release" }.at[Row]
        !Shift.exit(r * 2)
        99
    assertEquals(!.run(Resource.run[Int, P](prog)), 14)
    assertEquals(log, List("acquire", "release"))
  }

  test("resource: a multi-shot capture opens one per branch, and closes them ALL AT THE END") {
    // the cost to know about: two branches hold two handles at once.
    // Releases are LIFO at the end of the program, not between the
    // branches — a capture invoked n times is n open resources.
    var log = List.empty[String]
    type Row = Shift % ? + Resource
    val prog: Int ! Resource = Shift.delimited[Int, Resource]:
      direct:
        val x = !Shift.shift[Int, Int, Resource](k => direct { !k(1) + !k(2) })
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
    val prog: Int ! P = Shift.delimited[Int, P]:
      direct:
        !Shift.exit(1)
        cleaned = true
        0
    assertEquals(!.run(prog), 1)
    assert(!cleaned, "the dropped continuation ran its cleanup line")
  }

  test("bracketNow is REFUSED in a Shift row — the unsafe mix cannot be written") {
    // `bracketNow` runs its body to completion inside one suspension,
    // which is exactly what a capture breaks. It needs an Answers for
    // the row, and Shift has none: the compiler says no.
    val e = compileErrors(
      "okay.std.bracketNow[Int, Int, okay.freer.Shift % ? + okay.freer.Pure](1)(_ => ())(r => okay.freer.pure(r))")
    assert(e.nonEmpty, "bracketNow compiled under Shift")
    assert(e.contains("Answers"), s"refused for the wrong reason: $e")
  }

  // ==== ERRORS =====================================================

  test("Throws: a raise from inside a captured continuation reaches the handler") {
    type T = Throws % String
    type Row = Shift % ? + T
    val prog: Int ! T = Shift.delimited[Int, T]:
      direct:
        val x = !Shift.shift[Int, Int, T](k => k(1))
        if x == 1 then !okay.std.raise[String, Int]("boom").at[Row] else x
    assertEquals(!.run(okay.std.runEither[Int, P, String](prog)), Left("boom"))
  }

  test("Throws: a handler that raises instead of resuming leaves the rest unrun") {
    type T = Throws % String
    type Row = Shift % ? + T
    var ran = false
    val prog: Int ! T = Shift.delimited[Int, T]:
      direct:
        val x = !Shift.shift[Int, Int, T](_ => okay.std.raise[String, Int]("cut").at[Row])
        ran = true
        x
    assertEquals(!.run(okay.std.runEither[Int, P, String](prog)), Left("cut"))
    assert(!ran, "the abandoned continuation ran")
  }

  test("`try/finally` around a mark is a COMPILE error, not a silent one") {
    val e = compileErrors("""
      okay.freer.Shift.delimited[Int, okay.freer.Pure](okay.Direct.direct {
        var closed = false
        try { !okay.freer.Shift.exit(1); 0 } finally { closed = true }
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

  test("a door at a row where a machine runs NESTS on it (shift-merge-guard)") {
    // `Prompted` proves a delimiter was installed, not that THIS
    // machine holds it — which used to be a runtime NoPrompt for an
    // ordinary nesting, then a compile error (delim-safety stage 0).
    // `Shift.Machine` reads the row, and the door pushes its delimiter
    // on the machine already running: the capture crosses it.
    val outer = Shift.prompt[Int]
    val prog: Int ! P = Shift.run[Int, P](Shift.push[Int, P](outer)(
      Shift.delimited[Int, Shift % ? + P](Shift.abort[Int, Int, Shift % ? + P](outer)(7)).map(_ + 1000)))
    assertEquals(!.run(prog), 7)
  }

  test("an ABSTRACT row is a COMPILE error naming the fix; passed on, the helper nests") {
    // `NotGiven` read an unknown F as "absent", so a row-polymorphic
    // helper compiled and threw NoPrompt at a Shift row (the stages 1
    // and 2 of specs/delim-safety.md were for this line). The row
    // cannot be read, so the evidence is asked for instead of guessed.
    val e = compileErrors("""
      def generic[F[+_]](p: Int ! Shift % ? + F): Int ! F = Shift.run(p)
      """)
    assert(e.contains("using Shift.Machine[F]"), s"the message does not name the fix: $e")
    def generic[F[+_]](p: Int ! Shift % ? + F)(using Shift.Machine[F]): Int ! F = Shift.run(p)
    val outer = Shift.prompt[Int]
    def prog: Int ! P = Shift.run[Int, P](Shift.push[Int, P](outer)(
      generic[Shift % ? + P](Shift.shift[Int, Int, Shift % ? + P](outer)(k => k(5))).map(100 + _)))
    assertEquals(!.run(prog), 105)
  }

  // ==== DEPTH ======================================================

  test("depth: ten thousand emits, three thousand pauses, and a replay of them") {
    def many(n: Int)(using Shift.Emitting[Int]): Unit ! Shift % ? + P = direct:
      var i = 0
      while i < n do
        !Shift.emit(i)
        i += 1
    assertEquals(!.run(Shift.collect[Int, P](many(10000))).size, 10000)

    def asks(n: Int)(using Shift.Asking[Int, Int, Int, Shift % ? + P]): Int ! Shift % ? + P = direct:
      var acc = 0
      var i = 0
      while i < n do
        acc += !Shift.pause(i)
        i += 1
      acc
    val driven = !.run(Shift.drive[Int, Int, Int, P](
      !.run(Shift.resumable[Int, Int, Int, P](asks(3000))))(q => okay.freer.pure(q)))
    assertEquals(driven, (0 until 3000).sum)
    assertEquals(!.run(Shift.replay[Int, Int, Int, P](asks(3000))((0 until 3000).toList)).finished,
      Some((0 until 3000).sum))
  }

  // ==== SHAPES THAT DO WORK ========================================

  test("exit leaves from inside a lambda the block does not own") {
    val prog: Int ! P = Shift.delimited[Int, P]:
      direct:
        val xs = List(1, 2, 3).map(n => if n == 2 then !Shift.exit(n * 100) else ())
        xs.size
    assertEquals(!.run(prog), 200)
  }

  test("a dialogue pauses across an async operation") {
    type Row = Shift % ? + Async
    def body(using Shift.Asking[String, Int, Int, Row]): Int ! Row = direct:
      val a = !Shift.pause("q1")
      val b = !okay.async(a * 2).at[Row]
      val c = !Shift.pause(s"q2:$b")
      b + c
    assertEquals(!.run(Async.run[Int, P](
      Shift.resumable[String, Int, Int, Async](body).flatMap(p =>
        Shift.drive[String, Int, Int, Async](p)(q => okay.freer.pure(q.length))))), 8)
  }
}
