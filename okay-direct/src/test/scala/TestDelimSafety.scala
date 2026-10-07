package okay


import okay.freer.*


import okay.freer.given
import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE SECOND MACHINE DOES NOT HAPPEN (delim-safety stage 0,
 * 2026-09-17, then shift-merge-guard, 2026-10-02). It was first a
 * runtime NoPrompt, then a compile error; now every machine-starting
 * door reads its row (`Shift.Machine`) and, at a row holding `Shift`,
 * installs on the running machine. The old limit is closed too: an
 * abstract row is a compile error asking for the evidence.
 */
class TestDelimSafety extends munit.FunSuite {

  type P = okay.freer.Pure
  type Row = Shift % ? + P

  test("collect inside a Shift row compiles and nests: its emits reach it, a capture crosses it") {
    val p = Shift.prompt[List[Int]]
    val inner: List[Int] ! Row = Shift.collect[Int, Row](direct {
      !Shift.emit(1)
      !Shift.abort[List[Int], Unit, Row](p)(List(42))
    })
    assertEquals(!.run(Shift.run[List[Int], P](Shift.push[List[Int], P](p)(inner))), List(42))
    assertEquals(!.run(Shift.delimited[List[Int], P](Shift.collect[Int, Row](direct { !Shift.emit(1) }))), List(1))
  }

  test("the same on delimited, resumable, reset and run: each nests at a Shift row") {
    val outer = Shift.prompt[Int]
    def escape: Int ! Shift % ? + Row = Shift.abort[Int, Int, Row](outer)(7)
    def around(p: Int ! Row): Int = !.run(Shift.run[Int, P](Shift.push[Int, P](outer)(p.map(_ + 1000))))
    assertEquals(around(Shift.delimited[Int, Row](escape)), 7)
    assertEquals(around(Shift.reset[Int, Row](_ => escape)), 7)
    assertEquals(around(Shift.run[Int, Row](escape)), 7)
    assertEquals(around(Shift.resumable[String, Int, Int, Row](escape).map(_.finished.getOrElse(-1))), 7)
  }

  test("an abstract row is refused, naming the evidence") {
    val e = compileErrors("def h[F[+_]](p: Int ! okay.freer.Shift % ? + F): Int ! F = okay.freer.Shift.run(p)")
    assert(e.contains("using Shift.Machine[F]"), s"the abstract row was guessed: $e")
  }

  test("the nested forms still compile in a Shift row — that is what they are for") {
    // the explicit spelling, unchanged
    def half(using Shift.Asking[String, Int, List[Int], Shift % ? + P]): List[Int] ! Shift % ? + P =
      Shift.collecting[Int, P]:
        direct:
          !Shift.emit(1)
          !Shift.emit(!Shift.pause("more?"))
    val start = !.run(Shift.resumable[String, Int, List[Int], P](half))
    assertEquals(!.run(Shift.drive(start)(_ => okay.freer.pure(2))), List(1, 2))
  }

}
