package okay


import okay.freer.*
import okay.freer.given
import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 12, COMPILED (docs/continuations/12-one-machine.md).
 *
 * The rule — one machine per program — and the evidence that keeps it
 * (`Shift.Machine[F]`, shift-merge-guard): a door that would start a
 * second machine at a row where one already runs NESTS on it instead,
 * and a row the compiler cannot read asks for the evidence.
 */
class TestBookOneMachine extends munit.FunSuite {

  type Row = Shift % ? + Pure

  // ---- the shape that used to be the mistake

  test("a door at a row where a machine runs nests on it: a capture crosses it") {
    val p = Shift.prompt[String]
    val inner: Int ! Row = Shift.delimited[Int, Row](Shift.abort[String, Int, Row](p)("escaped"))
    val prog: String ! Row = Shift.push[String, Pure](p)(inner.map(_.toString))
    assertEquals(!.run(Shift.run[String, Pure](prog)), "escaped")
  }

  test("the nested spelling still works, and means the same") {
    val r = Shift.delimited[String, Pure]:
      direct:
        val n = !Shift.scope[Int, Pure]:
          direct:
            !Shift.exit(3)
            0
        s"n=$n"
    assertEquals(!.run(r), "n=3")
  }

  test("collect inside a delimited block: one machine, both delimiters on it") {
    val r = Shift.delimited[List[Int], Pure]:
      Shift.collect[Int, Row](Shift.emit(1).flatMap(_ => Shift.emit(2)))
    assertEquals(!.run(r), List(1, 2))
  }

  // ---- the evidence, read off the row

  test("Shift.Machine reads the row: a Shift of any key means a machine runs") {
    assert(summon[Shift.Machine[Shift % ? + Pure]].inner)
    assert(summon[Shift.Machine[Shift % Int + Pure]].inner)
    assert(summon[Shift.Machine[State % Int + Shift % String]].inner)
    assert(!summon[Shift.Machine[Pure]].inner)
    assert(!summon[Shift.Machine[State % Int + Pure]].inner)
  }

  // ---- THE HOLE, CLOSED: an abstract row is not guessed

  test("an ABSTRACT row is refused, and the message says to pass the evidence on") {
    val e = compileErrors("""
      def runAnything[A, F[+_]](p: A ! Shift % ? + F): A ! F =
        Shift.run(p)""")
    assert(e.nonEmpty, "an abstract row was guessed")
    assert(e.contains("using Shift.Machine[F]"), s"the message does not name the fix: $e")
  }

  /** the helper passes the obligation on, so its caller — where the row is known — answers */
  def runAnything[A, F[+_]](p: A ! Shift % ? + F)(using Shift.Machine[F]): A ! F =
    Shift.run(p)

  test("a helper passing it on runs its own machine at a plain row and nests at a Shift row") {
    val p = Shift.prompt[Int]
    // outermost: the helper's machine answers the capture
    assertEquals(!.run(runAnything[Int, Pure](Shift.push(p)(Shift.abort[Int, Int, Pure](p)(7)))), 7)
    // inside a machine: the capture to the OUTER prompt crosses the helper, which used to be a NoPrompt
    val inner: Int ! (Shift % ? + Row) = Shift.abort[Int, Int, Row](p)(7)
    val outer: Int ! Row = Shift.push[Int, Pure](p)(runAnything[Int, Row](inner).map(_ + 100))
    assertEquals(!.run(Shift.run[Int, Pure](outer)), 7)
  }
}
