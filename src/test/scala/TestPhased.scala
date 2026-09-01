import okay.*
import okay.given

/**
 * Typestate on the stream (specs/stage-pipeline.md, stage-phased):
 * the CSV shape end to end, the switch's output ordering, both
 * honest ends, the first does-not-COMPILE proof in the suite, and
 * PState doing work rather than being exhibited.
 */
class TestPhased extends munit.FunSuite {

  /** feed lines through a stage, collect (outputs, answer) */
  def runStage[O, A](lines: Seq[String])(st: Stage[String, O, A]): (Seq[O], A) =
    val told: String ! Writer % String =
      lines.foldLeft(pure[Writer % String, String]("")):
        (m, l) => m.flatMap(_ => Writer.tell(l).map(_ => l))
    !.run(Writer.run(through(told)(st)))

  /** the CSV shape: S1 = Unit (waiting for the header), S2 = the
   * column names; a row becomes name->value pairs — the body never
   * sees the header phase, BY TYPE */
  def csv: Stage[String, Vector[(String, String)], Either[Unit, Vector[String]]] =
    Stage.phased[String, Vector[(String, String)], Unit, Vector[String]](())(
      head = (_, line) => Right((line.split(',').toVector, Vector.empty)),
      body = (cols, line) => (cols, Vector(cols.zip(line.split(',').toVector))),
      endHead = _ => Vector.empty,
      endBody = _ => Vector.empty)

  test("the CSV shape: header typed into the body's state, rows keyed by it") {
    val (rows, answer) = runStage(Seq("name,age", "ann,25", "bo,31"))(csv)
    assertEquals(rows, Seq(
      Vector("name" -> "ann", "age" -> "25"),
      Vector("name" -> "bo", "age" -> "31")))
    assertEquals(answer, Right(Vector("name", "age")))
  }

  test("outputs at the switch precede the body's; order is total") {
    val st = Stage.phased[Int, String, Int, String](0)(
      head = (n, i) =>
        if n + i < 10 then Left((n + i, Vector(s"head:$i")))
        else Right((s"sum=${n + i}", Vector(s"switch:${n + i}"))),
      body = (tag, i) => (tag, Vector(s"$tag body:$i")),
      endHead = n => Vector(s"died-in-head:$n"),
      endBody = tag => Vector(s"end:$tag"))
    val told: Int ! Writer % Int =
      Seq(3, 4, 5, 1).foldLeft(pure[Writer % Int, Int](0)):
        (m, i) => m.flatMap(_ => Writer.tell(i).map(_ => i))
    val (out, answer) = !.run(Writer.run(through(told)(st)))
    assertEquals(out, Seq("head:3", "head:4", "switch:12", "sum=12 body:1", "end:sum=12"))
    assertEquals(answer, Right("sum=12"))
  }

  test("input ending DURING the head answers Left and flushes endHead") {
    val (out, answer) = runStage(Seq.empty[String])(csv)
    assertEquals(out, Seq.empty)
    assertEquals(answer, Left(()))
    // and with a non-empty head flush the death is visible
    val st = Stage.phased[String, String, Int, Unit](0)(
      head = (n, _) => Left((n + 1, Vector.empty)),
      body = (u, _) => (u, Vector.empty),
      endHead = n => Vector(s"header never completed after $n lines"),
      endBody = _ => Vector.empty)
    val (out2, answer2) = runStage(Seq("a", "b"))(st)
    assertEquals(out2, Seq("header never completed after 2 lines"))
    assertEquals(answer2, Left(2))
  }

  test("the illegal state does not COMPILE — the typestate proof") {
    // a body step written against the HEAD's state type: the phase
    // enum this combinator replaced would have made this a runtime
    // branch; here it is a type error, quoted
    val errors = compileErrors(
      """Stage.phased[String, String, Unit, Vector[String]](())(
           head = (_, l) => Right((l.split(',').toVector, Vector.empty)),
           body = (cols: Unit, l) => (cols, Vector.empty),
           endHead = _ => Vector.empty,
           endBody = _ => Vector.empty)""")
    assert(errors.nonEmpty, "the wrong-phase body compiled")
    assert(errors.contains("Unit") || errors.contains("Required"), errors)
  }

  test("the transition IS a PState run: the state type changes S1 -> Either under Cont") {
    // the same shape phased executes per head input, run bare — the
    // theory chapter's exhibit doing this library's work
    type R = (Either[Int, String], Vector[String])
    val (state, out) = PState.run[Int, Either[Int, String], Vector[String]](41):
      PState.get[Int, R].flatMap { n =>
        PState.set[Int, Either[Int, String], R](Right(s"n=${n + 1}")).map(_ => Vector("switched"))
      }
    assertEquals(state, Right("n=42"))
    assertEquals(out, Vector("switched"))
  }
}
