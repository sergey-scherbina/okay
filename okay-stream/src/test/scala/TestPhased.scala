import okay.*
import okay.given
import scala.annotation.nowarn

/**
 * Typestate on the stream (specs/stage-pipeline.md, stage-phased):
 * the CSV shape end to end, the switch's output ordering, both
 * honest ends, the first does-not-COMPILE proof in the suite, and
 * PState doing work rather than being exhibited.
 */
class TestPhased extends munit.FunSuite {

  /** feed lines through a stage, collect (outputs, answer) */
  // Writer.run's inline body checks the answer at O, abstract here —
  // the trusted kernel's warning (Effects.scala), not a cast added here
  @nowarn("msg=cannot be checked at runtime")
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

/** the three-phase sibling, driven by its consumer's shape: the
 * http message — request-line -> headers -> body */
class TestPhased3 extends munit.FunSuite {

  // Writer.run's inline body checks the answer at O, abstract here —
  // the trusted kernel's warning (Effects.scala), not a cast added here
  @nowarn("msg=cannot be checked at runtime")
  def runStage[O, A](lines: Seq[String])(st: Stage[String, O, A]): (Seq[O], A) =
    val told: String ! Writer % String =
      lines.foldLeft(pure[Writer % String, String]("")):
        (m, l) => m.flatMap(_ => Writer.tell(l).map(_ => l))
    !.run(Writer.run(through(told)(st)))

  /** S1 = Unit (awaiting the request line), S2 = (method, target) +
   * headers so far, S3 = the complete head; body lines emit tagged */
  def http: Stage[String, String,
      Either[Unit, Either[((String, String), Vector[String]), String]]] =
    Stage.phased3[String, String, Unit, ((String, String), Vector[String]), String](())(
      first = (_, line) =>
        val Array(m, t, _) = line.split(' ')
        Right((((m, t), Vector.empty), Vector(s"line:$m $t"))),
      second = (acc, line) =>
        if line.isEmpty then Right((s"${acc._1._1} ${acc._1._2} [${acc._2.mkString(";")}]",
          Vector("headers-done")))
        else Left(((acc._1, acc._2 :+ line), Vector.empty)),
      third = (head, line) => (head, Vector(s"$head body:$line")),
      endFirst = _ => Vector("died: no request line"),
      endSecond = acc => Vector(s"died in headers after ${acc._2.length}"),
      endThird = _ => Vector.empty)

  test("the http message shape: three phases, both switches typed") {
    val (out, answer) = runStage(Seq(
      "GET /q HTTP/1.1", "host: x", "accept: *", "", "hello", "world"))(http)
    assertEquals(out, Seq(
      "line:GET /q", "headers-done",
      "GET /q [host: x;accept: *] body:hello",
      "GET /q [host: x;accept: *] body:world"))
    assertEquals(answer, Right(Right("GET /q [host: x;accept: *]")))
  }

  test("the answer names the dying phase, three ways") {
    assertEquals(runStage(Seq.empty[String])(http)._2, Left(()))
    val (out2, a2) = runStage(Seq("GET /q HTTP/1.1", "host: x"))(http)
    assertEquals(a2, Right(Left((("GET", "/q"), Vector("host: x")))))
    assert(out2.contains("died in headers after 1"))
  }

  test("the wrong-phase step is a compile error at BOTH seams") {
    val atSecond = compileErrors(
      """Stage.phased3[String, String, Unit, Vector[String], Int](())(
           first = (_, l) => Right((Vector(l), Vector.empty)),
           second = (n: Int, l) => Left((n, Vector.empty)),
           third = (n, l) => (n, Vector.empty),
           endFirst = _ => Vector.empty, endSecond = _ => Vector.empty,
           endThird = _ => Vector.empty)""")
    assert(atSecond.nonEmpty, "a second step at the THIRD phase's type compiled")
    val atThird = compileErrors(
      """Stage.phased3[String, String, Unit, Vector[String], Int](())(
           first = (_, l) => Right((Vector(l), Vector.empty)),
           second = (v, l) => Right((v.length, Vector.empty)),
           third = (v: Vector[String], l) => (v, Vector.empty),
           endFirst = _ => Vector.empty, endSecond = _ => Vector.empty,
           endThird = _ => Vector.empty)""")
    assert(atThird.nonEmpty, "a third step at the SECOND phase's type compiled")
  }
}

