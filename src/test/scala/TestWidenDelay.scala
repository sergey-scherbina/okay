package okay

/**
 * windows-stage-rerun-loses-pane, the sixth door: `!.widen` and
 * `Writer.widen` walk from the head, and to see the head they RESUME
 * it — which forces a `Delay`. A stage whose state is made under
 * `Free.delay` (`Gather.stage`, `Transducers.stage`) therefore started
 * at WIDEN time, and the widened value held that start: run it twice,
 * the second run met the first run's state. Both walks now keep a
 * deferred head deferred — `Delay(t)` widens to `Delay(() =>
 * widen(t()))`, `Bind(Delay(t), f)` to `Free.defer` of the two
 * widened halves — so a widened program starts when it RUNS, once per
 * run, like any other value. `RowLift.plus`/`at` never had the
 * problem: they are one coercion and no walk.
 */
class TestWidenDelay extends munit.FunSuite:

  type Row = Reader % Int + Writer % String

  def runBoth(p: Int ! Row): (Seq[String], Int) =
    !.run(Writer.run[String, Int, okay.Pure](Reader.run[Int, Int, Writer % String](0)(p)))

  test("!.widen keeps a deferred head deferred: no start at widen, one start per run") {
    var starts = 0
    val delayed: Int ! Writer % String = Free.delay { () => starts += 1; Writer.tell("x").map(_ => 1) }
    val wide: Int ! Row = !.widen[Int, Writer % String, Reader % Int](delayed)
    assertEquals(starts, 0, "widening runs nothing")
    assertEquals(runBoth(wide), (Seq("x"), 1))
    assertEquals(runBoth(wide), (Seq("x"), 1))
    assertEquals(starts, 2, "one start per run")
  }

  test("!.widen keeps a deferred head under a bind (Free.defer) deferred too") {
    var starts = 0
    val deferred: Int ! Writer % String =
      Free.defer(() => { starts += 1; Writer.tell("x").map(_ => 1) })(n => Writer.tell("y").map(_ => n + 1))
    val wide: Int ! Row = !.widen[Int, Writer % String, Reader % Int](deferred)
    assertEquals(starts, 0, "widening runs nothing")
    assertEquals(runBoth(wide), (Seq("x", "y"), 2))
    assertEquals(runBoth(wide), (Seq("x", "y"), 2))
    assertEquals(starts, 2)
  }

  test("Writer.widen keeps a deferred head deferred") {
    var starts = 0
    val delayed: Int ! Writer % String + Reader % Int = Free.delay { () =>
      starts += 1; !.widen[Int, Writer % String, Reader % Int](Writer.tell("x").map(_ => 1)) }
    val wide: Int ! Writer % CharSequence + Reader % Int =
      Writer.widen[String, CharSequence, Int, Reader % Int](delayed)
    assertEquals(starts, 0, "widening runs nothing")
    def run = !.run(Reader.run[Int, (Seq[CharSequence], Int), okay.Pure](0)(
      Writer.run[CharSequence, Int, Reader % Int](wide)))
    assertEquals(run._1.map(_.toString), Seq("x"))
    assertEquals(run._1.map(_.toString), Seq("x"))
    assertEquals(starts, 2)
  }
