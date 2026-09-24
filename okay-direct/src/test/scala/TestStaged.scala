package okay

import okay.Direct.*

/**
 * specs/direct-staged.md: a `direct` block whose operations are
 * compiled to their handler's arms. The law is agreement with the
 * SAME block text lowered to a Free program and run by the shipping
 * runners — state, log and answer — with the block's SHAPE static (it
 * must be: the arm is chosen where the operation is written) and its
 * DATA generated.
 */
class TestStaged extends munit.FunSuite:

  type Row = State % Int + Writer % String
  type R = Stager.Answer[Int, String, Int]
  val sw = Stager.StateWriter[Int, String, Int]()

  /** the block, as a Free program: what every direct block is today */
  def free(xs: List[Int], k: Int): Int ! Row = direct[[A] =>> A ! Row] {
    val s0 = State.get[Int].!?
    Writer.tell(s"start $s0").!?
    for x <- xs do
      val s = State.get[Int].!?
      val s2 = State.set[Int](s + x).!?
      Writer.tell(s"set $s2").!?
    val mid = State.get[Int].!?
    if mid % 2 == 0 then Writer.tell("even").!? else Writer.tell("odd").!?
    // a raw OPERATION under the mark, beside the combinators
    val fin = (State.Get(): State[Int, Int]).!?
    fin + k
  }

  /** the same text, staged: every mark is one arm of `sw.stage` */
  def staged(xs: List[Int], k: Int): Handled[Row, R, Int] = Direct.staged(sw) {
    val s0 = State.get[Int].!?
    Writer.tell(s"start $s0").!?
    for x <- xs do
      val s = State.get[Int].!?
      val s2 = State.set[Int](s + x).!?
      Writer.tell(s"set $s2").!?
    val mid = State.get[Int].!?
    if mid % 2 == 0 then Writer.tell("even").!? else Writer.tell("odd").!?
    val fin = (State.Get(): State[Int, Int]).!?
    fin + k
  }

  def runFree(xs: List[Int], k: Int, s0: Int): (Int, Vector[String], Int) =
    val (s, (log, a)) = State.run[Int, (Seq[String], Int)](s0)(Writer.run[String, Int, State % Int](free(xs, k)))
    (s, log.toVector, a)

  def runStaged(xs: List[Int], k: Int, s0: Int): (Int, Vector[String], Int) =
    val ((s, log), a) = sw.run(s0)(staged(xs, k))
    (s, log, a)

  test("a staged block agrees with the Free block on generated data: state, log, answer") {
    val rnd = new scala.util.Random(20260922)
    for _ <- 1 to 300 do
      val xs = List.fill(rnd.nextInt(12))(rnd.nextInt(100) - 50)
      val k = rnd.nextInt(1000)
      val s0 = rnd.nextInt(1000)
      assertEquals(runStaged(xs, k, s0), runFree(xs, k, s0), s"xs=$xs k=$k s0=$s0")
  }

  test("one concrete run, read by eye") {
    assertEquals(runStaged(List(1, 2), 100, 5),
      (8, Vector("start 5", "set 6", "set 8", "even"), 108))
  }

  test("a loop of ten thousand operations fits the default stack (Func's contract, stated)") {
    def loop(i: Int, acc: Int): Handled[Row, R, Int] =
      if i >= 2500 then Handled.pure(acc)
      else Direct.staged(sw) {
        val a = State.get[Int].!?
        val _ = State.set[Int](a + 1).!?
        Writer.tell("w").!?
        val b = State.get[Int].!?
        loop(i + 1, acc + b).!?
      }
    val ((s, log), a) = sw.run(0)(loop(0, 0))
    assertEquals(s, 2500)
    assertEquals(log.size, 2500)
    assertEquals(a, (1 to 2500).sum)
  }

  // ---- v2: compound programs are walked into binds

  import okay.RowLift.at

  def freeCompound(xs: List[Int], k: Int): Int ! Row = direct[[A] =>> A ! Row] {
    val s1 = State.modify[Int](_ + k).!?                                  // an inline combinator
    val s2 = State.get[Int].flatMap(s => State.set[Int](s * 2)).!?         // a hand-written chain
    val s3 = (for                                                           // a for-comprehension over the row
        s <- State.get[Int].at[Row]
        _ <- Writer.tell(s"saw $s").at[Row]
        t <- State.set[Int](s + 1).at[Row]
      yield t - s).!?
    for x <- xs do State.modify[Int](_ + x).!?
    val s4 = State.get[Int].map(_ * 10).!?                                  // map: Bind into Pure
    s1 + s2 + s3 + s4
  }

  def stagedCompound(xs: List[Int], k: Int): Handled[Row, R, Int] = Direct.staged(sw) {
    val s1 = State.modify[Int](_ + k).!?
    val s2 = State.get[Int].flatMap(s => State.set[Int](s * 2)).!?
    val s3 = (for
        s <- State.get[Int].at[Row]
        _ <- Writer.tell(s"saw $s").at[Row]
        t <- State.set[Int](s + 1).at[Row]
      yield t - s).!?
    for x <- xs do State.modify[Int](_ + x).!?
    val s4 = State.get[Int].map(_ * 10).!?
    s1 + s2 + s3 + s4
  }

  test("v2: compound programs — modify, a flatMap chain, a for-comprehension, map — agree with the Free block") {
    val rnd = new scala.util.Random(20260923)
    for _ <- 1 to 300 do
      val xs = List.fill(rnd.nextInt(8))(rnd.nextInt(50))
      val k = rnd.nextInt(100)
      val s0 = rnd.nextInt(100)
      val (s, (log, a)) = State.run[Int, (Seq[String], Int)](s0)(Writer.run[String, Int, State % Int](freeCompound(xs, k)))
      val ((s2, log2), a2) = sw.run(s0)(stagedCompound(xs, k))
      assertEquals((s2, log2, a2), (s, log.toVector, a), s"xs=$xs k=$k s0=$s0")
  }

  test("v2: a program built at run time under a mark is refused, naming the shape") {
    val e = compileErrors("""
      val sw = Stager.StateWriter[Int, String, Int]()
      def opaque: Int ! State % Int + Writer % String = State.get[Int].at[State % Int + Writer % String]
      Direct.staged(sw) { opaque.!? }
    """)
    assert(e.contains("run time"), e)
  }

  test("a foreign monad under a mark is refused as in a Free block") {
    val e = compileErrors("""
      val sw = Stager.StateWriter[Int, String, Int]()
      Direct.staged(sw) { Option(1).!? }
    """)
    assert(e.contains("neither"), e)
  }
