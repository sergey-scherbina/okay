package okay

import okay.Direct.*

/**
 * specs/direct-stagers.md: `Stager.All` over Reader + State + Writer +
 * Throws, and the four singles — each staged block agrees with the
 * SAME block text as a Free `direct` block run by the shipping
 * runners, on generated data.
 */
class TestStagers extends munit.FunSuite:

  case class Env(bias: Int, limit: Int)

  // ---- All, on the full row

  type Row4 = Reader % Env + State % Int + Writer % String + Throws % String
  val all = Stager.All[Env, Int, String, String, Int]()

  def free4(xs: List[Int]): Int ! Row4 = direct[[A] =>> A ! Row4] {
    val env = Reader.ask[Env].?
    Writer.tell(s"bias ${env.bias}").?
    for x <- xs do
      val s = State.get[Int].?
      val s2 = State.set[Int](s + x + env.bias).?
      Writer.tell(s"set $s2").?
      if s2 > env.limit then raise[String, Unit](s"over $s2").?
    val fin = State.get[Int].?
    fin * 2
  }

  def staged4(xs: List[Int]): Handled[Row4, all.R, Int] = Direct.staged(all) {
    val env = Reader.ask[Env].?
    Writer.tell(s"bias ${env.bias}").?
    for x <- xs do
      val s = State.get[Int].?
      val s2 = State.set[Int](s + x + env.bias).?
      Writer.tell(s"set $s2").?
      if s2 > env.limit then raise[String, Unit](s"over $s2").?
    val fin = State.get[Int].?
    fin * 2
  }

  def runFree4(xs: List[Int], env: Env, s0: Int): (Int, Vector[String], Either[String, Int]) =
    val (s, (log, a)) = State.run[Int, (Seq[String], Either[String, Int])](s0)(
      Writer.run[String, Either[String, Int], State % Int](
        runEither[Int, State % Int + Writer % String, String](
          Reader.run[Env, Int, State % Int + Writer % String + Throws % String](env)(free4(xs)))))
    (s, log.toVector, a)

  test("All: a four-effect block agrees with the Free block on generated data — state, log, answer or error") {
    val rnd = new scala.util.Random(20260923)
    for _ <- 1 to 300 do
      val xs = List.fill(rnd.nextInt(10))(rnd.nextInt(40))
      val env = Env(rnd.nextInt(5), 60 + rnd.nextInt(100))
      val s0 = rnd.nextInt(30)
      val ((s, log), a) = all.run(env, s0)(staged4(xs))
      assertEquals((s, log, a), runFree4(xs, env, s0), s"xs=$xs env=$env s0=$s0")
  }

  test("a raise mid-loop ends the block there: state and log at the raise, nothing after it ran") {
    var after = 0
    val g = Direct.staged(all) {
      var i = 0
      while i < 5 do
        i += 1
        val _ = State.set[Int](i).?
        Writer.tell(s"i=$i").?
        if i == 3 then raise[String, Unit]("three").?
        after += 1
      0
    }
    val ((s, log), a) = all.run(Env(0, 0), 0)(g)
    assertEquals(a, Left("three"))
    assertEquals(s, 3)
    assertEquals(log, Vector("i=1", "i=2", "i=3"))
    assertEquals(after, 2)
  }

  // ---- All on a subrow: Reader + Throws through Unit/Nothing slots

  type RowRT = Reader % Env + Throws % String
  val rt = Stager.All[Env, Unit, Nothing, String, Int]()

  def freeRT(xs: List[Int]): Int ! RowRT = direct[[A] =>> A ! RowRT] {
    val env = Reader.ask[Env].?
    var acc = env.bias
    for x <- xs do
      acc += x
      if acc > env.limit then raise[String, Unit](s"over $acc").?
    acc
  }

  def stagedRT(xs: List[Int]): Handled[rt.Row, rt.R, Int] = Direct.staged(rt) {
    val env = Reader.ask[Env].?
    var acc = env.bias
    for x <- xs do
      acc += x
      if acc > env.limit then raise[String, Unit](s"over $acc").?
    acc
  }

  test("All on a subrow: Reader + Throws through Unit/Nothing slots agrees with Reader.run + runEither") {
    val rnd = new scala.util.Random(20260924)
    for _ <- 1 to 300 do
      val xs = List.fill(rnd.nextInt(10))(rnd.nextInt(40))
      val env = Env(rnd.nextInt(5), 60 + rnd.nextInt(100))
      val expected = !.run(runEither[Int, okay.Pure, String](Reader.run[Env, Int, Throws % String](env)(freeRT(xs))))
      val (_, a) = rt.run(env, ())(stagedRT(xs))
      assertEquals(a, expected, s"xs=$xs env=$env")
  }

  test("All on State + Writer agrees with StateWriter on the same block") {
    val sw = Stager.StateWriter[Int, String, Int]()
    val aw = Stager.All[Unit, Int, String, Nothing, Int]()
    def viaSw(xs: List[Int]) = Direct.staged(sw) {
      for x <- xs do
        val s = State.get[Int].?
        val _ = State.set[Int](s + x).?
        Writer.tell(s"$s").?
      State.get[Int].?
    }
    def viaAll(xs: List[Int]) = Direct.staged(aw) {
      for x <- xs do
        val s = State.get[Int].?
        val _ = State.set[Int](s + x).?
        Writer.tell(s"$s").?
      State.get[Int].?
    }
    val rnd = new scala.util.Random(20260925)
    for _ <- 1 to 200 do
      val xs = List.fill(rnd.nextInt(10))(rnd.nextInt(40))
      val s0 = rnd.nextInt(30)
      val ((s, log), a) = sw.run(s0)(viaSw(xs))
      val ((s2, log2), a2) = aw.run((), s0)(viaAll(xs))
      assertEquals((s2, log2, a2), (s, log, Right(a)))
  }

  // ---- the singles

  test("Reading: a Reader-only block") {
    val rd = Stager.Reading[Env, Int]()
    def p(k: Int) = Direct.staged(rd) {
      val e = Reader.ask[Env].?
      val e2 = Reader.ask[Env].?
      e.bias * k + e2.limit
    }
    assertEquals(rd.run(Env(3, 7))(p(10)), 37)
    assertEquals(rd.run(Env(3, 7))(p(10)), !.run(Reader.run[Env, Int, okay.Pure](Env(3, 7))(direct[[A] =>> A ! Reader % Env] {
      val e = Reader.ask[Env].?
      val e2 = Reader.ask[Env].?
      e.bias * 10 + e2.limit
    })))
  }

  test("Stateful: a State-only block, on generated data") {
    val st = Stager.Stateful[Int, Int]()
    def p(xs: List[Int]) = Direct.staged(st) {
      for x <- xs do
        val s = State.get[Int].?
        val _ = State.set[Int](s * 2 + x).?
      State.get[Int].?
    }
    val rnd = new scala.util.Random(20260926)
    for _ <- 1 to 100 do
      val xs = List.fill(rnd.nextInt(8))(rnd.nextInt(10))
      val s0 = rnd.nextInt(10)
      val expected = xs.foldLeft(s0)((s, x) => s * 2 + x)
      assertEquals(st.run(s0)(p(xs)), (expected, expected))
  }

  test("Logging: a Writer-only block") {
    val lg = Stager.Logging[String, Int]()
    val p = Direct.staged(lg) {
      Writer.tell("a").?
      for x <- List(1, 2) do Writer.tell(s"x$x").?
      Writer.tell("z").?
      42
    }
    assertEquals(lg.run(p), (Vector("a", "x1", "x2", "z"), 42))
  }

  test("Failing: a Throws-only block, both roads") {
    val fl = Stager.Failing[String, Int]()
    def p(n: Int) = Direct.staged(fl) {
      if n < 0 then raise[String, Unit]("negative").?
      val half = if n % 2 == 0 then n / 2 else raise[String, Int]("odd").?
      half + 1
    }
    assertEquals(fl.run(p(4)), Right(3))
    assertEquals(fl.run(p(3)), Left("odd"))
    assertEquals(fl.run(p(-2)), Left("negative"))
  }

  test("docs/direct-style.md, Layer 2½ — the Reader + Throws example, verbatim") {
    case class Cfg(k: Int, limit: Int)
    val rt = Stager.All[Cfg, Unit, Nothing, String, Int]()   // Reader + Throws, nothing else

    def total(xs: List[Int]): Handled[rt.Row, rt.R, Int] = Direct.staged(rt) {
      val cfg = Reader.ask[Cfg].?
      var acc = 0
      for x <- xs do
        acc += x * cfg.k
        if acc > cfg.limit then raise[String, Unit](s"over $acc").?   // ends the block: Left
      acc
    }

    @scala.annotation.nowarn("msg=unused value|discarded non-Unit value")
    def docStagerDemo(): Unit =
      rt.run(Cfg(2, 100), ())(total(List(1, 2, 3)))._2     // Right(12)
      rt.run(Cfg(2, 5), ())(total(List(1, 2, 3)))._2       // Left("over 6")
    docStagerDemo()
    assertEquals(rt.run(Cfg(2, 100), ())(total(List(1, 2, 3)))._2, Right(12))
    assertEquals(rt.run(Cfg(2, 5), ())(total(List(1, 2, 3)))._2, Left("over 6"))
  }
