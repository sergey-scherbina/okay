package okay.r

import okay.{Choose, Reader, effect, runChoice, given}
import okay.codec.{WireAuth, WireCompression, WireFormat}
import okay.py.ForeignGateway

/**
 * R ON THE NETWORK (foreign-one-r): once R's handler runs on the engine
 * every wire language shares, the gateway serves an R worker exactly as it
 * serves Python, TypeScript or Haskell — TCP, the default compression a
 * network link prefers, the HMAC challenge — and a timeout RECONNECTS to a
 * fresh R, whose programs as data are replayed as they are after a local
 * respawn. Before foreign-one-r none of this existed for R
 * (docs/one-language.md, "Limits": "R is not behind the gateway").
 */
class TestRNetwork extends munit.FunSuite {
  private val modules = Seq(TestRReplay.progs, TestRProgram.progs)

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private lazy val python: Option[String] =
    sys.env.getOrElse("PATH", "").split(":").iterator
      .map(d => java.nio.file.Path.of(d, "python3")).find(java.nio.file.Files.isExecutable(_)).map(_.toString)

  override def munitIgnore: Boolean = TestR.rscript.isEmpty || python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(4, "min")

  private val secret = "tea for two"

  private lazy val plain = ForeignGateway.start(RSubprocess.command(TestR.rscript.get, modules), python = python.get)
  private lazy val guarded = ForeignGateway.start(RSubprocess.command(TestR.rscript.get, modules),
    env = Map("OKAY_WIRE_SECRET" -> secret), python = python.get)

  override def afterAll(): Unit =
    if !munitIgnore then { plain._2.destroy(); guarded._2.destroy() }

  private val choose = R.callback[Vector[Double], Double]("choose")(xs => effect[Choose, Double](Choose(xs)))

  test("an R worker behind the gateway: a call, the network's preferred compression, a program resumed twice") {
    val r = RSubprocess.connect("127.0.0.1", plain._1)
    try
      // R announces zlib and not DEFLATE, so the network preference settles there
      assertEquals(r.wire, "json/zlib")
      assertEquals(r.handler.handle(REval.Call("sqrt", Vector(RValue.Vec(Vector(RValue.F64(9)))))),
        Right(RValue.Vec(Vector(RValue.F64(3)))))
      given okay.Handler[REval] = r.handler
      val pairs = R.program[Double]("rep::pairs").calling(R.callbacks(choose))()
      assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11.0), Right(21.0), Right(12.0), Right(22.0)))
      pairs.forget.runWith
    finally r.close()
  }

  test("behind a secret: the right one is served, a callback runs under the caller's Reader") {
    given WireAuth = WireAuth.secret(secret.getBytes)
    val r = RSubprocess.connect("127.0.0.1", guarded._1)
    try
      given okay.Handler[REval] = r.handler
      val price = R.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
      val run = R.program[Double]("progs::priced").calling(R.callbacks(price))("tea", 3.0)
      assertEquals(Reader.run(Map("tea" -> 4.0))(run.program).runWith, Right(12.0))
      run.forget.runWith
    finally r.close()
  }

  test("behind a secret: a wrong one, and none at all, are refused by name") {
    val wrong = intercept[IllegalStateException](RSubprocess.connect("127.0.0.1", guarded._1)(using
      summon[WireFormat], summon[WireCompression], WireAuth.secret("tea for one".getBytes))).getMessage
    assert(wrong.nonEmpty, wrong)
    val none = intercept[IllegalStateException](RSubprocess.connect("127.0.0.1", guarded._1)).getMessage
    assert(none.contains("authentication"), none)
  }

  test("a timeout over TCP RECONNECTS to a fresh R, and a multi-shot program is replayed onto it") {
    val r = RSubprocess.connect("127.0.0.1", plain._1, timeoutMillis = Some(3000L))
    given okay.Handler[REval] = r.handler
    var replaced = false
    val chooseLate = R.callback[Vector[Double], Double]("choose") { xs =>
      if !replaced && xs == Vector(10.0, 20.0) then
        replaced = true
        val late = r.handler.handle(REval.Call("rep::slow", Vector.empty))
        assert(late.left.exists(_.kind == "timeout"), late.toString)
      effect[Choose, Double](Choose(xs))
    }
    try
      val pairs = R.program[Double]("rep::pairs").calling(R.callbacks(chooseLate))()
      assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11.0), Right(21.0), Right(12.0), Right(22.0)))
      assert(replaced)
      pairs.forget.runWith
    finally r.close()
  }
}
