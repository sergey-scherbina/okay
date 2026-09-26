package okay.py

import okay.{Choose, Reader, effect, runChoice, given}

/**
 * ONE test body over every link (polyglot-one-wire): whatever the far side
 * is written in and however it is reached — a child process's pipes, a
 * socket, a call into this process — a program serving `pairs`, `total` and
 * `boom` must behave the same. That is the claim "one language", as a test.
 *
 * The far side's programs:
 *  - `pairs()`: two `choose` operations, answering their sum;
 *  - `total(sku, qty)`: `price_of(sku)`, then `discount(price * qty)`;
 *  - `boom()`: fails with a message containing "says no";
 *  - `quote(sku, qty)`: the same as `total`, in DIRECT STYLE — ordinary code
 *    calling `okay_call` twice (a far side without direct style overrides
 *    `direct` to false);
 *  - `scale(frame, k)`: a TABLE call — a frame with column `x` in, the same
 *    column times `k` out (a far side without table calls overrides `tables`);
 *  - `counter(n)` held, then `describe(c, k)`: `n + k` from the HELD value
 *    (a far side that keeps nothing overrides `holds`).
 */
abstract class WireConformance extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  /** how this far side addresses a served name (Python: `module:name`) */
  def address(name: String): String = name

  /** the value rules this far side is read by: Python's, or a language's
   * own (R's vectors-of-one are read by `R.shape`) — the body is the same */
  def shape: Shape = Shape.python
  given Shape = shape

  /** the engine over this suite's link, made once */
  def engine: ForeignWorker
  private given okay.Handler[ForeignEval] = engine.handler

  private val choose = Foreign.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))
  private val priceOf = Foreign.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
  private val discount = Foreign.callback[Double, Double]("discount")(a => Reader.ask[Map[String, Double]].map(m => a * m("rate")))

  test("MULTI-SHOT across the link: every branch of two choices") {
    val pairs = Foreign.program[Long](address("pairs")).calling(Foreign.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
    pairs.forget.runWith
  }

  test("each operation is a Scala callback, run under the caller's Reader") {
    val run = Foreign.program[Double](address("total")).calling(Foreign.callbacks(priceOf, discount))("tea", 3L)
    assertEquals(Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(run.program).runWith, Right(6.0))
  }

  /** whether the far side serves direct-style functions */
  def direct: Boolean = true

  test("DIRECT STYLE: far-side code calls okay_call(request) -> answer, under the caller's Reader") {
    assume(direct, "this far side serves programs only")
    val quote = Foreign.fn[Double](address("quote")).calling(Foreign.callbacks(priceOf, discount))("tea", 3L)
    assertEquals(Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(quote).runWith, Right(6.0))
  }

  /** whether the far side serves table calls (a frame as the first argument) */
  def tables: Boolean = true

  test("a TABLE call: a frame in, a frame out, by the far side's own codec (foreign-one-bulk)") {
    assume(tables, "this far side serves no table calls")
    val in = PyFrame(Vector("x" -> Vector(PyValue.I64(1), PyValue.I64(2))))
    val out = engine.handler.handle(ForeignEval.Frame(address("scale"), in, Vector(PyValue.I64(3))))
    // numbers compare as numbers: R and TypeScript may answer a double where Go answers an int
    val xs = out.map(_.cols.map((n, vs) => n -> vs.map {
      case PyValue.I64(v) => v.toDouble
      case PyValue.F64(v) => v
      case v => fail(s"not a number: $v")
    }))
    assertEquals(xs, Right(Vector("x" -> Vector(3.0, 6.0))))
  }

  /** whether the far side keeps values for the host (`call … held`) */
  def holds: Boolean = true

  test("a HELD value: kept on the far side, used twice by its ref, released, then refused by name (foreign-held-values)") {
    assume(holds, "this far side keeps no values")
    val h = engine.handler
    val ref = h.handle(ForeignEval.Call(address("counter"), Vector(PyValue.I64(10)), held = true))
      .flatMap(Wire.asRef).fold(c => fail(s"hold: $c"), identity)
    def describe(k: Long): Either[Condition, Double] =
      // read by this far side's rules: R answers a vector of one
      h.handle(ForeignEval.Call(address("describe"), Vector(PyValue.Ref(ref), PyValue.I64(k)))).flatMap(shape.decode[Double](_))
    assertEquals(describe(2), Right(12.0))
    assertEquals(describe(5), Right(15.0))
    h.handle(ForeignEval.Release(ref))
    assert(describe(1).isLeft, "a released value was still found")
  }

  /** whether a far-side failure leaves the far side alive: false for Rust
   * compiled to WebAssembly, where a panic is `abort` and traps the module */
  def survivesPanics: Boolean = true

  test("a far side that cannot survive a panic still reports it, with its message") {
    assume(!survivesPanics, "this far side survives its panics (the next test)")
    val boom = Foreign.program[Long](address("boom")).calling(Foreign.callbacks(choose))()
    val e = intercept[IllegalStateException](runChoice(boom.program).runWith)
    assert(e.getMessage.contains("says no"), e.getMessage)
  }

  test("a failure is a condition by name, and the far side lives on") {
    assume(survivesPanics, "a panic ends this far side (the test above)")
    val boom = Foreign.program[Long](address("boom")).calling(Foreign.callbacks(choose))()
    val got = runChoice(boom.program).runWith
    assert(got.headOption.exists(_.left.exists(c => c.kind.nonEmpty && c.message.contains("says no"))), s"$got")
    assertEquals(runChoice(Foreign.program[Long](address("pairs")).calling(Foreign.callbacks(choose))().program).runWith.size, 4)
  }
