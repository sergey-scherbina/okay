package okay.foreign

import okay.{!, %, +, Choose, Reader, Take, Writer, effect, pure, runChoice, given}
import okay.Row.plus

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
 *    (a far side that keeps nothing overrides `holds`);
 *  - `await_open(name)` answers once `open(name)` has been called — on a
 *    worker that claims `mux` (Go), each served while the other waits;
 *  - `numbers(n, size, tag)` DRIVES a stream of `0 until n` in chunks of
 *    `size`, `emitted_of(tag)` says how many chunks it has sent and
 *    `stopped_of(tag)` whether it has returned;
 *  - `sum_after(tag)` reads the HOST's stream once `open(tag)` ran, and sends
 *    its sum; `dedup` answers each chunk in with what it had not seen.
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

  test("MUX: a call that waits for another is answered once the other arrives, on ONE worker (foreign-mux-duplex)") {
    assume(engine.muxed, "this far side is served one exchange at a time")
    import scala.concurrent.{Await, ExecutionContext, Future}
    import scala.concurrent.duration.DurationInt
    given ExecutionContext = ExecutionContext.global
    val name = s"g${System.nanoTime}"
    val waiting = Future(engine.handler.handle(ForeignEval.Call(address("await_open"), Vector(PyValue.Str(name)))))
    // best effort to send the waiting call first: if it ran second it would
    // not wait at all, and the test would say nothing; if it runs first on a
    // wire that is not multiplexed, `open` is never read and this times out
    Thread.sleep(200)
    // `open` in a future too: on a wire that is not multiplexed it would
    // block for ever, and the test must fail in seconds, not at the suite's timeout
    val opened = Future(engine.handler.handle(ForeignEval.Call(address("open"), Vector(PyValue.Str(name)))))
    assertEquals(Await.result(opened, 30.seconds), Right(PyValue.Str(name)))
    assertEquals(Await.result(waiting, 30.seconds), Right(PyValue.Str(name)))
  }

  /** a stream that stalls must fail in seconds, not at the suite's timeout */
  private def within[X](body: => X): X =
    import scala.concurrent.{Await, ExecutionContext, Future}
    import scala.concurrent.duration.DurationInt
    Await.result(Future(body)(using ExecutionContext.global), 30.seconds)

  private def emittedOf(tag: String): Long =
    engine.handler.handle(ForeignEval.Call(address("emitted_of"), Vector(PyValue.Str(tag)))) match
      case Right(v) => shape.decode[Long](v).getOrElse(-1L)
      case Left(c) => fail(s"emitted_of: $c")

  /** `emitted_of(tag)` once it stops moving (a far side's goroutine may be
   * a moment behind the host) */
  private def settled(tag: String): Long =
    var last = -1L
    var now = emittedOf(tag)
    val until = System.nanoTime() + 5_000_000_000L
    while now != last && System.nanoTime() < until do
      last = now
      Thread.sleep(150)
      now = emittedOf(tag)
    now

  test("STREAM: the far side drives it, every element in order, under a credit of two (foreign-mux-duplex part 3)") {
    assume(engine.muxed, "this far side is served one exchange at a time")
    val tag = s"s${System.nanoTime}"
    val out = within(Writer.run(Foreign.releasing(Foreign.stream[Long](address("numbers"), credit = 2)(100L, 7L, tag))).runWith(using engine.handler)._1)
    assertEquals(out.toList, (0L until 100L).toList)
  }

  test("STREAM: the far side runs ahead by the credit and no further — counted on the far side") {
    assume(engine.muxed, "this far side is served one exchange at a time")
    val h = engine.handler
    val tag = s"c${System.nanoTime}"
    val s = System.nanoTime()
    assertEquals(h.handle(ForeignEval.Stream(s, address("numbers"), Vector(PyValue.I64(1000), PyValue.I64(1), PyValue.Str(tag)), 3)), Right(()))
    assertEquals(settled(tag), 3L, "with nothing taken, the far side sent more than its credit")
    assertEquals(h.handle(ForeignEval.Pull(s)).map(_.isDefined), Right(true))
    assertEquals(settled(tag), 4L, "one chunk taken is one more chunk allowed")
    h.handle(ForeignEval.Cancel(s))
  }

  test("STREAM: a consumer that stops early cancels it, and the far side stops producing") {
    assume(engine.muxed, "this far side is served one exchange at a time")
    val tag = s"e${System.nanoTime}"
    type S = Take % Long + Writer % Long
    def take(left: Int): Unit ! S =
      if left == 0 then pure(())
      else effect[S, Option[Long]](Take.Await()).flatMap {
        case Some(x) => effect[S, Unit](Writer(x)).flatMap(_ => take(left - 1))
        case None => pure(())
      }
    val consumer = okay.through(Foreign.stream[Long](address("numbers"), credit = 2)(100000L, 1L, tag))(take(4).plus[ForeignEval + Holding])
    assertEquals(within(Writer.run(Foreign.releasing(consumer)).runWith(using engine.handler)._1.toList), List(0L, 1L, 2L, 3L))
    val sent = settled(tag)
    assert(sent <= 4 + 2, s"the far side sent $sent chunks for a consumer that took four at a credit of two")
    // cancelled, its Emit fails and the function returns — not left blocked for ever
    val until = System.nanoTime() + 5_000_000_000L
    def stopped = engine.handler.handle(ForeignEval.Call(address("stopped_of"), Vector(PyValue.Str(tag)))) == Right(PyValue.Bool(true))
    while !stopped && System.nanoTime() < until do Thread.sleep(50)
    assert(stopped, "the stream's function is still blocked: the early stop did not cancel it")
  }

  test("FED: the host feeds the far function, never more than its credit ahead — counted on the host (foreign-host-streams)") {
    assume(engine.muxed, "this far side is served one exchange at a time")
    val tag = s"f${System.nanoTime}"
    val fed = java.util.concurrent.atomic.AtomicInteger()
    val input = Iterator.tabulate(1000)(i => { fed.incrementAndGet(); i.toLong })
    import scala.concurrent.{Await, ExecutionContext, Future}
    import scala.concurrent.duration.DurationInt
    val summed = Future(Writer.run(Foreign.releasing(Foreign.stream[Long](address("sum_after"), credit = 3)
      .feeding(input, chunk = 1)(tag))).runWith(using engine.handler)._1)(using ExecutionContext.global)
    // nothing is taken until the gate opens: the host holds at the credit
    var last = -1
    while fed.get != last do { last = fed.get; Thread.sleep(200) }
    assert(fed.get <= 3 + 1, s"the host fed ${fed.get} chunks ahead of a far side that took none, at a credit of three")
    assertEquals(engine.handler.handle(ForeignEval.Call(address("open"), Vector(PyValue.Str(tag)))), Right(PyValue.Str(tag)))
    assertEquals(Await.result(summed, 30.seconds).toList, List((0L until 1000L).sum))
    assertEquals(fed.get, 1000)
  }

  test("DUPLEX: a dedup both ways at once — 20 000 rows in, the distinct ones out, in order") {
    assume(engine.muxed, "this far side is served one exchange at a time")
    val input = Iterator.tabulate(20000)(i => (i % 1000).toLong)
    val out = within(Writer.run(Foreign.releasing(Foreign.stream[Long](address("dedup"), credit = 2)
      .feeding(input, chunk = 512)())).runWith(using engine.handler)._1)
    assertEquals(out.toList, (0L until 1000L).toList)
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
