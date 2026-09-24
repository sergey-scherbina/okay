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
 *  - `boom()`: fails with a message containing "says no".
 */
abstract class WireConformance extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  /** the engine over this suite's link, made once */
  def engine: PySubprocess
  private given okay.Handler[PyEval] = engine.handler

  private val choose = Py.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))
  private val priceOf = Py.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
  private val discount = Py.callback[Double, Double]("discount")(a => Reader.ask[Map[String, Double]].map(m => a * m("rate")))

  test("MULTI-SHOT across the link: every branch of two choices") {
    val pairs = Py.program[Long]("pairs").calling(Py.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
    pairs.forget.runWith
  }

  test("each operation is a Scala callback, run under the caller's Reader") {
    val run = Py.program[Double]("total").calling(Py.callbacks(priceOf, discount))("tea", 3L)
    assertEquals(Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(run.program).runWith, Right(6.0))
  }

  test("a failure is a condition by name, and the far side lives on") {
    val boom = Py.program[Long]("boom").calling(Py.callbacks(choose))()
    val got = runChoice(boom.program).runWith
    assert(got.headOption.exists(_.left.exists(c => c.kind.nonEmpty && c.message.contains("says no"))), s"$got")
    assertEquals(runChoice(Py.program[Long]("pairs").calling(Py.callbacks(choose))().program).runWith.size, 4)
  }
