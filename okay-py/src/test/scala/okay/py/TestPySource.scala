package okay.py

import okay.{!, %, +, Take, Writer, effect, pure, given}
import okay.Row.plus

object TestPySource:
  val m = Py.module("sources", """
    def rows(n, size):
        def gen():
            for i in range(0, n, size):
                yield list(range(i, min(i + size, n)))
        return gen()

    def broken():
        def gen():
            yield [1, 2]
            raise ValueError("source says no")
        return gen()
  """)

/**
 * A FAR-SIDE SOURCE (foreign-one-mux): a Python generator of chunks read at
 * the consumer's pace, derived from a held object and a call per chunk — no
 * operation of its own on the wire. Live, python3.
 */
class TestPySource extends munit.FunSuite:
  import TestPySource.m

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private lazy val w = ForeignWorker.start(TestPy.python.get, modules = Seq(m))
  override def afterAll(): Unit = if TestPy.python.nonEmpty then w.close()

  /** the worker's handler, counting each chunk asked for and each release */
  private final class Counting extends okay.Handler[ForeignEval]:
    var chunks = 0
    var released = 0
    var held: Option[PyRef] = None
    def handle[A](e: ForeignEval[A]): A =
      e match
        case ForeignEval.Call(Address.Method(r, "__next__"), _, _) => chunks += 1; held = Some(r)
        case ForeignEval.Release(_) => released += 1
        case _ => ()
      w.handler.handle(e)

  private def run[O](p: Unit ! PyStream.SourceRow[O], h: Counting): List[O] =
    Writer.run(Py.releasing(p)).runWith(using h)._1.toList

  /** a consumer that has had enough after four */
  private val takeFour: Unit ! Take % Long + Writer % Long =
    type S = Take % Long + Writer % Long
    def go(left: Int): Unit ! S =
      if left == 0 then pure(())
      else effect[S, Option[Long]](Take.Await()).flatMap {
        case Some(x) => effect[S, Unit](Writer(x)).flatMap(_ => go(left - 1))
        case None => pure(())
      }
    go(4)

  test("every element, in order, one call per chunk; the iterator released at its end") {
    val h = Counting()
    assertEquals(run(Py.source[Long]("sources:rows")(10L, 3L), h), (0L until 10L).toList)
    // four chunks, and the call that met StopIteration
    assertEquals((h.chunks, h.released), (5, 1))
  }

  test("BACK-PRESSURE: a consumer that takes four asks the far side for two chunks, not four") {
    val h = Counting()
    val out = run(okay.through(Py.source[Long]("sources:rows")(10L, 3L))(takeFour.plus[ForeignEval + Holding]), h)
    assertEquals(out, List(0L, 1L, 2L, 3L))
    assertEquals(h.chunks, 2, "the far side was read further than the consumer asked")
  }

  test("a consumer that stops early still gives the iterator back, once, when the scope ends (foreign-source-early-stop)") {
    val h = Counting()
    val out = run(okay.through(Py.source[Long]("sources:rows")(10000L, 3L))(takeFour.plus[ForeignEval + Holding]), h)
    assertEquals(out, List(0L, 1L, 2L, 3L))
    assertEquals(h.released, 1, "the stopped source kept its iterator on the far side")
    // and it is gone there: asking it for more is refused by name
    assert(w.handler.handle(ForeignEval.Call(Address.Method(h.held.get, "__next__"), Vector.empty)).isLeft)
  }

  test("a failure inside the generator ends the source naming it, and releases the iterator") {
    val h = Counting()
    val e = intercept[PyStream.Failed](run(Py.source[Long]("sources:broken")(), h))
    assertEquals(e.condition.kind, "ValueError")
    assert(e.condition.message.contains("source says no"), e.condition.message)
    assertEquals(h.released, 1)
  }
