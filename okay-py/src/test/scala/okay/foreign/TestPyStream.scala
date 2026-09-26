package okay.foreign

import okay.{!, %, +, Writer, given}
import okay.Row.plus

object TestPyStream:
  val m = Py.module("streamy", """
    calls = []

    def double(xs):
        calls.append(len(xs))
        return [x * 2 for x in xs]

    def evens(xs):
        return [x for x in xs if x % 2 == 0]

    def call_sizes():
        return calls

    class Windows:
        def __init__(self, n):
            self.n, self.buf = n, []
        def step(self, xs):
            out = []
            for x in xs:
                self.buf.append(x)
                if len(self.buf) == self.n:
                    out.append(sum(self.buf))
                    self.buf = []
            return out
        def flush(self):
            return [sum(self.buf)] if self.buf else []
  """)

/** foreign-streaming against a LIVE python3 (specs/foreign-highlevel.md stage 6) */
class TestPyStream extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private lazy val w = PySubprocess.start(TestPy.python.get, modules = Seq(TestPyStream.m))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if TestPy.python.nonEmpty then w.close()

  private def numbers(n: Int): Unit ! Writer % Long + PyEval =
    (1L to n.toLong).foldLeft(okay.pure[Writer % Long, Unit](()))((p, x) => p.flatMap(_ => Writer.tell(x))).plus[PyEval]

  private def run[O](p: Unit ! Writer % O + PyEval): List[O] = Writer.run(p).runWith._1.toList

  test("a function over a list as a stage: one call per chunk, a partial chunk flushed at the end") {
    val out = run(okay.through(numbers(10))(Py.stage[Long, Long]("streamy:double", chunk = 4)))
    assertEquals(out, (1L to 10L).map(_ * 2).toList)
    assertEquals(Py.fn[Vector[Long]]("streamy:call_sizes")().runWith, Right(Vector(4L, 4L, 2L)))
  }

  test("the answer may be shorter: a filter") {
    assertEquals(run(okay.through(numbers(9))(Py.stage[Long, Long]("streamy:evens", chunk = 3))), List(2L, 4L, 6L, 8L))
  }

  test("a held object as a stateful stage, flushed at the end") {
    val win = Py.hold("streamy:Windows")(3L).runWith.toOption.get
    val out = run(okay.through(numbers(8))(win.stage[Long, Long]("step", chunk = 2, finish = Some("flush"))))
    assertEquals(out, List(6L, 15L, 15L))   // 1+2+3, 4+5+6, and the rest 7+8
  }

  test("a Python failure ends the stage, naming the condition") {
    val bad = Py.stage[String, Long]("streamy:evens", chunk = 2)
    val src = okay.pure[Writer % String, Unit](()).flatMap(_ => Writer.tell("x")).plus[PyEval]
    val e = intercept[PyStream.Failed](run(okay.through(src)(bad)))
    assertEquals(e.condition.kind, "TypeError")
  }
}
