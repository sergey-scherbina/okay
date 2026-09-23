package okay.py

import okay.given

object TestPyHandles:
  val module: String =
    """class Acc:
      |    def __init__(self):
      |        self.total = 0
      |    def add(self, x):
      |        self.total += x
      |        return self.total
      |    def fork(self):
      |        a = Acc()
      |        a.total = self.total
      |        return a
      |
      |def acc():
      |    return Acc()
      |
      |def total_of(a):
      |    return a.total
      |""".stripMargin

/** foreign-object-handles against a LIVE python3 (specs/foreign-highlevel.md stage 3) */
class TestPyHandles extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private lazy val dir =
    val d = java.nio.file.Files.createTempDirectory("okay-py-h")
    java.nio.file.Files.writeString(d.resolve("okayh.py"), TestPyHandles.module): Unit
    d
  private lazy val w = PySubprocess.start(TestPy.python.get, Map("PYTHONPATH" -> dir.toString))
  override def afterAll(): Unit = if TestPy.python.nonEmpty then w.close()

  test("a seeded Random held in Python: its state lives there between calls") {
    given okay.Handler[PyEval] = w.handler
    val rng = Py.hold("random:Random")(42L).runWith.toOption.get
    assertEquals(rng.pyType, "random.Random")
    val draws = (1 to 2).map(_ => rng.call[Double]("random")().runWith)
    // Python's own Mersenne Twister, seeded 42: random.Random(42).random() twice
    assertEquals(draws, Vector(Right(0.6394267984578837), Right(0.025010755222666936)))
  }

  test("an object's methods, its attribute, the object as an argument, and a held method result") {
    given okay.Handler[PyEval] = w.handler
    val acc = Py.hold("okayh:acc")().runWith.toOption.get
    assertEquals(acc.call[Long]("add")(5L).runWith, Right(5L))
    assertEquals(acc.call[Long]("add")(3L).runWith, Right(8L))
    assertEquals(acc.attr[Long]("total").runWith, Right(8L))
    assertEquals(Py.fn[Long]("okayh:total_of")(acc).runWith, Right(8L))
    val fork = acc.hold("fork")().runWith.toOption.get
    assertEquals(fork.call[Long]("add")(1L).runWith, Right(9L))
    assertEquals(acc.attr[Long]("total").runWith, Right(8L), "the fork is its own object")
  }

  test("a released ref is refused by name") {
    given okay.Handler[PyEval] = w.handler
    val acc = Py.hold("okayh:acc")().runWith.toOption.get
    acc.release.runWith
    val after = acc.call[Long]("add")(1L).runWith
    assertEquals(after.left.map(_.kind), Left("LookupError"))
    assert(after.left.exists(_.message.contains("not held")), s"$after")
    acc.release.runWith   // idempotent
  }

  test("a pool of ONE: a held object's calls reach its worker, and plain calls still get through") {
    val pool = PyWorkers.start(1, TestPy.python.get, Map("PYTHONPATH" -> dir.toString))
    try
      given okay.Handler[PyEval] = pool.handler
      val acc = Py.hold("okayh:acc")().runWith.toOption.get
      assertEquals(acc.call[Long]("add")(2L).runWith, Right(2L))
      assertEquals(Py.fn[Double]("math:sqrt")(16.0).runWith, Right(4.0))
      assertEquals(Py.fn[Long]("okayh:total_of")(acc).runWith, Right(2L))
      acc.release.runWith
      // a ref the POOL no longer knows is refused before any worker is asked
      val gone = intercept[IllegalArgumentException](Py.fn[Long]("okayh:total_of")(acc).runWith)
      assert(gone.getMessage.contains("not held by this pool"), gone.getMessage)
    finally pool.close()
  }

  test("a pool of two: refs are renamed pool-wide and each reaches its own worker") {
    val pool = PyWorkers.start(2, TestPy.python.get, Map("PYTHONPATH" -> dir.toString))
    try
      given okay.Handler[PyEval] = pool.handler
      val a = Py.hold("okayh:acc")().runWith.toOption.get
      val b = Py.hold("okayh:acc")().runWith.toOption.get
      assertNotEquals(a.id, b.id)
      assertEquals(a.call[Long]("add")(10L).runWith, Right(10L))
      assertEquals(b.call[Long]("add")(1L).runWith, Right(1L))
      assertEquals(a.attr[Long]("total").runWith, Right(10L))
    finally pool.close()
  }
}
