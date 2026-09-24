package okay.py

/** the conformance programs in Python: the same four as the Go and Rust workers */
object PyConformance:
  val conf = Foreign.module("conf", """
    import okay

    def pairs():
        return okay.perform("choose", [1, 2]).then(lambda x:
               okay.perform("choose", [10, 20]).then(lambda y:
               okay.done(x + y)))

    def total(sku, qty):
        return okay.perform("price_of", sku).then(lambda price:
               okay.perform("discount", price * qty))

    def boom():
        raise RuntimeError("python says no")

    # DIRECT STYLE: ordinary Python calling okay's effects
    def quote(sku, qty):
        price = okay.call("price_of", sku)
        return okay.call("discount", price * qty)
  """)

/** (Python, pipes), JSON, and DEFLATE by default */
class TestPyPipes extends WireConformance:
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override def address(name: String): String = s"conf:$name"
  lazy val engine: ForeignWorker = ForeignWorker.start(TestPy.python.get, modules = Seq(PyConformance.conf))
  override def afterAll(): Unit = if TestPy.python.nonEmpty then engine.close()

  test("with no import, the wire is JSON with DEFLATE") {
    assertEquals(engine.wire, "json/deflate")
  }

/** (Python, pipes), compression turned off by an import */
class TestPyPipesOff extends WireConformance:
  import WireCompression.Off.given
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override def address(name: String): String = s"conf:$name"
  lazy val engine: ForeignWorker = ForeignWorker.start(TestPy.python.get, modules = Seq(PyConformance.conf))
  override def afterAll(): Unit = if TestPy.python.nonEmpty then engine.close()

  test("Off keeps the plain JSON lines") {
    assertEquals(engine.wire, "json/none")
  }

/** (Python, pipes), CBOR and DEFLATE chosen by givens */
class TestPyPipesCbor extends WireConformance:
  import WireFormat.Cbor.given
  import WireCompression.Deflate.given
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override def address(name: String): String = s"conf:$name"
  lazy val engine: ForeignWorker = ForeignWorker.start(TestPy.python.get, modules = Seq(PyConformance.conf))
  override def afterAll(): Unit = if TestPy.python.nonEmpty then engine.close()
