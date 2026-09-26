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
    from okay import okay_call

    def quote(sku, qty):
        price = okay_call("price_of", sku)
        return okay_call("discount", price * qty)

    # a TABLE call: the frame arrives as a dict of columns
    def scale(frame, k):
        return {"x": [v * k for v in frame["x"]]}

    # a HELD value, and a function reading it
    def counter(n):
        return {"n": n}

    def describe(c, k):
        return c["n"] + k
  """)

/** (Python, pipes), JSON, uncompressed by default: a pipe is not a network */
class TestPyPipes extends WireConformance:
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override def address(name: String): String = s"conf:$name"
  lazy val engine: ForeignWorker = ForeignWorker.start(TestPy.python.get, modules = Seq(PyConformance.conf))
  override def afterAll(): Unit = if TestPy.python.nonEmpty then engine.close()

  test("with no import, a pipe's wire is plain JSON (the default compresses on a network only)") {
    assertEquals(engine.wire, "json/none")
  }

  test("the Python shim claims no mux: served one exchange at a time, as before") {
    assert(!engine.muxed)
  }

/** (Python, pipes), DEFLATE asked for by an import */
class TestPyPipesDeflate extends WireConformance:
  import WireCompression.Deflate.given
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override def address(name: String): String = s"conf:$name"
  lazy val engine: ForeignWorker = ForeignWorker.start(TestPy.python.get, modules = Seq(PyConformance.conf))
  override def afterAll(): Unit = if TestPy.python.nonEmpty then engine.close()

  test("an explicit Deflate compresses on a pipe too") {
    assertEquals(engine.wire, "json/deflate")
  }

/** (Python, pipes), CBOR and DEFLATE chosen by givens */
class TestPyPipesCbor extends WireConformance:
  import WireFormat.Cbor.given
  import WireCompression.Deflate.given
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override def address(name: String): String = s"conf:$name"
  lazy val engine: ForeignWorker = ForeignWorker.start(TestPy.python.get, modules = Seq(PyConformance.conf))
  override def afterAll(): Unit = if TestPy.python.nonEmpty then engine.close()

/** (Python, pipes), the wire picked EXPLICITLY at runtime (`WireChoice`),
 * not by a compile-time given import */
class TestPyPipesWireChoice extends WireConformance:
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override def address(name: String): String = s"conf:$name"
  private val wire = okay.codec.WireChoice.named(format = "cbor", compression = "deflate")
    .fold(why => throw IllegalStateException(why), identity)
  lazy val engine: ForeignWorker = ForeignWorker.startWithWire(wire, TestPy.python.get, modules = Seq(PyConformance.conf))
  override def afterAll(): Unit = if TestPy.python.nonEmpty then engine.close()

  test("startWithWire applies WireChoice.named, exactly as the givens above would") {
    assertEquals(engine.wire, "cbor/deflate")
  }
