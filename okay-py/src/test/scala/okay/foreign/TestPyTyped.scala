package okay.foreign

import okay.given
import okay.codec.Schema
import PyValue.*

object TestPyTyped:
  final case class Order(sku: String, qty: Int, price: Double) derives Schema
  final case class Total(sku: String, amount: Double, note: Option[String]) derives Schema
  enum Shape derives Schema:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)

  /** the module the tests call: written to a temp dir on PYTHONPATH */
  val module: String =
    """import dataclasses
      |
      |@dataclasses.dataclass
      |class Order:
      |    sku: str
      |    qty: int
      |    price: float
      |
      |def total(order):
      |    return {"sku": order["sku"], "amount": order["qty"] * order["price"], "note": None}
      |
      |def an_order():
      |    return Order("kyiv-7", 2, 1.5)
      |
      |def area(shape):
      |    if shape["type"] == "Circle":
      |        return 3.0 * shape["r"] ** 2
      |    return shape["w"] * shape["h"]
      |
      |def unit():
      |    return {"type": "Rect", "w": 1.0, "h": 1.0}
      |
      |def big():
      |    return 2 ** 70
      |
      |def echo(x):
      |    return x
      |
      |def doubled(frame):
      |    return {"sku": frame["sku"], "qty": [q * 2 for q in frame["qty"]], "price": frame["price"]}
      |""".stripMargin

/** foreign-typed-calls against a LIVE python3 (specs/foreign-highlevel.md stage 2) */
class TestPyTyped extends munit.FunSuite {
  import TestPyTyped.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private lazy val w =
    val dir = java.nio.file.Files.createTempDirectory("okay-py-typed")
    java.nio.file.Files.writeString(dir.resolve("okaytyped.py"), module): Unit
    PySubprocess.start(TestPy.python.get, Map("PYTHONPATH" -> dir.toString))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if TestPy.python.nonEmpty then w.close()

  test("a dict answered by a plain call reaches okay as a dict") {
    // two shapes, two failures before wire v2: a dict of scalars failed
    // INSIDE the shim (it was encoded as a frame, a column per key), and
    // a dict of lists crossed as a frame the host decoded as None
    val scalars = w.handler.handle(PyEval.Call("json:loads", Vector(Str("""{"a": 1}"""))))
    val lists = w.handler.handle(PyEval.Call("json:loads", Vector(Str("""{"a": [1, 2]}"""))))
    assertEquals(scalars, Right(Dict(Vector("a" -> I64(1)))))
    assertEquals(lists, Right(Dict(Vector("a" -> Arr(Vector(I64(1), I64(2)))))))
  }

  test("a case class in, a case class out: the Python function reads a dict") {
    val total = Py.fn[Total]("okaytyped:total")
    assertEquals(total(Order("kyiv-7", 3, 2.5)).runWith, Right(Total("kyiv-7", 7.5, None)))
  }

  test("a dataclass answered by Python decodes as the case class of the same shape") {
    assertEquals(Py.fn[Order]("okaytyped:an_order")().runWith, Right(Order("kyiv-7", 2, 1.5)))
  }

  test("a sum crosses as a dict whose 'type' names the case, both ways") {
    val area = Py.fn[Double]("okaytyped:area")
    assertEquals(area(Shape.Rect(2.0, 3.0): Shape).runWith, Right(6.0))
    assertEquals(area(Shape.Circle(1.0): Shape).runWith, Right(3.0))
    assertEquals(Py.fn[Shape]("okaytyped:unit")().runWith, Right(Shape.Rect(1.0, 1.0)))
  }

  test("integers past 2^53 cross exactly: a Long both ways, and a Python int past a Long as a BigInt") {
    val echo = Py.fn[Long]("okaytyped:echo")
    assertEquals(echo(Long.MaxValue).runWith, Right(Long.MaxValue))
    assertEquals(echo(9007199254740993L).runWith, Right(9007199254740993L))
    assertEquals(Py.fn[BigInt]("okaytyped:big")().runWith, Right(BigInt(2).pow(70)))
  }

  test("an answer of the wrong shape is a Decode condition naming the field") {
    val wrong = Py.fn[Order]("okaytyped:total")(Order("kyiv-7", 3, 2.5)).runWith
    assertEquals(wrong, Left(Condition("Decode", ".qty: missing")))
  }

  test("a Python exception and a decode failure arrive on the same Left") {
    val fails = Py.fn[Double]("okaytyped:area")("not a shape").runWith
    assertEquals(fails.left.map(_.kind), Left("TypeError"))
  }

  test("rows of a case class out as a frame, through a frame function, and back as rows") {
    val frame = PyFrame.of(Vector(Order("a", 1, 1.0), Order("b", 2, 2.0))).toOption.get
    val back = w.handler.handle(PyEval.Frame("okaytyped:doubled", frame, Vector.empty))
    assertEquals(back.flatMap(_.rows[Order]), Right(Vector(Order("a", 2, 1.0), Order("b", 4, 2.0))))
  }
}
