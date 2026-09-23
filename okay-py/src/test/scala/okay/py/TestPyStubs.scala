package okay.py

import okay.codec.{Json, Schema, Stubs}
import PyValue.*

object TestPyStubs:
  final case class Line(sku: String, qty: Int, price: Double) derives Schema
  final case class Order(id: Long, lines: Vector[Line], note: Option[String], total: BigInt, raw: Array[Byte]) derives Schema
  enum Shape derives Schema:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)
  enum Tree derives Schema:
    case Leaf(v: Int)
    case Node(children: List[Tree])

/**
 * The generated Python declarations (schema-stubs) checked by the REAL
 * mypy, through uvx (Live): what PyCodec SENDS, written as a Python
 * literal, type-checks as the declared TypedDict; a read of a field that
 * does not exist is refused.
 */
class TestPyStubs extends munit.FunSuite {
  import TestPyStubs.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private lazy val uvx = scala.util.Try(ProcessBuilder("uvx", "--version").start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !uvx

  /** a PyValue as the Python literal it is */
  private def lit(v: PyValue): String = v match
    case PyNone => "None"
    case Bool(b) => if b then "True" else "False"
    case I64(n) => n.toString
    case BigI(n) => n.toString
    case F64(d) => d.toString
    case Str(s) => Json.print(Json.JStr(s))
    case Bytes(b) => b.map(_ & 0xff).mkString("bytes([", ", ", "])")
    case Arr(xs) => xs.map(lit).mkString("[", ", ", "]")
    case Dict(kv) => kv.map((k, x) => s"${Json.print(Json.JStr(k))}: ${lit(x)}").mkString("{", ", ", "}")
    case Ref(r) => throw IllegalArgumentException(s"a handle has no literal: $r")

  private def mypy(usage: String): (Int, String) =
    val dir = java.nio.file.Files.createTempDirectory("okay-stubs-py")
    val declarations = Stubs.python(summon[Schema[Order]], summon[Schema[Shape]], summon[Schema[Tree]])
    java.nio.file.Files.writeString(dir.resolve("model.py"), declarations): Unit
    java.nio.file.Files.writeString(dir.resolve("usage.py"), usage): Unit
    val p = ProcessBuilder("uvx", "mypy", "--strict", "usage.py").directory(dir.toFile).redirectErrorStream(true).start()
    val out = String(p.getInputStream.readAllBytes())
    (p.waitFor(), out)

  private val order = Order(7L, Vector(Line("tea", 2, 4.0)), None, BigInt("123456789012345678901234567890"), Array[Byte](1, 2))
  private val tree: Tree = Tree.Node(List(Tree.Leaf(1), Tree.Node(Nil)))

  test("what PyCodec sends type-checks as the declared TypedDict; a wrong read does not") {
    val ok = s"""from model import Order, Shape, Tree
                |
                |order: Order = ${lit(PyCodec.encode(order))}
                |shape: Shape = ${lit(PyCodec.encode(Shape.Rect(2, 3): Shape))}
                |tree: Tree = ${lit(PyCodec.encode(tree))}
                |
                |def qty(o: Order) -> int:
                |    return o["lines"][0]["qty"]
                |""".stripMargin
    val (code, out) = mypy(ok)
    assertEquals(code, 0, out)
    val (bad, why) = mypy(ok.replace("""o["lines"][0]["qty"]""", """o["lines"][0]["quantity"]"""))
    assertNotEquals(bad, 0)
    assert(why.contains("quantity"), why)
  }
}
