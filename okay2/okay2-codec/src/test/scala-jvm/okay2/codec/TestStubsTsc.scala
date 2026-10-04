package okay2.codec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import StubModels._

/**
 * The generated `.d.ts` checked by the REAL TypeScript compiler (Live;
 * okay-codec's TestStubsTsc): what the JSON codec writes is assignable
 * to the declared type, and a read of a field that does not exist is
 * refused.
 */
class TestStubsTsc extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private lazy val tsc = scala.util.Try(new ProcessBuilder("tsc", "--version").start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !tsc

  private def check(usage: String): (Int, String) = {
    val dir = Files.createTempDirectory("okay-stubs-ts")
    val declarations = Stubs.typescript(Order.schema, Shape.schema, Tree.schema)
    val _ = Files.writeString(dir.resolve("model.d.ts"), declarations)
    val _ = Files.writeString(dir.resolve("usage.ts"), usage)
    val p = new ProcessBuilder("tsc", "--noEmit", "--strict", "usage.ts").directory(dir.toFile).redirectErrorStream(true).start()
    val out = new String(p.getInputStream.readAllBytes(), UTF_8)
    (p.waitFor(), out)
  }

  private val order = Order(7L, Vector(Line("tea", 2, 4.0)), None, BigInt("123456789012345678901234567890"), Array[Byte](1, 2))
  private val tree: Tree = Tree.Node(List(Tree.Leaf(1), Tree.Node(Nil)))

  test("what the JSON codec writes type-checks as the declared type; a wrong read does not") {
    val ok = s"""import type { Order, Shape, Tree } from "./model";
                |const order: Order = ${Json.write(order)};
                |const shape: Shape = ${Json.write[Shape](Shape.Rect(2, 3))};
                |const tree: Tree = ${Json.write(tree)};
                |const qty: number = order.lines[0].qty;
                |export { order, shape, tree, qty };
                |""".stripMargin
    val (code, out) = check(ok)
    assertEquals(code, 0, out)
    val (bad, why) = check(ok.replace("order.lines[0].qty", "order.lines[0].quantity"))
    assertNotEquals(bad, 0)
    assert(why.contains("quantity"), why)
  }
}
