package okay.foreign

import java.nio.file.{Files, Path}
import okay.{Reader, given}
import okay.foreign.TestTsOneShape.{Order, Totals}

object TestTsTypedEffects:
  /** the operations a TypeScript module may perform, written ONCE, here, with their types */
  val priceOf = Ts.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
  val discount = Ts.callback[Double, Double]("discount")(amount => Reader.ask[Map[String, Double]].map(m => amount * m("rate")))

  // no margin: the docs quote these lines
  val typed: String = """
import { effects } from "./okay.ts";
import type { Order, Totals } from "./model.ts";
import type { ShopOps } from "./ops.ts";

const shop = effects<ShopOps>();

export function total(order: Order): Totals {
  const price: number = shop.call("price_of", order.sku);
  return { sku: order.sku, amount: shop.call("discount", price * order.qty), note: order.gift };
}
"""

/** typescript-types T12 against a LIVE Node and tsc: a module's effects in its TypeScript type */
class TestTsTypedEffects extends munit.FunSuite {
  import TestTsTypedEffects.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  private lazy val node = has("node", "--version")
  override def munitIgnore: Boolean = !node

  private lazy val dir: Path =
    val d = Files.createTempDirectory("okay-ts-typed-effects")
    Files.writeString(d.resolve("model.ts"), TestTsOneShape.declarations): Unit
    Files.writeString(d.resolve("ops.ts"), Ts.ops("ShopOps", Ts.callbacks(priceOf, discount))): Unit
    Files.writeString(d.resolve("typed.ts"), typed): Unit
    d
  private lazy val w = TsWorker.start(dir, modules = Seq("typed"))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if node then w.close()

  test("the operations' TypeScript type is generated from the Scala callbacks") {
    val ops = Ts.ops("ShopOps", Ts.callbacks(priceOf, discount))
    assert(ops.contains("export type ShopOps = {\n  price_of: (a0: string) => number;\n  discount: (a0: number) => number;\n};"), ops)
  }

  test("a module typed by its operations runs in the worker, performing them through okay's handlers") {
    val total = Ts.fn[Totals]("typed:total").calling(Ts.callbacks(priceOf, discount))(Order("tea", 3L, None))
    assertEquals(Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(total).runWith, Right(Totals("tea", 6.0, None)))
  }

  test("tsc --strict: an operation the module may not perform, or a wrong argument, does not compile") {
    assume(has("tsc", "--version"), "tsc is not installed")
    Files.writeString(dir.resolve("okay.ts"), TsWorker.library): Unit
    def tsc(file: String): (Boolean, String) =
      val p = ProcessBuilder("tsc", "--noEmit", "--strict", "--allowImportingTsExtensions",
        "--target", "es2022", "--module", "nodenext", file).directory(dir.toFile).redirectErrorStream(true).start()
      val said = String(p.getInputStream.readAllBytes())
      (p.waitFor() == 0, said)
    val (ok, out) = tsc("typed.ts")
    assert(ok, out)
    Files.writeString(dir.resolve("name.ts"), typed.replace("""shop.call("price_of", order.sku)""", """shop.call("prices_of", order.sku)""")): Unit
    val (nameOk, nameSaid) = tsc("name.ts")
    assert(!nameOk && nameSaid.contains("prices_of"), nameSaid)
    Files.writeString(dir.resolve("arg.ts"), typed.replace("""shop.call("price_of", order.sku)""", """shop.call("price_of", order.qty)""")): Unit
    val (argOk, argSaid) = tsc("arg.ts")
    assert(!argOk && argSaid.contains("not assignable to parameter of type 'string'"), argSaid)
  }
}
