package okay.ts

import scala.scalajs.js
import okay.{!, Async, pure}
import okay.codec.Schema

object TestTsExport:
  final case class Order(sku: String, qty: Int) derives Schema
  final case class Totals(sku: String, amount: Double, note: Option[String]) derives Schema

  val prices = Map("tea" -> 3.0)

  val shop = Ts.module("shop")(
    Ts.expose[Order, Totals]("total")(o =>
      pure[Async, Totals](Totals(o.sku, prices.getOrElse(o.sku, 0.0) * o.qty, None))),
    Ts.expose[Vector[String], Int]("count")(skus => pure[Async, Int](skus.size)),
  )

  /** Node's own modules, where the test runs */
  def node(name: String): js.Dynamic = js.Dynamic.global.process.getBuiltinModule(name)

/** typescript-types T6: okay functions on Scala.js, called from TypeScript */
class TestTsExport extends munit.FunSuite {
  import TestTsExport.*
  given scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  private def call(fn: String, arg: js.Any): js.Promise[js.Any] =
    js.Promise.resolve[js.Any](shop.js.applyDynamic(fn)(arg))

  test("a TypeScript caller awaits an okay function: the argument and the answer in the JSON codec's shape") {
    call("total", js.Dynamic.literal(sku = "tea", qty = 4)).toFuture
      .map(v => assertEquals(js.JSON.stringify(v), """{"sku":"tea","amount":12,"note":null}"""))
  }

  test("a wrong argument rejects the promise with a TypeError naming the function") {
    call("total", js.Dynamic.literal(sku = "tea")).toFuture.transform {
      case scala.util.Failure(js.JavaScriptException(e: js.Error)) =>
        assertEquals(e.name, "TypeError")
        assert(e.message.startsWith("total: "), e.message)
        scala.util.Success(())
      case other => scala.util.Failure(AssertionError(s"expected a rejection, got $other"))
    }
  }

  test("the declaration: the codec's types and one signature per exposed function") {
    val d = shop.declaration
    assert(d.contains("export interface Order {\n  sku: string;\n  qty: Int;\n}"), d)
    assert(d.contains("""export declare const shop: {
  total(input: Order): Promise<Totals>;
  count(input: string[]): Promise<Int>;
};"""), d)
  }

  test("tsc --strict accepts a caller written against the declaration and refuses a wrong field".tag(new munit.Tag("Live"))) {
    val cp = node("child_process")
    val fs = node("fs")
    val has = (cp.spawnSync("tsc", js.Array("--version")).status: Any) == 0
    assume(has, "tsc is not installed")
    val dir = fs.mkdtempSync(node("path").join(node("os").tmpdir(), "okay-ts-export-")).toString
    def write(f: String, s: String): Unit = fs.writeFileSync(s"$dir/$f", s): Unit
    write("package.json", """{"type":"module"}""")
    write("shop.ts", shop.declaration)
    write("usage.ts", """import { shop, type Totals } from "./shop.ts";
const t: Totals = await shop.total({ sku: "tea", qty: 4 });
const n: number = await shop.count(["tea", "milk"]);
console.log(t.amount, n);
""")
    write("wrong.ts", """import { shop } from "./shop.ts";
const t = await shop.total({ sku: "tea", qty: 4 });
console.log(t.price);
""")
    def tsc(f: String): (Boolean, String) =
      val r = cp.spawnSync("tsc", js.Array("--noEmit", "--strict", "--allowImportingTsExtensions",
        "--target", "es2022", "--module", "nodenext", f), js.Dynamic.literal(cwd = dir, encoding = "utf8"))
      ((r.status: Any) == 0, String.valueOf(r.stdout))
    val (ok, out) = tsc("usage.ts")
    assert(ok, out)
    val (bad, why) = tsc("wrong.ts")
    assert(!bad, why)
    assert(why.contains("price"), why)
  }
}
