package okay.foreign

import okay.{Choose, Reader, effect, runChoice, given}
import okay.codec.{Json, Schema, Stubs}

object TestTsOneShape:
  final case class Order(sku: String, qty: Long, gift: Option[String]) derives Schema
  final case class Totals(sku: String, amount: Double, note: Option[String]) derives Schema
  enum Shape derives Schema:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)

  /** one declaration file, written ONCE, in Scala — the one an HTTP frontend
   * and okay-ts use too. A DEF: as an object `val` it summoned `Schema[Order]`
   * while `Order`'s own lazily derived Schema was initialising this object,
   * and the one thread waited on itself (a stall the gate's watchdog caught) */
  def declarations: String = Stubs.typescript(summon[Schema[Order]], summon[Schema[Totals]], summon[Schema[Shape]])

  // no margin: the docs quote these lines
  val shop: String = """
import { call, done, perform, then, type Prog } from "./okay.ts";
import type { Order, Shape, Totals } from "./model.ts";

export function total(order: Order): Totals {
  const price = call<number>("price_of", order.sku);
  return { sku: order.sku, amount: price * order.qty, note: order.gift };
}

export function area(s: Shape): number {
  return "Rect" in s ? s.Rect.w * s.Rect.h : 3 * s.Circle.r * s.Circle.r;
}

export function asJson(x: unknown): string {
  return JSON.stringify(x);
}

export function unit(): Shape {
  return { Circle: { r: 1 } };
}

export function pairs(): Prog<number> {
  return then(perform<number>("choose", [1, 2]), (x) =>
    then(perform<number>("choose", [10, 20]), (y) => done(x + y)));
}
"""

/** T1 of specs/typescript-types.md against a LIVE Node: one shape, one declaration */
class TestTsOneShape extends munit.FunSuite {
  import TestTsOneShape.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  private lazy val node = has("node", "--version")
  override def munitIgnore: Boolean = !node

  private lazy val dir =
    val d = java.nio.file.Files.createTempDirectory("okay-ts-one")
    java.nio.file.Files.writeString(d.resolve("model.ts"), declarations): Unit
    java.nio.file.Files.writeString(d.resolve("shop.ts"), shop): Unit
    d
  private lazy val w = TsWorker.start(dir, modules = Seq("shop"))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if node then w.close()

  test("what the worker receives IS the JSON an okay HTTP endpoint would send") {
    val order = Order("tea", 3L, Some("ribbon"))
    val seen = Ts.fn[String]("shop:asJson")(order).runWith
    assertEquals(seen.map(Json.parse), Right(Json.parse(Json.encode(summon[Schema[Order]])(order))))
    val shape: Shape = Shape.Rect(2, 3)
    assertEquals(Ts.fn[String]("shop:asJson")(shape).runWith, Right("""{"Rect":{"w":2,"h":3}}"""))
  }

  test("a call typed only by Stubs.typescript: a case class, an Option, a callback, a sum both ways") {
    val priceOf = Ts.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
    val total = Ts.fn[Totals]("shop:total").calling(Ts.callbacks(priceOf))(Order("tea", 3L, None))
    assertEquals(Reader.run(Map("tea" -> 4.0))(total).runWith, Right(Totals("tea", 12.0, None)))
    assertEquals(Ts.fn[Double]("shop:area")(Shape.Rect(2, 3): Shape).runWith, Right(6.0))
    assertEquals(Ts.fn[Shape]("shop:unit")().runWith, Right(Shape.Circle(1)))
  }

  test("programs as data, multi-shot, in the JSON shape") {
    val choose = Ts.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))
    val pairs = Ts.program[Long]("shop:pairs").calling(Ts.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
  }

  test("tsc --strict: the module typed by the ONE declaration file compiles; a wrong field does not") {
    assume(has("tsc", "--version"), "tsc is not installed")
    def tsc(file: String): (Int, String) =
      java.nio.file.Files.writeString(dir.resolve("okay.ts"), TsWorker.library): Unit
      val p = ProcessBuilder("tsc", "--noEmit", "--strict", "--allowImportingTsExtensions",
        "--target", "es2022", "--module", "nodenext", file).directory(dir.toFile).redirectErrorStream(true).start()
      (p.waitFor(), String(p.getInputStream.readAllBytes()))
    val (ok, out) = tsc("shop.ts")
    assertEquals(ok, 0, out)
    java.nio.file.Files.writeString(dir.resolve("bad.ts"), shop.replace("s.Rect.w", "s.Rect.width")): Unit
    val (bad, why) = tsc("bad.ts")
    assertNotEquals(bad, 0)
    assert(why.contains("width"), why)
  }
}
