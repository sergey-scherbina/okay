package okay.py

import okay.{Choose, Reader, effect, runChoice, given}
import okay.codec.{Schema, Stubs}

object TestTsWorker:
  final case class Order(sku: String, qty: Long) derives Schema
  final case class Totals(sku: String, amount: Double, note: Option[String]) derives Schema
  enum Shape derives Schema:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)

  // no margin: the docs quote these lines
  val shop: String = """import { call, done, perform, then, type Prog } from "./okay.ts";
import type { Order, Shape, Totals } from "./model.ts";

export function total(order: Order): Totals {
  const price = call<number>("price_of", order.sku);
  return { sku: order.sku, amount: price * Number(order.qty), note: null };
}

export function area(s: Shape): number {
  return s.type === "Circle" ? 3 * s.r * s.r : s.w * s.h;
}

export async function later(x: number): Promise<number> {
  return x * 2;
}

export function echo(x: unknown): unknown {
  return x;
}

export function fail(): never {
  throw new RangeError("typescript says no");
}

export class Counter {
  n = 0;
  add(x: number): number {
    this.n += x;
    return this.n;
  }
}

export function counter(): Counter {
  return new Counter();
}

export function pairs(): Prog<number> {
  return then(perform<number>("choose", [1, 2]), (x) =>
    then(perform<number>("choose", [10, 20]), (y) => done(x + y)));
}
"""

/** the TypeScript worker against a LIVE Node (specs/typescript.md, stage 1) */
class TestTsWorker extends munit.FunSuite {
  import TestTsWorker.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  private lazy val node = has("node", "--version")
  override def munitIgnore: Boolean = !node

  private lazy val dir =
    val d = java.nio.file.Files.createTempDirectory("okay-ts")
    val model = Stubs.typescriptWire(summon[Schema[Order]], summon[Schema[Totals]], summon[Schema[Shape]])
    java.nio.file.Files.writeString(d.resolve("model.ts"), model): Unit
    java.nio.file.Files.writeString(d.resolve("shop.ts"), shop): Unit
    d
  private lazy val w = TsWorker.start(dir, modules = Seq("shop"))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if node then w.close()

  test("a typed call whose TypeScript calls back into okay's Reader") {
    val priceOf = Py.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
    val total = Py.fn[Totals]("shop:total").calling(Py.callbacks(priceOf))(Order("tea", 3L))
    assertEquals(Reader.run(Map("tea" -> 4.0))(total).runWith, Right(Totals("tea", 12.0, None)))
  }

  test("a sum crosses with its type field; an async function is awaited") {
    assertEquals(Py.fn[Double]("shop:area")(Shape.Rect(2, 3): Shape).runWith, Right(6.0))
    assertEquals(Py.fn[Long]("shop:later")(21L).runWith, Right(42L))
  }

  test("a held TypeScript object: its method and its field") {
    val counter = Py.hold("shop:counter")().runWith.toOption.get
    assertEquals(counter.pyType, "Counter")
    assertEquals(counter.call[Long]("add")(5L).runWith, Right(5L))
    assertEquals(counter.attr[Long]("n").runWith, Right(5L))
  }

  test("programs as data from TypeScript, MULTI-SHOT under okay's Choice") {
    val choose = Py.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))
    val pairs = Py.program[Long]("shop:pairs").calling(Py.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
  }

  test("a Long past 2^53 (a bigint there), bytes and NaN come back as they went") {
    assertEquals(Py.fn[Long]("shop:echo")(Long.MaxValue).runWith, Right(Long.MaxValue))
    assertEquals(Py.fn[Array[Byte]]("shop:echo")(Array[Byte](1, 2, 3)).runWith.map(_.toVector), Right(Vector[Byte](1, 2, 3)))
    assert(Py.fn[Double]("shop:echo")(Double.NaN).runWith.exists(_.isNaN))
  }

  test("a TypeScript exception is a condition by name, and the worker lives on") {
    assertEquals(Py.fn[Long]("shop:fail")().runWith, Left(Condition("RangeError", "typescript says no")))
    assertEquals(Py.fn[Long]("shop:later")(1L).runWith, Right(2L))
  }

  test("tsc --strict accepts the module typed with the generated declarations, and refuses a wrong field") {
    assume(has("tsc", "--version"), "tsc is not installed")
    def tsc(file: String): (Int, String) =
      java.nio.file.Files.writeString(dir.resolve("okay.ts"), TsWorker.library): Unit
      val p = ProcessBuilder("tsc", "--noEmit", "--strict", "--allowImportingTsExtensions",
        "--target", "es2022", "--module", "nodenext", file).directory(dir.toFile).redirectErrorStream(true).start()
      (p.waitFor(), String(p.getInputStream.readAllBytes()))
    val (ok, out) = tsc("shop.ts")
    assertEquals(ok, 0, out)
    java.nio.file.Files.writeString(dir.resolve("bad.ts"), shop.replace("order.sku);", "order.skuu);")): Unit
    val (bad, why) = tsc("bad.ts")
    assertNotEquals(bad, 0)
    assert(why.contains("skuu"), why)
  }
}
