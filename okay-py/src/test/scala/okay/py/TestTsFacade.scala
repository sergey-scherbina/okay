package okay.py

import java.nio.file.{Files, Paths}
import okay.given
import okay.py.TestTsOneShape.{Order, Totals}
import okay.py.golden.{FacadeTs, Receipt}

/** typescript-types T7 against a LIVE tsc and Node: a TypeScript module as a typed Scala facade */
class TestTsFacade extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  private lazy val ready = has("node", "--version") && has("tsc", "--version")
  override def munitIgnore: Boolean = !ready

  /** the module beside the model its types come from — written from Scala */
  private lazy val dir =
    val d = Files.createTempDirectory("okay-ts-facade-test")
    val module = Paths.get(getClass.getResource("/okay/py/golden/facadets.ts").toURI)
    Files.copy(module, d.resolve("facadets.ts")): Unit
    Files.writeString(d.resolve("model.ts"), TestTsOneShape.declarations): Unit
    d
  private lazy val w = TsWorker.start(dir, modules = Seq("facadets"))
  private given okay.Handler[ForeignEval] = w.handler
  override def afterAll(): Unit = if ready then w.close()

  private def golden: String =
    val rel = "src/test/scala/okay/py/golden/FacadeTs.scala"
    val here = Paths.get(rel)
    Files.readString(if Files.exists(here) then here else Paths.get("okay-py", rel))

  test("the checked-in facade is what the generator writes from tsc's declarations today") {
    val src = TsFacade.declarations(dir, "facadets")
      .flatMap(TsFacade.render("FacadeTs", "okay.py.golden", "facadets", _, Seq("okay.py.TestTsOneShape.{Order, Totals}")))
    assertEquals(src, Right(golden))
  }

  test("the generated facade calls TypeScript: typed from the declarations, open where they are open") {
    assertEquals(FacadeTs.total(Order("tea", 4L, Some("ribbon"))).runWith, Right(Totals("tea", 12.0, Some("ribbon"))))
    // an async function, and an answer type tsc inferred: the source wrote neither
    assertEquals(FacadeTs.receipt(Vector("a", "b"), 2.5).runWith, Right(Receipt(Vector("a", "b"), 5.0, None)))
    assertEquals(FacadeTs.greet("okay").runWith, Right("hello okay!"))
    assertEquals(FacadeTs.echo[Vector[Long], Vector[Long]](Vector(1L, 2L)).runWith, Right(Vector(1L, 2L)))
  }

  test("a module tsc refuses is refused, with tsc's reason") {
    val d = Files.createTempDirectory("okay-ts-facade-bad")
    Files.writeString(d.resolve("bad.ts"), "export function f(x: number): string { return x; }\n"): Unit
    val r = TsFacade.declarations(d, "bad")
    assert(r.left.exists(_.contains("not assignable")), r)
  }
}

/** the reading and the rendering, on declarations written by hand (default gate) */
class TestTsFacadeRender extends munit.FunSuite {

  private val dts = """import type { Order } from "./model.ts";
export interface Line { sku: string; qty: Int; }
export type Int = number;
export declare function lines(order: Order, max?: number): Promise<Line[]>;
export declare function each(xs: Line[], f: (l: Line) => void): void;
export {};
"""

  test("functions in order, imported names known, a callback refused alone") {
    val m = okay.codec.TsTypes.parseModule(dts).toOption.get
    assertEquals(m.imported, Vector("Order"))
    assertEquals(m.functions.map(_.fold(_.name, _.name)), Vector("lines", "each"))
    assert(m.functions(1).left.exists(_.why.contains("a function type is not data")), m.functions(1))
  }

  test("a fully typed module imports neither ToPy nor more than it uses") {
    val src = TsFacade.render("M", "p", "m", dts, Seq("q.Order")).toOption.get
    assert(src.contains("  def lines(order: Order): Either[Condition, Vector[Line]] ! ForeignEval =\n    Ts.fn[Vector[Line]](\"m:lines\")(order)\n"), src)
    assert(src.contains("final case class Line(sku: String, qty: Int) derives Schema"), src)
    assert(src.contains("Left out, being optional: max."), src)
    assert(src.contains("import okay.py.{Condition, ForeignEval, Ts}\nimport q.Order\n"), src)
    assert(!src.contains("type Int"), src)
  }

  test("a type nobody declared or imported refuses that function, not the module") {
    val src = TsFacade.render("M", "p", "m", "export declare function f(x: Mystery): string;\nexport declare function g(): string;\n").toOption.get
    assert(src.contains("  // f: not generated — f(x): 'Mystery' is not declared here"), src)
    assert(src.contains("def g(): Either[Condition, String] ! ForeignEval"), src)
  }
}
