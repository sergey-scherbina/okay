package okay.ts

import scala.scalajs.js
import okay.{!, %, Async, Choose, Reader, effect, runChoice, given}
import okay.codec.Schema

object TestTs:
  final case class Order(sku: String, qty: Long) derives Schema
  final case class Totals(sku: String, amount: Double, note: Option[String]) derives Schema
  enum Shape derives Schema:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)

  /** TypeScript's okay library and some programs, as the JavaScript that
   * TypeScript compiles to (Node strips the types; so does this) */
  val programs: js.Dynamic = js.Dynamic.global.eval("""(function () {
    const done = (value) => ({ tag: "done", value });
    const perform = (name, ...args) => ({ tag: "perform", name, args, k: (x) => done(x) });
    const then = (p, f) => p.tag === "done" ? f(p.value)
      : { tag: "perform", name: p.name, args: p.args, k: (x) => then(p.k(x), f) };
    return {
      pairs: () => then(perform("choose", [1, 2]), (x) =>
        then(perform("choose", [10, 20]), (y) => done(x + y))),
      total: (order) => then(perform("price_of", order.sku), (price) =>
        done({ sku: order.sku, amount: price * order.qty, note: null })),
      area: (s) => done(s.Rect ? s.Rect.w * s.Rect.h : 3 * s.Circle.r * s.Circle.r),
      boom: () => then(perform("choose", [1]), (x) => { throw new RangeError("typescript says no"); }),
      notAProgram: () => 42,
    };
  })()""")

/** stage 2 and 3 of specs/typescript.md, on Scala.js (Node runs the tests) */
class TestTs extends munit.FunSuite {
  import TestTs.*

  private val choose = Ts.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))

  test("a TypeScript program walked inside okay, MULTI-SHOT: Choice calls a JS continuation twice") {
    val answers = !.run(runChoice(Ts.run[Choose, Long](programs.pairs(), Ts.callbacks(choose))))
    assertEquals(answers.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
  }

  test("a named operation is an okay callback, under the caller's Reader; values in JSON's shape") {
    val priceOf = Ts.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
    val program = programs.total(Ts.toJs(Order("tea", 3L)))
    val answer = !.run(Reader.run(Map("tea" -> 4.0))(Ts.run[Reader % Map[String, Double], Totals](program, Ts.callbacks(priceOf))))
    assertEquals(answer, Right(Totals("tea", 12.0, None)))
  }

  test("a sum crosses as okay's JSON writes it: { \"Rect\": {...} }") {
    val program = programs.area(Ts.toJs(Shape.Rect(2, 3): Shape))
    assertEquals(!.run(runChoice(Ts.run[Choose, Double](program, Ts.callbacks(choose)))), Seq(Right(6.0)))
  }

  test("a JavaScript exception, and a function that is not a program, are Lefts by name") {
    assertEquals(!.run(runChoice(Ts.run[Choose, Long](programs.boom(), Ts.callbacks(choose)))),
      Seq(Left(Ts.Failure("RangeError", "typescript says no"))))
    assertEquals(!.run(runChoice(Ts.run[Choose, Long](programs.notAProgram(), Ts.callbacks(choose)))).map(_.left.map(_.kind)),
      Seq(Left("TypeError")))
  }

  test("okay called FROM TypeScript: an Async program as a JS Promise of its JSON value") {
    val program = okay.pure[Async, Totals](Totals("tea", 12.0, None))
    given scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue
    Ts.promise(program).toFuture.map(v => assertEquals(js.JSON.stringify(v), """{"sku":"tea","amount":12,"note":null}"""))
  }
}
