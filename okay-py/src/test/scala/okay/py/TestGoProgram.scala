package okay.py

import java.nio.file.{Files, Path}
import okay.{Choose, Reader, effect, runChoice, given}

object TestGoProgram:
  val priceOf = Foreign.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
  val discount = Foreign.callback[Double, Double]("discount")(amount => Reader.ask[Map[String, Double]].map(m => amount * m("rate")))

  // no margin: the docs quote these lines
  val main: String = """package main

import (
	"worker/okay"
	"worker/shop"
)

// two choices; okay's Choice handler continues each continuation twice
func pairs(_ []any) okay.Prog {
	return okay.Perform("choose", []any{1, 2}).Then(func(x any) okay.Prog {
		return okay.Perform("choose", []any{10, 20}).Then(func(y any) okay.Prog {
			return okay.Done(x.(int64) + y.(int64))
		})
	})
}

// the same operations, typed: generated from the Scala callbacks by Go.ops
func total(sku string, qty int64) okay.Program[float64] {
	return okay.Bind(okay.Send(shop.PriceOf(sku)), func(price float64) okay.Program[float64] {
		return okay.Send(shop.Discount(price * float64(qty)))
	})
}

func boom(_ []any) okay.Prog {
	panic("go says no")
}

// DIRECT STYLE: ordinary Go calling okay's effects, okay_call(request) -> answer
func quote(c *okay.Ctx, args []any) any {
	price, err := okay.Call(c, shop.PriceOf(args[0].(string)))
	if err != nil {
		panic(err)
	}
	total, err := okay.Call(c, shop.Discount(price*float64(args[1].(int64))))
	if err != nil {
		panic(err)
	}
	return total
}

var programs = okay.Programs{
	"pairs": pairs,
	"total": func(args []any) okay.Prog { return total(args[0].(string), args[1].(int64)).Untyped() },
	"boom":  boom,
}

// a TABLE call: the frame arrives as an okay.Frame of columns
func scale(_ *okay.Ctx, args []any) any {
	k := args[1].(int64)
	var x []any
	for _, v := range args[0].(okay.Frame).Col("x") {
		x = append(x, v.(int64)*k)
	}
	return okay.Frame{{Name: "x", Values: x}}
}

// a HELD value (the number itself), and a function reading it
func counter(_ *okay.Ctx, args []any) any  { return args[0] }
func describe(_ *okay.Ctx, args []any) any { return args[0].(int64) + args[1].(int64) }

var functions = okay.Functions{"quote": quote, "scale": scale, "counter": counter, "describe": describe}

// in-process (a WebAssembly build): what okay_exchange serves
func init() {
	okay.Export(programs, functions)
}

// as a process: stdin/stdout, or TCP with OKAY_LISTEN
func main() {
	okay.Main(programs, functions)
}
"""

/** polyglot-go against a LIVE Go toolchain: the same wire, a Go far side */
class TestGoProgram extends munit.FunSuite {
  import TestGoProgram.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private lazy val go = scala.util.Try(ProcessBuilder("go", "version").start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !go

  private def project(source: String): Path =
    val dir = Files.createTempDirectory("okay-go")
    Files.createDirectories(dir.resolve("shop")): Unit
    Files.writeString(dir.resolve("shop").resolve("ops.go"), Go.ops("shop", Foreign.callbacks(priceOf, discount))): Unit
    Files.writeString(dir.resolve("main.go"), source): Unit
    dir

  private lazy val w = ForeignWorker.speaking(Seq(GoWorker.build(project(main)).toString))
  private given okay.Handler[ForeignEval] = w.handler
  override def afterAll(): Unit = if go then w.close()

  private val choose = Foreign.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))

  test("MULTI-SHOT across a process, from Go: every branch of two choices") {
    assertEquals(w.pythonVersion, "go")
    val pairs = Foreign.program[Long]("pairs").calling(Foreign.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
    pairs.forget.runWith
  }

  test("typed operations, generated from the Scala callbacks, each a callback under the caller's Reader") {
    val ops = Go.ops("shop", Foreign.callbacks(priceOf, discount))
    assert(ops.contains("func PriceOf(a0 string) okay.Op[float64] {"), ops)
    val run = Foreign.program[Double]("total").calling(Foreign.callbacks(priceOf, discount))("tea", 3L)
    assertEquals(Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(run.program).runWith, Right(6.0))
  }

  test("a Go panic is a condition by name, and the worker lives on") {
    val boom = Foreign.program[Long]("boom").calling(Foreign.callbacks(choose))()
    val got = runChoice(boom.program).runWith
    assert(got.headOption.exists(_.left.exists(c => c.kind == "GoError" && c.message.contains("go says no"))), s"$got")
    assertEquals(runChoice(Foreign.program[Long]("pairs").calling(Foreign.callbacks(choose))().program).runWith.size, 4)
  }

  test("go build refuses an operation called with the wrong argument type") {
    val wrong = main.replace("shop.PriceOf(sku)", "shop.PriceOf(qty)")
    val e = intercept[IllegalStateException](GoWorker.build(project(wrong)))
    assert(e.getMessage.contains("cannot use qty"), e.getMessage)
  }
}
