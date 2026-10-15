package okay.cluster.foreign

import java.nio.file.{Files, Path}
import okay.foreign.{GoWorker, HaskellWorker, RustWorker}

/**
 * The facade's conformance body over TypeScript, Go, Rust and Haskell
 * (foreign-more-languages): the same text Python and R answer, each
 * language one `Language` value and the givens by its module type. The
 * far side in each is five names — `echo`, `boom` (a record's), `fecho`,
 * `fboom` (a frame's), `priced` and `pairs` (programs), `make` and
 * `describe` (a held value) — and in TypeScript the held thing is a
 * `Counter` with a method. Live: each needs its toolchain.
 */
object MoreLanguages:
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  lazy val node: Boolean = has("node", "--version")
  lazy val go: Boolean = has("go", "version")
  lazy val cargo: Boolean = has("cargo", "--version")
  lazy val ghc: Boolean = has("ghc", "--version")

  val ts = TsModule("tsfacade", """
import { done, perform, then, type Prog } from "./okay.ts";

export function echo(rec: unknown): unknown { return rec; }
export function boom(_: unknown): unknown { throw new Error("nope"); }
export function fecho(frame: unknown): unknown { return frame; }
export function fboom(_: unknown): unknown { throw new Error("nope"); }

// a field, not a parameter property: Node runs TypeScript by stripping its types
export class Counter {
  n: number;
  constructor(n: number) { this.n = n; }
  add(k: number): number { return this.n + k; }
}
export function make(n: number): Counter { return new Counter(n); }
export function describe(c: Counter, k: number): number { return c.n + k; }

export function priced(order: { sku: string; qty: number }): Prog<number> {
  return then(perform<number>("price_of", order.sku), (p) => done(p * order.qty));
}
export function pairs(_: unknown): Prog<number> {
  return then(perform<number>("choose", [1, 2]), (x) =>
    then(perform<number>("choose", [10, 20]), (y) => done(x + y)));
}
""")

  private val goMain = """package main

import "worker/okay"

func field(d okay.Dict, k string) any {
	for _, kv := range d {
		if kv.Key == k {
			return kv.Val
		}
	}
	return nil
}

func echo(_ *okay.Ctx, args []any) any  { return args[0] }
func boom(_ *okay.Ctx, _ []any) any     { panic("nope") }
func fecho(_ *okay.Ctx, args []any) any { return args[0] }
func fboom(_ *okay.Ctx, _ []any) any    { panic("nope") }

func priced(args []any) okay.Prog {
	order := args[0].(okay.Dict)
	qty := field(order, "qty").(int64)
	return okay.Perform("price_of", field(order, "sku")).Then(func(p any) okay.Prog {
		return okay.Done(p.(float64) * float64(qty))
	})
}

func pairs(_ []any) okay.Prog {
	return okay.Perform("choose", []any{1, 2}).Then(func(x any) okay.Prog {
		return okay.Perform("choose", []any{10, 20}).Then(func(y any) okay.Prog {
			return okay.Done(x.(int64) + y.(int64))
		})
	})
}

func keep(_ *okay.Ctx, args []any) any     { return args[0] }
func describe(_ *okay.Ctx, args []any) any { return args[0].(int64) + args[1].(int64) }

func main() {
	okay.Main(okay.Programs{"priced": priced, "pairs": pairs},
		okay.Functions{"echo": echo, "boom": boom, "fecho": fecho, "fboom": fboom, "make": keep, "describe": describe})
}
"""

  private val rustMain = """use okay::{done, function, perform, Functions, Programs, Value, Wire, Worker};

fn field(v: &Value, k: &str) -> Value {
    match v {
        Value::Dict(kv) => kv.iter().find(|(n, _)| n == k).map(|(_, x)| x.clone()).unwrap_or(Value::Null),
        _ => Value::Null,
    }
}

fn make() -> Worker {
    let mut programs = Programs::new();
    programs.insert("priced".into(), Box::new(|args: Vec<Value>| {
        let qty = i64::from_value(&field(&args[0], "qty")).unwrap_or(0);
        perform("price_of", vec![field(&args[0], "sku")])
            .and_then(move |p| done(f64::from_value(&p).unwrap_or(0.0) * qty as f64))
    }));
    programs.insert("pairs".into(), Box::new(|_: Vec<Value>| {
        perform("choose", vec![vec![1i64, 2].to_value()]).and_then(|x| {
            perform("choose", vec![vec![10i64, 20].to_value()]).and_then(move |y| {
                done(i64::from_value(&x).unwrap() + i64::from_value(&y).unwrap())
            })
        })
    }));
    let mut functions = Functions::new();
    functions.insert("echo".into(), function(|args| Ok(args[0].clone())));
    functions.insert("boom".into(), function(|_| Err("nope".to_string())));
    functions.insert("fecho".into(), function(|args| Ok(args[0].clone())));
    functions.insert("fboom".into(), function(|_| Err("nope".to_string())));
    functions.insert("make".into(), function(|args| Ok(args[0].clone())));
    functions.insert("describe".into(), function(|args| Ok(Value::Int(i64::from_value(&args[0])? + i64::from_value(&args[1])?))));
    Worker::new(programs, functions)
}

fn main() {
    okay::main(make)
}
"""

  private val hsMain = """module Main (main) where

import Okay

num :: Value -> Double
num (VDouble d) = d
num (VInt n) = fromInteger n
num v = error ("not a number: " ++ show v)

field :: String -> Value -> Value
field k (VDict kv) = maybe VNull id (lookup k kv)
field _ _ = VNull

echo, boom, fecho, fboom, priced, pairs :: [Value] -> Prog Value
echo [r] = done r
echo _ = error "echo takes one value"
boom _ = error "nope"
fecho [f] = done f
fecho _ = error "fecho takes one frame"
fboom _ = error "nope"
priced [order] = do
  p <- perform "price_of" [field "sku" order]
  return (VDouble (num p * num (field "qty" order)))
priced _ = error "priced takes an order"
pairs _ = do
  x <- perform "choose" [VList [VInt 1, VInt 2]]
  y <- perform "choose" [VList [VInt 10, VInt 20]]
  return (VInt (round (num x + num y)))

make, describe :: [Value] -> Prog Value
make [n] = done n
make _ = error "make takes a number"
describe [c, k] = done (VInt (round (num c + num k)))
describe _ = error "describe takes a held value and a number"

main :: IO ()
main = serve [("echo", echo), ("boom", boom), ("fecho", fecho), ("fboom", fboom), ("priced", priced), ("pairs", pairs),
              ("make", make), ("describe", describe)]
"""

  private def dir(prefix: String): Path = Files.createTempDirectory(prefix)

  lazy val goModule: WorkerModule =
    val d = dir("okay-go-facade")
    Files.writeString(d.resolve("main.go"), goMain): Unit
    WorkerModule("go", "gofacade", Seq(GoWorker.build(d).toString))

  lazy val rustModule: WorkerModule =
    val d = dir("okay-rust-facade")
    Files.createDirectories(d.resolve("src")): Unit
    Files.writeString(d.resolve("src").resolve("main.rs"), rustMain): Unit
    WorkerModule("rust", "rustfacade", Seq(RustWorker.build(d).toString))

  lazy val hsModule: WorkerModule =
    val d = dir("okay-hs-facade")
    Files.writeString(d.resolve("Main.hs"), hsMain): Unit
    WorkerModule("haskell", "hsfacade", Seq(HaskellWorker.build(d).toString))

/** the body every language answers; Holds and Methods where it has them */
abstract class MoreLanguagesFacade[M](using Calls[M], Frames[M], Programs[M], Speaks[M], Engine[M]) extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")
  def module: M
  def word: String

  test("Calls: a record back at its type, a raise refused by kind, a missing function refused") {
    FacadeConformance.calls(module, "echo", "boom")
  }
  test("Frames: rows as one table and back, the empty table too, a raise refused by kind") {
    FacadeConformance.frames(module, "fecho", "fboom")
  }
  test("Streams: 20 000 rows through fecho in frames of 4 096, every row back in order") {
    FacadeConformance.streams(module, "fecho", 20000, 4096)
  }
  test("Programs: a callback under a Reader, and a continuation resumed twice") {
    FacadeConformance.programs(module, "priced", "pairs")
  }
  test("a cluster stage: a partitioned Flow mapped through fecho, every row back") {
    import okay.given
    val recs = Vector.tabulate(1000)(i => FacadeConformance.Rec(i, i * 0.5, s"r$i"))
    val out = okay.cluster.Flows.collect(okay.cluster.Flow.slices(recs, 3).mapIn[FacadeConformance.Rec](module, "fecho", batch = 128)).runWith
    assertEquals(out.toVector.sortBy(_.key), recs)
  }
  test("Speaks: the language names itself, and its frames cross as columnar JSON") {
    val r = summon[Speaks[M]].speaks(module)
    assertEquals((r.language, r.frames), (word, "columnar-json"))
  }

class TestTsFacadeConformance extends MoreLanguagesFacade[TsModule]:
  override def munitIgnore: Boolean = !MoreLanguages.node
  def module = MoreLanguages.ts
  def word = "typescript"
  test("Holds: two Counters held, each described with its own state, released") {
    FacadeConformance.holds(module, "make", "describe")
  }
  test("Methods: a held Counter's add and n") {
    FacadeConformance.methods(module, "make", "add", "n")
  }

class TestGoFacadeConformance extends MoreLanguagesFacade[WorkerModule]:
  override def munitIgnore: Boolean = !MoreLanguages.go
  def module = MoreLanguages.goModule
  def word = "go"
  test("Holds: two values held, each described by its ref, released") {
    FacadeConformance.holds(module, "make", "describe")
  }

class TestRustFacadeConformance extends MoreLanguagesFacade[WorkerModule]:
  override def munitIgnore: Boolean = !MoreLanguages.cargo
  def module = MoreLanguages.rustModule
  def word = "rust"
  test("Holds: two values held, each described by its ref, released") {
    FacadeConformance.holds(module, "make", "describe")
  }

class TestHsFacadeConformance extends MoreLanguagesFacade[WorkerModule]:
  override def munitIgnore: Boolean = !MoreLanguages.ghc
  def module = MoreLanguages.hsModule
  def word = "haskell"
  test("Holds: two values held, each described by its ref, released") {
    FacadeConformance.holds(module, "make", "describe")
  }
