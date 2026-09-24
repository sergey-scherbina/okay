package okay.py

import java.nio.file.{Files, Path}
import okay.Reader

object RustWorkerBinary:
  val priceOf = Foreign.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
  val discount = Foreign.callback[Double, Double]("discount")(a => Reader.ask[Map[String, Double]].map(m => a * m("rate")))

  // no margin: the docs quote these lines
  val main: String = """mod ops;

use okay::{done, perform, send, function, okay_call, Functions, Program, Programs, Value, Wire, Worker};

/// two choices; okay's Choice handler continues each continuation twice
fn pairs(_: Vec<Value>) -> okay::Prog {
    perform("choose", vec![vec![1i64, 2].to_value()]).and_then(|x| {
        perform("choose", vec![vec![10i64, 20].to_value()]).and_then(move |y| {
            done(i64::from_value(&x).unwrap() + i64::from_value(&y).unwrap())
        })
    })
}

/// the same operations, typed: generated from the Scala callbacks by Rs.ops
fn total(sku: String, qty: i64) -> Program<f64> {
    send(ops::price_of(sku)).and_then(move |price| send(ops::discount(price * qty as f64)))
}

fn make() -> Worker {
    let mut programs = Programs::new();
    programs.insert("pairs".into(), Box::new(pairs));
    programs.insert("total".into(), Box::new(|args: Vec<Value>| {
        total(String::from_value(&args[0]).unwrap(), i64::from_value(&args[1]).unwrap()).into_prog()
    }));
    programs.insert("boom".into(), Box::new(|_: Vec<Value>| -> okay::Prog { panic!("rust says no") }));
    let mut functions = Functions::new();
    // DIRECT STYLE: ordinary Rust calling okay's effects, okay_call(request) -> answer
    functions.insert("quote".into(), function(|args| {
        let sku = String::from_value(&args[0])?;
        let qty = i64::from_value(&args[1])?;
        let price = okay_call(ops::price_of(sku))?;
        let total = okay_call(ops::discount(price * qty as f64))?;
        Ok(total.to_value())
    }));
    Worker::new(programs, functions)
}

fn main() {
    okay::main(make)
}
"""

  private def has = scala.util.Try(ProcessBuilder("cargo", "--version").start().waitFor() == 0).getOrElse(false)
  lazy val available: Boolean = has
  lazy val binary: Path =
    val dir = Files.createTempDirectory("okay-rust-worker")
    Files.createDirectories(dir.resolve("src")): Unit
    Files.writeString(dir.resolve("src").resolve("ops.rs"), Rs.ops(Foreign.callbacks(priceOf, discount))): Unit
    Files.writeString(dir.resolve("src").resolve("main.rs"), main): Unit
    RustWorker.build(dir)

  @volatile var lastPort: Int = 0

  def serveTcp(): (ForeignWorker, Process) =
    val pb = ProcessBuilder(binary.toString)
    pb.environment().put("OKAY_LISTEN", "127.0.0.1:0")
    val p = pb.start()
    val first = java.io.BufferedReader(java.io.InputStreamReader(p.getInputStream, "UTF-8")).readLine()
    val port = okay.codec.Json.parse(first) match
      case okay.codec.Json.JObj(fs) => fs.toMap.get("listening").collect { case okay.codec.Json.JStr(a) => a.split(":").last.toInt }
      case _ => None
    port match
      case Some(n) => lastPort = n; (ForeignWorker.connect("127.0.0.1", n), p)
      case None => p.destroy(); throw IllegalStateException(s"the Rust worker did not say where it listens: $first")

/** (Rust, pipes) */
class TestRustPipes extends WireConformance:
  override def munitIgnore: Boolean = !RustWorkerBinary.available
  lazy val engine: ForeignWorker = ForeignWorker.speaking(Seq(RustWorkerBinary.binary.toString))
  override def afterAll(): Unit = if RustWorkerBinary.available then engine.close()

/** (Rust, TCP): the same binary with OKAY_LISTEN, reached over a socket */
class TestRustTcp extends WireConformance:
  override def munitIgnore: Boolean = !RustWorkerBinary.available
  private lazy val served = RustWorkerBinary.serveTcp()
  lazy val engine: ForeignWorker = served._1
  override def afterAll(): Unit = if RustWorkerBinary.available then { served._1.close(); served._2.destroy() }

/** (Rust, pipes), CBOR and DEFLATE chosen by givens (stage 5a) */
class TestRustPipesCbor extends WireConformance:
  import WireFormat.Cbor.given
  import WireCompression.Deflate.given
  override def munitIgnore: Boolean = !RustWorkerBinary.available
  lazy val engine: ForeignWorker = ForeignWorker.speaking(Seq(RustWorkerBinary.binary.toString))
  override def afterAll(): Unit = if RustWorkerBinary.available then engine.close()

/** (Rust, TCP), CBOR and DEFLATE */
class TestRustTcpCbor extends WireConformance:
  import WireFormat.Cbor.given
  import WireCompression.Deflate.given
  override def munitIgnore: Boolean = !RustWorkerBinary.available
  private lazy val served =
    val (plain, p) = RustWorkerBinary.serveTcp()
    plain.close()
    (ForeignWorker.connect("127.0.0.1", RustWorkerBinary.lastPort), p)
  lazy val engine: ForeignWorker = served._1
  override def afterAll(): Unit = if RustWorkerBinary.available then { served._1.close(); served._2.destroy() }

/** the generated operations (default gate) */
class TestRsOps extends munit.FunSuite:
  test("typed operation constructors, written from the Scala callbacks") {
    val src = Rs.ops(Foreign.callbacks(RustWorkerBinary.priceOf, RustWorkerBinary.discount))
    assert(src.contains("pub fn price_of(a0: String) -> Op<f64> {"), src)
    assert(src.contains("pub fn discount(a0: f64) -> Op<f64> {"), src)
    assertEquals(Rs.function("type"), "r#type")
  }
