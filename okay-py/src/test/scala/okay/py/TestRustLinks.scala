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
    // a TABLE call: the frame arrives as a Value::Table of columns
    functions.insert("scale".into(), function(|args| {
        let k = i64::from_value(&args[1])?;
        let x = args[0].col("x").ok_or("no column x")?;
        let scaled = x.iter().map(|v| i64::from_value(v).map(|n| Value::Int(n * k))).collect::<Result<Vec<_>, _>>()?;
        Ok(Value::Table(vec![("x".into(), scaled)]))
    }));
    // a HELD value (the number itself), and a function reading it
    functions.insert("counter".into(), function(|args| Ok(args[0].clone())));
    functions.insert("describe".into(), function(|args| Ok(Value::Int(i64::from_value(&args[0])? + i64::from_value(&args[1])?))));
    // a table answered as it came: every column kind, both ways
    functions.insert("echo".into(), function(|args| Ok(args[0].clone())));
    // a column mixing kinds, which C Data cannot carry: answered on the wire instead
    functions.insert("mixed".into(), function(|_| Ok(Value::Table(vec![("m".into(), vec![Value::Int(1), Value::Str("two".into())])]))));
    Worker::new(programs, functions)
}

fn main() {
    okay::main(make)
}
"""

  private def has = scala.util.Try(ProcessBuilder("cargo", "--version").start().waitFor() == 0).getOrElse(false)
  lazy val available: Boolean = has
  private def built(features: Seq[String]): Path =
    val dir = Files.createTempDirectory("okay-rust-worker")
    Files.createDirectories(dir.resolve("src")): Unit
    Files.writeString(dir.resolve("src").resolve("ops.rs"), Rs.ops(Foreign.callbacks(priceOf, discount))): Unit
    Files.writeString(dir.resolve("src").resolve("main.rs"), main): Unit
    RustWorker.build(dir, features = features)
  lazy val binary: Path = built(Seq("tls"))
  /** the same worker without the tls feature: what a default build is */
  lazy val plainBinary: Path = built(Nil)

  @volatile var lastPort: Int = 0

  /** the binary serving TCP on a port it chooses, with `env` added (a
   * secret): answers the port and the process (to stop) */
  def listen(env: Map[String, String] = Map.empty): (Int, Process) =
    val pb = ProcessBuilder(binary.toString)
    pb.environment().put("OKAY_LISTEN", "127.0.0.1:0")
    env.foreach((k, v) => pb.environment().put(k, v))
    val p = pb.start()
    val first = java.io.BufferedReader(java.io.InputStreamReader(p.getInputStream, "UTF-8")).readLine()
    val port = okay.codec.Json.parse(first) match
      case okay.codec.Json.JObj(fs) => fs.toMap.get("listening").collect { case okay.codec.Json.JStr(a) => a.split(":").last.toInt }
      case _ => None
    port match
      case Some(port) =>
        lastPort = port
        (port, p)
      case None => p.destroy(); throw IllegalStateException(s"the Rust worker did not say where it listens: $first")

  def serveTcp(): (ForeignWorker, Process) =
    val (port, p) = listen()
    val engine = ForeignWorker.connect("127.0.0.1", port)
    (engine, p)

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

/** (Rust, TCP) behind a SECRET (wire-auth): the whole suite after a mutual
 * HMAC-SHA256 challenge, and each way the challenge refuses */
class TestRustTcpAuth extends WireConformance:
  import okay.codec.WireAuth
  given WireAuth = WireAuth.secret("tea for two".getBytes)
  override def munitIgnore: Boolean = !RustWorkerBinary.available
  private lazy val served = RustWorkerBinary.listen(Map("OKAY_WIRE_SECRET" -> "tea for two"))
  lazy val engine: ForeignWorker = ForeignWorker.connect("127.0.0.1", served._1)
  override def afterAll(): Unit = if RustWorkerBinary.available then { engine.close(); served._2.destroy() }

  private def refusal(auth: WireAuth, port: Int = served._1): String =
    intercept[IllegalStateException](ForeignWorker.connect("127.0.0.1", port)(using summon[WireFormat], summon[WireCompression], auth)).getMessage

  test("a wrong secret is refused by the server, which closes the connection") {
    val e = refusal(WireAuth.secret("tea for one".getBytes))
    assert(e.contains("refused this host's authentication"), e)
    assert(e.contains("authentication refused"), e)
  }

  test("a host with no given WireAuth is refused by name before it sends anything") {
    val e = refusal(WireAuth.Off)
    assert(e.contains("requires hmac-sha256 authentication; this host has no given WireAuth"), e)
  }

  test("a request before the auth is answered with a refusal, not served") {
    val s = java.net.Socket("127.0.0.1", served._1)
    try
      val in = java.io.BufferedReader(java.io.InputStreamReader(s.getInputStream, "UTF-8"))
      val out = s.getOutputStream
      assert(in.readLine().contains("hmac-sha256"))
      out.write("{\"id\":1,\"op\":\"program\",\"run\":1,\"fn\":\"pairs\",\"args\":[]}\n".getBytes("UTF-8"))
      out.flush()
      val answer = in.readLine()
      assert(answer.contains("PermissionError") && !answer.contains("perform"), answer)
    finally s.close()
  }

  test("a host whose given demands a secret refuses a server that announced none") {
    val (port, p) = RustWorkerBinary.listen()
    try
      val e = refusal(WireAuth.secret("tea for two".getBytes), port)
      assert(e.contains("requires the worker at 127.0.0.1:"), e)
      assert(e.contains("it announced none"), e)
    finally p.destroy()
  }


/** (Rust, TLS): the conformance suite over rustls (the crate's tls feature),
 * and TLS's refusals */
class TestRustTcpTls extends TlsConformance:
  def listen(env: Map[String, String]): (Int, Process) = RustWorkerBinary.listen(env)
  def serverAvailable: Boolean = RustWorkerBinary.available

/** a Rust worker built WITHOUT the tls feature, asked for TLS, refuses to
 * start and says how to build it */
class TestRustTlsFeature extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !RustWorkerBinary.available || !TestTlsCerts.available

  test("no tls feature, TLS asked for: the worker refuses at start, naming the build switch") {
    val pb = ProcessBuilder(RustWorkerBinary.plainBinary.toString).redirectErrorStream(true)
    pb.environment().put("OKAY_LISTEN", "127.0.0.1:0")
    pb.environment().put("OKAY_TLS_CERT", TestTlsCerts.server.get.cert.toString)
    pb.environment().put("OKAY_TLS_KEY", TestTlsCerts.server.get.key.toString)
    val p = pb.start()
    val said = String(p.getInputStream.readAllBytes(), "UTF-8")
    assertEquals(p.waitFor(), 1)
    assert(said.contains("built without the okay crate's tls feature"), said)
    assert(said.contains("""RustWorker.build(dir, features = Seq("tls"))"""), said)
  }

