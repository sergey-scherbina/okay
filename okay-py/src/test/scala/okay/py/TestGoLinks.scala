package okay.py

import java.nio.file.Path

/** the Go worker, built once, for the conformance suites */
object GoWorkerBinary:
  private def has = scala.util.Try(ProcessBuilder("go", "version").start().waitFor() == 0).getOrElse(false)
  lazy val available: Boolean = has
  lazy val binary: Path =
    val dir = java.nio.file.Files.createTempDirectory("okay-go-links")
    java.nio.file.Files.createDirectories(dir.resolve("shop")): Unit
    java.nio.file.Files.writeString(dir.resolve("shop").resolve("ops.go"),
      Go.ops("shop", Foreign.callbacks(TestGoProgram.priceOf, TestGoProgram.discount))): Unit
    java.nio.file.Files.writeString(dir.resolve("main.go"), TestGoProgram.main): Unit
    GoWorker.build(dir)

  /** the same binary serving TCP on a port it chooses; answers the engine
   * and the process (to stop) */
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
      case None => p.destroy(); throw IllegalStateException(s"the Go worker did not say where it listens: $first")

  def serveTcp(): (ForeignWorker, Process) =
    val (port, p) = listen()
    val engine = ForeignWorker.connect("127.0.0.1", port)
    (engine, p)

/** (Go, pipes) */
class TestGoPipes extends WireConformance:
  override def munitIgnore: Boolean = !GoWorkerBinary.available
  lazy val engine: ForeignWorker = ForeignWorker.speaking(Seq(GoWorkerBinary.binary.toString))
  override def afterAll(): Unit = if GoWorkerBinary.available then engine.close()

/** (Go, TCP): the same binary with OKAY_LISTEN, reached over a socket */
class TestGoTcp extends WireConformance:
  override def munitIgnore: Boolean = !GoWorkerBinary.available
  private lazy val served = GoWorkerBinary.serveTcp()
  lazy val engine: ForeignWorker = served._1
  override def afterAll(): Unit = if GoWorkerBinary.available then { served._1.close(); served._2.destroy() }

/** (Go, pipes), CBOR and DEFLATE chosen by givens (stage 5a) */
class TestGoPipesCbor extends WireConformance:
  import WireFormat.Cbor.given
  import WireCompression.Deflate.given
  override def munitIgnore: Boolean = !GoWorkerBinary.available
  lazy val engine: ForeignWorker = ForeignWorker.speaking(Seq(GoWorkerBinary.binary.toString))
  override def afterAll(): Unit = if GoWorkerBinary.available then engine.close()

/** (Go, TCP), CBOR and DEFLATE */
class TestGoTcpCbor extends WireConformance:
  import WireFormat.Cbor.given
  import WireCompression.Deflate.given
  override def munitIgnore: Boolean = !GoWorkerBinary.available
  private lazy val served =
    val (plain, p) = GoWorkerBinary.serveTcp()
    plain.close()
    // a second connection, configured: the server gives each connection its own worker
    val port = GoWorkerBinary.lastPort
    (ForeignWorker.connect("127.0.0.1", port), p)
  lazy val engine: ForeignWorker = served._1
  override def afterAll(): Unit = if GoWorkerBinary.available then { served._1.close(); served._2.destroy() }

/** (Go, TCP) behind a SECRET (wire-auth): the whole suite after a mutual
 * HMAC-SHA256 challenge, and each way the challenge refuses */
class TestGoTcpAuth extends WireConformance:
  import okay.codec.WireAuth
  given WireAuth = WireAuth.secret("tea for two".getBytes)
  override def munitIgnore: Boolean = !GoWorkerBinary.available
  private lazy val served = GoWorkerBinary.listen(Map("OKAY_WIRE_SECRET" -> "tea for two"))
  lazy val engine: ForeignWorker = ForeignWorker.connect("127.0.0.1", served._1)
  override def afterAll(): Unit = if GoWorkerBinary.available then { engine.close(); served._2.destroy() }

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
    val (port, p) = GoWorkerBinary.listen()
    try
      val e = refusal(WireAuth.secret("tea for two".getBytes), port)
      assert(e.contains("requires the worker at 127.0.0.1:"), e)
      assert(e.contains("it announced none"), e)
    finally p.destroy()
  }


/** (Go, TCP), SUPERVISED (stage 6): the server process is killed in the
 * middle of a multi-shot program and started again on the same port; the
 * supervisor reconnects and replays, and every branch comes back */
class TestGoTcpSupervised extends munit.FunSuite:
  import okay.{Choose, effect, runChoice, given}
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !GoWorkerBinary.available

  test("the server dies and comes back on its port: the program's branches all come back") {
    val (port, first) = GoWorkerBinary.listen()
    var server = first
    val w = ForeignWorker.supervised(ForeignWorker.connect("127.0.0.1", port))
    given okay.Handler[ForeignEval] = w.handler
    var restarted = false
    val choose = Foreign.callback[Vector[Long], Long]("choose") { xs =>
      if !restarted && xs == Vector(10L, 20L) then
        restarted = true
        server.destroyForcibly().waitFor(): Unit
        server = GoWorkerBinary.listen(Map("OKAY_LISTEN" -> s"127.0.0.1:$port"))._2
      effect[Choose, Long](Choose(xs))
    }
    try
      val pairs = Foreign.program[Long]("pairs").calling(Foreign.callbacks(choose))()
      assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
      assert(restarted)
      assertEquals(w.restarts, 1)
    finally
      w.close()
      server.destroy()
  }
