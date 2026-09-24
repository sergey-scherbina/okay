package okay.py

/** the gateway in front of a stdio worker: its port and its process */
object Gateways:
  def python(env: Map[String, String]): (Int, Process) =
    ForeignGateway.start(ForeignWorker.pythonCommand(TestPy.python.get, Seq(PyConformance.conf)), env = env,
      python = TestPy.python.get)

  def typescript(env: Map[String, String]): (Int, Process) =
    val d = java.nio.file.Files.createTempDirectory("okay-ts-gw")
    java.nio.file.Files.writeString(d.resolve("conf.ts"), TsConformance.conf): Unit
    ForeignGateway.start(TsWorker.command(d, Seq("conf")), env = env, python = TestPy.python.get)

  def haskell(env: Map[String, String]): (Int, Process) =
    ForeignGateway.start(WorkerCommand(Vector(HsConformance.binary), Map.empty), env = env, python = TestPy.python.get)

/** (Python, TCP through the gateway): the conformance suite over TLS with its
 * refusals, and TLS with a secret — the same suite Go and Rust pass */
class TestGatewayPyTls extends TlsConformance:
  def listen(env: Map[String, String]): (Int, Process) = Gateways.python(env)
  def serverAvailable: Boolean = TestPy.python.nonEmpty
  override def address(name: String): String = s"conf:$name"

/** (TypeScript, TCP through the gateway), behind a secret */
class TestGatewayTsAuth extends WireConformance:
  import okay.codec.WireAuth
  given WireAuth = WireAuth.secret("tea for two".getBytes)
  override def munitIgnore: Boolean = !TsConformance.node || TestPy.python.isEmpty
  override def address(name: String): String = s"conf:$name"
  private lazy val served = Gateways.typescript(Map("OKAY_WIRE_SECRET" -> "tea for two"))
  lazy val engine: ForeignWorker = ForeignWorker.connect("127.0.0.1", served._1)
  override def afterAll(): Unit = if !munitIgnore then { engine.close(); served._2.destroy() }

  test("a wrong secret is refused by the gateway, which closes the connection") {
    val e = intercept[IllegalStateException](ForeignWorker.connect("127.0.0.1", served._1)(using
      summon[WireFormat], summon[WireCompression], WireAuth.secret("tea for one".getBytes))).getMessage
    assert(e.contains("authentication refused"), e)
  }

  test("each connection has a worker of its own: a second connection runs beside the first") {
    val second = ForeignWorker.connect("127.0.0.1", served._1)
    try assertEquals(second.wire, engine.wire) finally second.close()
  }

/** (Haskell, TCP through the gateway, CBOR): programs only */
class TestGatewayHs extends WireConformance:
  import WireFormat.Cbor.given
  override def munitIgnore: Boolean = !HsConformance.ghc || TestPy.python.isEmpty
  override def direct: Boolean = false
  private lazy val served = Gateways.haskell(Map.empty)
  lazy val engine: ForeignWorker = ForeignWorker.connect("127.0.0.1", served._1)
  override def afterAll(): Unit = if !munitIgnore then { engine.close(); served._2.destroy() }

  test("the stage-5a configure passes through the gateway untouched: CBOR reaches Haskell") {
    assertEquals(engine.wire, "cbor/none")
  }
