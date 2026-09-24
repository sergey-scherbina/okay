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
  def serveTcp(): (ForeignWorker, Process) =
    val pb = ProcessBuilder(binary.toString)
    pb.environment().put("OKAY_LISTEN", "127.0.0.1:0")
    val p = pb.start()
    val first = java.io.BufferedReader(java.io.InputStreamReader(p.getInputStream, "UTF-8")).readLine()
    val port = okay.codec.Json.parse(first) match
      case okay.codec.Json.JObj(fs) => fs.toMap.get("listening").collect { case okay.codec.Json.JStr(a) => a.split(":").last.toInt }
      case _ => None
    port match
      case Some(n) => (ForeignWorker.connect("127.0.0.1", n), p)
      case None => p.destroy(); throw IllegalStateException(s"the Go worker did not say where it listens: $first")

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
