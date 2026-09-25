package okay.cluster

import okay.given


/**
 * okay-arrow stage 7b's number: the same chunks through a real loopback
 * socket in each format — bytes on the wire and the time from the first
 * send to the last chunk received. Not JMH (a socket and a fiber per run);
 * medians of five, printed; Live-tagged; sanity assertions only.
 */
class MeasureRemote extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")
  import RemoteModel.*

  /** a socket that counts what crosses it */
  private final class Counting(s: java.net.Socket) extends java.net.Socket:
    var bytes = 0L
    override def getOutputStream: java.io.OutputStream = new java.io.FilterOutputStream(s.getOutputStream):
      override def write(b: Int): Unit = { bytes += 1; out.write(b) }
      override def write(b: Array[Byte], off: Int, len: Int): Unit = { bytes += len; out.write(b, off, len) }
    override def close(): Unit = s.close()

  /** a run that does not finish within a minute dumps every thread of THIS
   * JVM, virtual ones included, and fails naming the file: a stall is a
   * finding, and the gate's own dump is of sbt, not of this forked JVM */
  private def watched[A](what: String)(body: => A): A =
    val f = java.util.concurrent.CompletableFuture.supplyAsync(() => body)
    try f.get(60, java.util.concurrent.TimeUnit.SECONDS)
    catch case _: java.util.concurrent.TimeoutException =>
      val dump = java.nio.file.Files.createTempFile("remote-stall", ".json")
      java.nio.file.Files.delete(dump)
      java.lang.management.ManagementFactory.getPlatformMXBean(classOf[com.sun.management.HotSpotDiagnosticMXBean])
        .dumpThreads(dump.toString, com.sun.management.HotSpotDiagnosticMXBean.ThreadDumpFormat.JSON)
      fail(s"$what stalled; the threads: $dump")

  private def run(fmt: RemoteFormat, cmp: RemoteCompression, rows: List[Trade], chunk: Int): (Double, Long) =
    watched(s"${fmt.name}+${cmp.codec.map(_.name)} chunk=$chunk")(runOnce(fmt, cmp, rows, chunk))

  private def runOnce(fmt: RemoteFormat, cmp: RemoteCompression, rows: List[Trade], chunk: Int): (Double, Long) =
    val server = RemoteModel.loopback()
    val received = Remote.listen[Trade](server)
    val sock = Counting(java.net.Socket(server.getInetAddress.getHostAddress, server.getLocalPort))
    val sender = Remote.Sender[Trade](sock)(using summon, fmt, cmp)
    val t0 = System.nanoTime()
    val sent = scala.util.Try(rows.grouped(chunk).foreach(g => sender.send(okay.ChunkBuf.of(g))))
    scala.util.Try(sender.close()): Unit
    var n = 0
    val drained = scala.util.Try {
      var c = received.receiveBlocking()
      while c.isDefined do { n += c.get.size; c = received.receiveBlocking() }
    }
    if sent.isFailure || drained.isFailure then
      fail(s"${fmt.name}+${cmp.codec.map(_.name)}: sender ${sent.failed.map(_.toString).getOrElse("ok")}, " +
        s"receiver ${drained.failed.map(e => e.toString + " " + e.getStackTrace.take(6).mkString(" | ")).getOrElse("ok")}")
    val ms = (System.nanoTime() - t0) / 1e6
    server.close()
    assertEquals(n, rows.length)
    (ms, sock.bytes)

  test("the same chunks in each format: bytes on the wire, time end to end") {
    val rows = trades(200000)
    val cases = Vector(
      (RemoteFormat.Json.json, RemoteCompression.none), (RemoteFormat.Cbor.cbor, RemoteCompression.none),
      (RemoteFormat.arrow, RemoteCompression.none), (RemoteFormat.Cbor.cbor, RemoteCompression.Zstd.zstd),
      (RemoteFormat.arrow, RemoteCompression.Zstd.zstd), (RemoteFormat.arrow, RemoteCompression.Lz4.lz4))
    for chunk <- Vector(1000, 10000) do
      for (fmt, cmp) <- cases do
        run(fmt, cmp, rows.take(20000), chunk): Unit          // warm
        val runs = Vector.fill(5)(run(fmt, cmp, rows, chunk))
        val ms = runs.map(_._1).sorted.apply(2)
        println(f"REMOTE chunk=$chunk%6d ${fmt.name + cmp.codec.fold("")("+" + _.name)}%-10s ${runs.head._2}%10d bytes ${ms}%8.1f ms")
  }
