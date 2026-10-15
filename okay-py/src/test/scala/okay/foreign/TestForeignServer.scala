package okay.foreign

import java.net.Socket
import java.nio.charset.StandardCharsets.UTF_8

/** a far side that DIALS: the TypeScript worker, its pipes copied to a socket
 * connected to a `ForeignServer` (the gateway's two streams, the other way
 * round — the worker speaks stdio only) */
final class Dialling(command: WorkerCommand, port: Int):
  private val proc =
    val pb = ProcessBuilder(command.command*)
    pb.environment().clear()
    command.env.foreach((k, v) => pb.environment().put(k, v))
    pb.redirectError(ProcessBuilder.Redirect.INHERIT)
    pb.start()
  private val socket = Socket("127.0.0.1", port)
  private def pump(from: java.io.InputStream, to: java.io.OutputStream, name: String): Unit =
    val t = Thread(() =>
      try
        val buf = new Array[Byte](8192)
        var n = from.read(buf)
        while n >= 0 do
          to.write(buf, 0, n)
          to.flush()
          n = from.read(buf)
      catch case _: java.io.IOException => ()
      finally close()
    , name)
    t.setDaemon(true)
    t.start()
  pump(proc.getInputStream, socket.getOutputStream, "okay-dial-out")
  pump(socket.getInputStream, proc.getOutputStream, "okay-dial-in")
  def close(): Unit =
    try socket.close() catch case _: java.io.IOException => ()
    proc.destroy()

/** (TypeScript, ACCEPTED): the conformance suite over a connection the server
 * accepted, the far side the TypeScript worker dialling it (foreign-server) */
class TestTsServed extends WireConformance:
  override def munitIgnore: Boolean = !TsConformance.node
  override def address(name: String): String = s"conf:$name"
  private lazy val server = ForeignServer.listen()
  private lazy val dialling =
    val d = java.nio.file.Files.createTempDirectory("okay-ts-served")
    java.nio.file.Files.writeString(d.resolve("conf.ts"), TsConformance.conf): Unit
    Dialling(TsWorker.command(d, Seq("conf")), server.port)
  lazy val engine: ForeignWorker =
    dialling: Unit
    server.accept()
  override def afterAll(): Unit = if TsConformance.node then
    engine.close()
    dialling.close()
    server.close()

/** the server itself: a silent far side refused by name, the next one served,
 * each on its own thread and closed after its session (foreign-server) */
class TestForeignServer extends okay.testkit.Munit.Diagnosed {

  private val hello = """{"shim": 9, "python": "*", "speaks": {"format": ["json"], "compress": [], "frames": []}}"""

  test("a far side that connects and says nothing is refused by name, its connection closed; the next is accepted") {
    val server = ForeignServer.listen(helloMillis = 300)
    try
      val silent = Socket("127.0.0.1", server.port)
      val refused = intercept[IllegalStateException](server.accept())
      note(refused.getMessage)
      assert(refused.getMessage.contains("said nothing for 300ms"), refused.getMessage)
      assertEquals(silent.getInputStream.read(), -1, "its connection closed")
      silent.close()
      val speaking = Socket("127.0.0.1", server.port)
      speaking.getOutputStream.write((hello + "\n").getBytes(UTF_8))
      speaking.getOutputStream.flush()
      val w = server.accept()
      w.close()
      speaking.close()
    finally server.close()
  }

  test("serve: every far side its own session, its engine closed after it; a refused one does not stop the server") {
    val server = ForeignServer.listen(helloMillis = 300)
    val served = java.util.concurrent.LinkedBlockingQueue[String]()
    val failures = java.util.concurrent.LinkedBlockingQueue[String]()
    try
      server.serve(_ => served.put("session"), e => failures.put(e.getMessage))
      val silent = Socket("127.0.0.1", server.port)
      def client(): Socket =
        val c = Socket("127.0.0.1", server.port)
        c.getOutputStream.write((hello + "\n").getBytes(UTF_8))
        c.getOutputStream.flush()
        c
      val a = client()
      val b = client()
      val sec = java.util.concurrent.TimeUnit.SECONDS
      assertEquals(Option(served.poll(10, sec)), Some("session"))
      assertEquals(Option(served.poll(10, sec)), Some("session"))
      assertEquals(a.getInputStream.read(), -1, "a's connection closed after its session")
      assertEquals(b.getInputStream.read(), -1, "b's connection closed after its session")
      val f = Option(failures.poll(10, sec))
      note(f.toString)
      assert(f.exists(_.contains("said nothing")), f)
      Seq(silent, a, b).foreach(_.close())
    finally server.close()
  }
}
