package okay2.http

import java.util.concurrent.TimeUnit
import okay2.{!, Pure, Resource}
import okay2.async.Async
import okay2.platform._

/**
 * The acceptance run: a JS client against a JVM server, one
 * shared-source program (okay-jetty's TestAcceptance for okay-http).
 *
 * The JS transports are `js.native` facades: a mistyped member is
 * `undefined`, not a compile error, so a transport can be entirely
 * broken and entirely green until it runs. This is the run. The JS side
 * is linked as a Node program (`Client`, by Test/compile), the JVM
 * serves, `node main.js <port>` runs, and exit 0 is the acceptance —
 * both ends run `Acceptance.check`, so a difference between platforms
 * is a failure, not two green suites checking two different things.
 */
class TestAcceptance extends Live {

  val clientJs: Option[String] =
    Option(System.getProperty("okay2.http.client.js")).filter(p => new java.io.File(p).isFile)

  /** run `use` while one port serves Acceptance's routes and its echo */
  def serving[A](use: Int => A): A =
    !.run(Resource.run[A, Pure](
      Server.serve(0)(r => Acceptance.routes.applyOrElse(r, (_: Request) => Server.notFound)).map { s =>
        val front = new AcceptanceServer(Server.port(s))
        try use(front.port) finally front.close()
      }))

  test("the JVM side runs the acceptance against its own transports") {
    // the control: if this fails the fixture is wrong, not the JS
    // transports, and the run below would blame the wrong side
    val results = serving(port =>
      !.run(Async.run[Seq[(String, Boolean)], Pure](Acceptance.check(Transports.http(), Transports.sockets(), port))))
    assertEquals(results.filterNot(_._2).map(_._1), Nil, s"on the JVM: $results")
    assertEquals(results.length, 4)
  }

  test("a JS client drives the JVM server: the same program, over fetch and WebSocket") {
    assume(clientJs.isDefined, "no linked JS client; run through sbt so Test/compile links it")
    val (code, out) = serving { port =>
      // output to a file, so a client that hangs is a timeout, not a
      // read that never returns
      val log = java.io.File.createTempFile("okay2-http-client", ".log")
      log.deleteOnExit()
      val pb = new ProcessBuilder("node", clientJs.get, port.toString)
      pb.redirectErrorStream(true)
      pb.redirectOutput(log)
      val p = pb.start()
      val finished = p.waitFor(60, TimeUnit.SECONDS)
      if (!finished) p.destroyForcibly(): Unit
      val text = new String(java.nio.file.Files.readAllBytes(log.toPath), "UTF-8")
      (if (finished) p.exitValue else -1, text)
    }
    assertEquals(code, 0, s"the JS client failed:\n$out")
    // every line it printed is an `ok`, and there are four
    assertEquals(out.linesIterator.count(_.startsWith("ok")), 4, out)
  }
}
