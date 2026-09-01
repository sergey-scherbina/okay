package okay.jetty

import okay.*
import okay.given
import okay.http.{Acceptance, Frame, Transports}

/**
 * The acceptance run: a JS client against a JVM server, one
 * shared-source program.
 *
 * `okay-http`'s JS transports compiled and had never run. That is the
 * failure `js.Dynamic` is worst at — a mistyped field is `undefined`,
 * not an error, so a transport can be entirely broken and entirely
 * green. This is the run that would have caught it.
 *
 * The shape is okay-cluster's: link the JS side as a Node program, have
 * the JVM serve, spawn `node main.js <port>`, and take exit 0 as the
 * acceptance. What makes it an acceptance rather than a smoke test is
 * that both ends run `Acceptance.check` — the same shared-source
 * program, the same schema, the same session — so a difference between
 * platforms shows up as a failure rather than as two green suites
 * checking two different things.
 *
 * It lives in okay-jetty because this is the module that can serve both
 * halves: `okay-http`'s own server does REST, and the JDK has no
 * server-side WebSocket at all.
 */
class TestAcceptance extends munit.FunSuite {

  val clientJs: Option[String] =
    Option(System.getProperty("okay.http.client.js")).filter(p =>
      java.io.File(p).isFile)

  test("the JVM side runs the acceptance against its own transports") {
    // the control: if this fails, the fixture is wrong rather than the
    // JS transports, and the run below would be blaming the wrong side
    val results = Resource.run[Seq[(String, Boolean)], Pure](
      Jetty.serve(0)(Acceptance.routes)({ case _ => Acceptance.echo }).map { server =>
        Async.run[Seq[(String, Boolean)], Pure](
          Acceptance.check(Transports.http(), Transports.sockets(),
            Jetty.port(server))).runWith
      }).runWith

    val failed = results.filterNot(_._2).map(_._1)
    assertEquals(failed, Nil, s"on the JVM: $results")
  }

  test("a JS client drives the JVM server — the same program, over fetch and WebSocket") {
    assume(clientJs.isDefined,
      "no linked JS client; run through sbt so Test/compile links it")

    val (code, out) = Resource.run[(Int, String), Pure](
      Jetty.serve(0)(Acceptance.routes)({ case _ => Acceptance.echo }).map { server =>
        val pb = ProcessBuilder("node", clientJs.get, Jetty.port(server).toString)
        pb.redirectErrorStream(true)
        val p = pb.start()
        val text = String(p.getInputStream.readAllBytes(), "UTF-8")
        val finished = p.waitFor(60, java.util.concurrent.TimeUnit.SECONDS)
        if !finished then { p.destroyForcibly(); (-1, text) }
        else (p.exitValue, text)
      }).runWith

    assertEquals(code, 0, s"the JS client failed:\n$out")
    // every line it printed should be an `ok`, and there should be four
    assertEquals(out.linesIterator.count(_.startsWith("ok")), 4, out)
  }
}
