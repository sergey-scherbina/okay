package okay.script

import java.net.{HttpURLConnection, ServerSocket, URI}
import scala.util.Try

/** okay-script-lifecycle: a runtime-compiled script's long-lived app
 * (a real okay-jetty server) starts without blocking the caller and
 * stops CLEANLY on Thread.interrupt() -- see specs/okay-script.md
 * "Lifecycle". Binds a real port, so Live-tagged like every other
 * suite reaching outside the JVM.
 */
class TestScalaScriptLifecycle extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private def freePort(): Int =
    val s = new ServerSocket(0)
    try s.getLocalPort finally s.close()

  private def statusOf(url: String): Try[Int] =
    Try {
      val c = URI.create(url).toURL.openConnection().asInstanceOf[HttpURLConnection]
      c.setConnectTimeout(300)
      c.setReadTimeout(300)
      val code = c.getResponseCode
      c.disconnect()
      code
    }

  private def waitUntil(deadlineMs: Long)(cond: => Boolean): Boolean =
    val end = System.currentTimeMillis() + deadlineMs
    var ok = cond
    while !ok && System.currentTimeMillis() < end do
      Thread.sleep(50)
      ok = cond
    ok

  test("a script's Resource-run Jetty server answers while alive, and Thread.interrupt() stops it -- not just abandons it") {
    val port = freePort()
    val md =
      s"""```scala
         |import okay.*
         |import okay.given
         |import okay.jetty.Jetty
         |import okay.http.{Server as OkayServer}
         |
         |Resource.run[Unit, Pure](
         |  Jetty.serve($port) {
         |    case r if r.url == "/hello" => OkayServer.text(200, "hi from okay-script")
         |  }().map { s =>
         |    Thread.sleep(Long.MaxValue)
         |  }
         |).runWith
         |```
         |""".stripMargin

    @volatile var result: Option[Result] = None
    val t = new Thread(() => result = Some(ScalaScript.run(md)))
    t.setDaemon(true)
    t.start()

    val url = s"http://127.0.0.1:$port/hello"
    val up = waitUntil(10000)(statusOf(url).toOption.contains(200))
    assert(up, s"server never answered $url")

    t.interrupt()

    val down = waitUntil(10000)(statusOf(url).isFailure)
    assert(down, s"server kept answering $url after interrupt -- Resource release did not stop it")

    waitUntil(5000)(result.isDefined): Unit
    val r = result.getOrElse(fail("run() never returned after interrupt"))
    assert(!r.ok, r.toString)
    assert(r.thrown.exists(_.isInstanceOf[InterruptedException]), r.thrown.toString)
  }
