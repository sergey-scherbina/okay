package okay.script

import java.net.{HttpURLConnection, ServerSocket, URI}
import scala.util.Try

/** okay-script-storefront-example: the worked "generate a .md, compile
 * it at runtime, get a live web app" scenario, end to end -- against
 * `examples/it-consulting-storefront.md` on disk. Content is the real
 * IT-consulting business line's own services (../it-consulting/site/
 * site.md); see specs/okay-script.md "Worked example". Binds a real
 * port, so Live-tagged.
 */
class TestScalaScriptStorefront extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private def freePort(): Int =
    val s = new ServerSocket(0)
    try s.getLocalPort finally s.close()

  private def get(url: String): Try[(Int, String)] =
    Try {
      val c = URI.create(url).toURL.openConnection().asInstanceOf[HttpURLConnection]
      c.setConnectTimeout(300)
      c.setReadTimeout(300)
      val code = c.getResponseCode
      val body =
        val stream = if code < 400 then c.getInputStream else c.getErrorStream
        new String(stream.readAllBytes(), "UTF-8")
      c.disconnect()
      (code, body)
    }

  private def waitUntil(deadlineMs: Long)(cond: => Boolean): Boolean =
    val end = System.currentTimeMillis() + deadlineMs
    var ok = cond
    while !ok && System.currentTimeMillis() < end do
      Thread.sleep(50)
      ok = cond
    ok

  test("examples/it-consulting-storefront.md compiles and runs as a live storefront, stoppable by Thread.interrupt()") {
    val candidates = Vector(
      "examples/it-consulting-storefront.md",              // sbt fork CWD = okay-script/
      "okay-script/examples/it-consulting-storefront.md",  // CWD = repo root
    ).map(java.nio.file.Paths.get(_))
    val exampleFile = candidates.find(java.nio.file.Files.exists(_))
      .getOrElse(fail(s"missing example file, tried: ${candidates.mkString(", ")} (cwd=${System.getProperty("user.dir")})"))
    val md = java.nio.file.Files.readString(exampleFile)

    val port = freePort()
    System.setProperty("okay.script.storefront.port", port.toString)

    @volatile var result: Option[Result] = None
    val t = new Thread(() => result = Some(ScalaScript.run(md)))
    t.setDaemon(true)
    t.start()

    val root = s"http://127.0.0.1:$port/"
    val up = waitUntil(10000)(get(root).toOption.exists(_._1 == 200))
    assert(up, "storefront never came up")

    val (rootCode, rootBody) = get(root).get
    assertEquals(rootCode, 200)
    // all five services from ../it-consulting/site/site.md render
    assert(rootBody.contains("Добавление новых функций и развитие"), rootBody)
    assert(rootBody.contains("Исправление ошибок в системе"), rootBody)
    assert(rootBody.contains("Консультация"), rootBody)
    assert(rootBody.contains("Настройка CI/CD под ключ"), rootBody)
    assert(rootBody.contains("Искусственный интеллект, который решает"), rootBody)
    assert(rootBody.contains("4500"), rootBody)

    val (orderCode, orderBody) = get(s"http://127.0.0.1:$port/order/audit").get
    assertEquals(orderCode, 200)
    assert(orderBody.contains("Добавление новых функций и развитие"), orderBody)
    assert(orderBody.contains("принята"), orderBody)

    t.interrupt()
    val down = waitUntil(10000)(get(root).isFailure)
    assert(down, "storefront kept answering after interrupt")

    waitUntil(5000)(result.isDefined): Unit
    assert(result.exists(r => !r.ok && r.thrown.exists(_.isInstanceOf[InterruptedException])), result.toString)
  }
