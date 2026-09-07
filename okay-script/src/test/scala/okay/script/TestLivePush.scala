package okay.script

import okay.*
import okay.given
import okay.codec.Json
import okay.http.{Frame, Request, Transports, Ws}
import okay.jetty.Jetty
import okay.ui.{Event, Patch, WireJson}

import java.nio.file.{Files, Path}

/** script-live-push: a Live page whose app pushes its own events —
 * in-JVM the pushed frames and the patches they cause; over Jetty
 * (Live-tagged) the patches arrive with nobody pressing. */
class TestLivePush extends munit.FunSuite:

  override def munitTests(): Seq[Test] =
    super.munitTests().map(t => if t.name.startsWith("over Jetty") then t.tag(new munit.Tag("Live")) else t)

  private val page =
    """```scala declare
      |import okay.*
      |import okay.given
      |import okay.ui.*
      |import okay.script.api.*
      |// the server's two ticks: each "presses" the shown button, as okay-ui's own timer test does
      |val ticks: Source[Event] = !.widen[Unit, Writer % Event, Async](Writer.of(List(Event.Pressed("inc"), Event.Pressed("inc"))))
      |val counter = Live(0)(n => Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+1", "inc"))))(
      |  (n, e) => e match { case Event.Pressed("inc") => n + 1; case _ => n }, push = ticks)
      |```
      |${mount("counter", counter)}
      |""".stripMargin

  private def withSite[A](f: Site => A): A =
    val root = Files.createTempDirectory("okay-script-live-push-")
    Files.writeString(root.resolve("counter.md"), page): Unit
    val site = Site(root)
    try f(site)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  test("in-JVM: the pushed source yields the app's events as frames, and fed to the stage they patch the tree") {
    withSite { site =>
      val served = site.ws(Request.get("/counter?__live=counter"))
      val pushed = Async.run[Seq[Frame], Pure](Writer.run(served.push).map(_._1)).runWith
      val events = pushed.collect { case Frame.Text(s) => WireJson.eventOf(Json.parse(s)) }
      assertEquals(events, Seq(Some(Event.Pressed("inc")), Some(Event.Pressed("inc"))))
      val (out, _) = !.run(Writer.run(through(Writer.of(pushed.toList :+ Frame.Close(1000, "")))(served.stage)))
      val lines = out.collect { case Frame.Text(s) => s }
      assertEquals(lines.length, 3, lines.toString)
      assertEquals(WireJson.patchOf(Json.parse(lines(1))), Some(Patch.SetText(List(0), "count: 1")))
      assertEquals(WireJson.patchOf(Json.parse(lines(2))), Some(Patch.SetText(List(0), "count: 2")))
    }
  }

  test("over Jetty: the tree, then two patches nobody pressed for") {
    withSite { site =>
      val got = Resource.run[Seq[Frame], Pure](
        Jetty.serve(0)(site.routes)(site.ws).map { server =>
          val sockets = Transports.sockets()
          Async.run[Seq[Frame], Pure](
            sockets.connect(s"ws://127.0.0.1:${Jetty.port(server)}/counter?__live=counter").flatMap { sock =>
              val listen: Stage[Frame, Frame, Seq[Frame]] =
                Stage.await[Frame, Frame].flatMap { a =>
                  Stage.await[Frame, Frame].flatMap { b =>
                    Stage.await[Frame, Frame].map(c => a.toSeq ++ b.toSeq ++ c.toSeq) } }
              Ws.over(sock)(listen).flatMap(fs => sock.close().map(_ => fs))
            }).runWith
        }).runWith
      val lines = got.collect { case Frame.Text(s) => s }
      assertEquals(lines.length, 3, got.toString)
      assert(WireJson.uiOf(Json.parse(lines(0))).isDefined, lines(0))
      assertEquals(WireJson.patchOf(Json.parse(lines(1))), Some(Patch.SetText(List(0), "count: 1")))
      assertEquals(WireJson.patchOf(Json.parse(lines(2))), Some(Patch.SetText(List(0), "count: 2")))
    }
  }
