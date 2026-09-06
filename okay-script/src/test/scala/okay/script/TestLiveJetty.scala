package okay.script

import okay.*
import okay.given
import okay.codec.Json
import okay.http.{Frame, Transports, Ws}
import okay.jetty.Jetty
import okay.ui.{Event, Patch, WireJson}

import java.nio.file.{Files, Path}

/** okay-script-live over a REAL Jetty WebSocket: connect to the page's
 * `?__live` socket, receive the tree, press, receive the patch.
 * Live-tagged like every suite that binds a port.
 */
class TestLiveJetty extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  test("a browser-shaped client over Jetty: tree, then a patch for a press") {
    val root = Files.createTempDirectory("okay-script-live-jetty-")
    Files.writeString(root.resolve("counter.md"),
      """```scala declare
        |import okay.ui.*
        |import okay.script.api.*
        |val counter = Live(0)(n => Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+1", "inc"))))(
        |  (n, e) => e match { case Event.Pressed("inc") => n + 1; case _ => n })
        |```
        |${mount("counter", counter)}
        |""".stripMargin): Unit
    val site = Site(root)
    try
      val got = Resource.run[Seq[Frame], Pure](
        Jetty.serve(0)(site.routes)(site.ws).map { server =>
          val sockets = Transports.sockets()
          Async.run[Seq[Frame], Pure](
            sockets.connect(s"ws://127.0.0.1:${Jetty.port(server)}/counter?__live=counter").flatMap { sock =>
              val say: Stage[Frame, Frame, Seq[Frame]] =
                Stage.await[Frame, Frame].flatMap { first =>
                  Stage.tell[Frame, Frame](Frame.Text(Json.print(WireJson.eventJson(Event.Pressed("inc"))))).flatMap(_ =>
                    Stage.await[Frame, Frame].map(second => first.toSeq ++ second.toSeq))
                }
              Ws.over(sock)(say).flatMap(fs => sock.close().map(_ => fs))
            }).runWith
        }).runWith
      val lines = got.collect { case Frame.Text(s) => s }
      assertEquals(lines.length, 2, got.toString)
      assert(WireJson.uiOf(Json.parse(lines(0))).isDefined, lines(0))
      assertEquals(WireJson.patchOf(Json.parse(lines(1))), Some(Patch.SetText(List(0), "count: 1")))
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }
