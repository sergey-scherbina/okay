package okay.chatweb

import okay.*
import okay.given
import okay.ui.{Event, React, ReactJs, Ui}
import scala.scalajs.js
import js.Dynamic.global as g

/**
 * The glue, and only the glue (the ReactJs doctrine): mount okay-ui's
 * React mapping on the CDN React the page loads, run the Elm fold on
 * `runAsync` (there is no CanBlock on JS — the event loop IS the
 * runner), and feed the streaming reply back as `$token`/`$done`/
 * `$cut` events through the same bus the clicks arrive on. The brain
 * is ChatUi, cross and JVM-tested; nothing here decides anything.
 */
object Main {

  private val bus = Channel[Event]()
  private var current: Ui = Ui.Text("")

  def main(args: Array[String]): Unit =
    val react = g.React
    val root = g.ReactDOM.createRoot(g.document.getElementById("root"))
    def render(ui: Ui): Unit =
      current = ui
      val _ = root.render(ReactJs.element(react, React.elem(ui), e => bus.offer(e): Unit, () => current))
      ()

    def loop(s: ChatUi.State): Unit ! Async =
      async(render(ChatUi.view(s))).flatMap { _ =>
        Writer.uncons[Event, Unit, Async](Writer.of(bus)).flatMap {
          case Left(_) => pure(())
          case Right((e, _)) =>
            val (s2, go) = ChatUi.update(s, e)
            go match
              case ChatUi.Go.Send(history) => fetchReply(history)
              case ChatUi.Go.Stay => ()
            loop(s2)
        }
      }

    val _ = Async.runAsync(loop(ChatUi.State()))

  private var subscribed = false

  /** the reverse chain's delivery: once we know the user's email,
   * hold /events open — a match arriving LATER becomes a bubble */
  private def subscribe(email: String): Unit =
    if !subscribed then
      subscribed = true
      val es = js.Dynamic.newInstance(g.EventSource)(s"/events/$email")
      val _ = es.addEventListener("match", { (ev: js.Dynamic) =>
        bus.offer(okay.ui.Event.Edited("$match",
          js.JSON.parse(ev.data.asInstanceOf[String]).asInstanceOf[String])): Unit
      }: js.Function1[js.Dynamic, Unit])
      ()

  /** POST the history, parse the SSE frames, feed the bus */
  private def fetchReply(history: Vector[ChatUi.Msg]): Unit =
    history.lastOption.foreach(m =>
      "[\\w.+-]+@[\\w.-]+".r.findFirstIn(m.text).foreach(subscribe))
    val body = js.JSON.stringify(js.Dictionary(
      "messages" -> js.Array(history.map(m => js.Dictionary(
        "role" -> m.role, "content" -> m.text): js.Any)*)))
    val init = js.Dictionary[js.Any](
      "method" -> "POST",
      "headers" -> js.Dictionary("content-type" -> "application/json"),
      "body" -> body)
    val _ = g.fetch("/chat", init).`then` { (res: js.Dynamic) =>
      val reader = res.body.getReader()
      val decoder = js.Dynamic.newInstance(g.TextDecoder)()
      var buf = ""
      def frame(text: String): Unit =
        val ev = "(?m)^event: (.*)$".r.findFirstMatchIn(text).map(_.group(1)).getOrElse("data")
        val data = "(?m)^data: (.*)$".r.findFirstMatchIn(text).map(_.group(1)).getOrElse("")
        ev match
          case "data" => bus.offer(Event.Edited("$token",
            js.JSON.parse(data).asInstanceOf[String])): Unit
          case "cut" => bus.offer(Event.Edited("$cut", data)): Unit
          case _ => bus.offer(Event.Pressed("$done")): Unit
      def pump(): Unit =
        val _ = reader.read().`then` { (r: js.Dynamic) =>
          if r.done.asInstanceOf[Boolean] then
            if buf.nonEmpty then frame(buf)
          else
            buf += decoder.decode(r.value, js.Dictionary("stream" -> true)).asInstanceOf[String]
            var idx = buf.indexOf("\n\n")
            while idx >= 0 do
              frame(buf.take(idx)); buf = buf.drop(idx + 2)
              idx = buf.indexOf("\n\n")
            pump()
          (): js.Any
        }
        ()
      pump()
      (): js.Any
    }
    ()
}
