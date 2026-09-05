# Building a chat application on Okay, from an empty directory

A streaming LLM chat — a web page, a server, a real token stream —
built OUTSIDE this repository, as a user of the library rather than a
contributor to it. Every command below was executed in a scratch
project before it was written down, in the order it appears, and the
outputs quoted are the ones that came back.

What you end with: a `POST /chat` route that streams tokens as SSE
under a token budget, a React page whose logic is a pure Scala fold
tested on the JVM, three server tests over a real socket, and one
`sbt app/run` that serves the whole thing. About 200 lines of your
own code.

The finished thing this is a small copy of is
[`okay-demo`](modules/okay-demo.md)'s `ChatDemo` — the same route,
the same UI construction, plus a task board, sessions, an admin
surface and monitoring. When you want more than this tutorial gives,
read that.

**Prerequisites**: JDK 21+, sbt 1.13, and this repository cloned
somewhere. No Node, no npm — Scala.js links to a plain `.js` file
with no JavaScript toolchain involved.

---

## 1. Get the library — it is not published yet

Okay is not on Maven Central. That makes this the one step that is
genuinely awkward, so it comes first and gets the most words.

The road that works and costs nothing: publish it to your own machine.

```
cd /path/to/okay
sbt publishLocal
```

Measured on this machine: **38 seconds, 86 artifacts**, into
`~/.ivy2/local/dev.okay/`. It publishes every module for every
platform it cross-builds — JVM (`okay_3`), Scala.js (`okay_sjs1_3`)
and Native (`okay_native0.5_3`) — which is why one command is enough
for both halves of an application. Nothing is downloaded from the
network for the modules themselves.

The coordinates you then depend on:

| | |
|---|---|
| organization | `dev.okay` |
| version | `0.1.0-SNAPSHOT` (`ThisBuild / version` in okay's build.sbt) |
| Scala | 3.7.4 — use this or newer; 3.6 is the floor for the syntax the library uses |
| JVM artifact | `"dev.okay" %% "okay-jetty" % "0.1.0-SNAPSHOT"` |
| JS artifact | `"dev.okay" %%% "okay-ui" % "0.1.0-SNAPSHOT"` (`%%%` picks `_sjs1_3`) |

Two things bite here. Your Scala version must be **at least** the one
the library was built with (TASTy is forward-, not backward-,
compatible) — 3.7.4 published, so 3.7.4 or newer in your build. And
`%%` is the JVM artifact while `%%%` picks the platform's; in a
crossProject you always want `%%%`.

If you re-publish after changing okay, your build will not always
notice a SNAPSHOT has moved: `sbt reload` in your project, or delete
`~/.ivy2/local/dev.okay/<module>` and publish again.

**Two other roads**, worth knowing but not what this tutorial uses:

- **A source dependency.** In your `build.sbt`,
  `.dependsOn(ProjectRef(file("../okay"), "okayJettyJVM"))` — sbt
  compiles okay as part of your build. No publishing step at all, and
  an edit to okay is picked up by your next compile; the cost is that
  your build now compiles a large library and your IDE indexes it.
  Good while you are changing okay itself, heavy otherwise.
- **Unmanaged jars.** Copy the jars into `lib/`. Do not: you lose the
  transitive dependencies (okay-jetty pulls okay-http, okay, jetty
  itself) and you will spend the afternoon rediscovering them by
  `NoClassDefFoundError`.

---

## 2. Scaffold the project

Two projects: the **brain** cross-compiles (its tests run on the JVM,
its code runs in the browser), the **server** is JVM only.

```
chat-app/
├── build.sbt
├── project/
│   ├── build.properties
│   └── plugins.sbt
├── app/src/main/scala/example/Server.scala
├── app/src/test/scala/example/ServerTest.scala
└── ui/src/
    ├── main/scala/example/ChatUi.scala       ← pure, both platforms
    ├── main/scala-js/example/Main.scala      ← browser glue, JS only
    └── test/scala/example/ChatUiTest.scala   ← runs on the JVM
```

`project/build.properties`:

```
sbt.version = 1.13.0
```

`project/plugins.sbt`:

```scala
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
```

`build.sbt`:

```scala
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / scalaVersion := "3.7.4"
ThisBuild / organization := "example"

val okay = "0.1.0-SNAPSHOT"

// the brain: one source, compiled twice — the JVM copy is what the
// tests run, the JS copy is what the browser runs
lazy val ui = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("ui"))
  .settings(
    name := "chat-ui",
    libraryDependencies ++= Seq(
      "dev.okay" %%% "okay-ui" % okay,
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
    ),
  )
  .jsSettings(
    // the browser glue lives only on the JS side
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "src" / "main" / "scala-js",
    scalaJSUseMainModuleInitializer := true,
  )

lazy val app = project
  .in(file("app"))
  .settings(
    name := "chat-app",
    libraryDependencies ++= Seq(
      "dev.okay" %% "okay-jetty" % okay,
      "dev.okay" %% "okay-chat" % okay,
      "org.scalameta" %% "munit" % "1.1.1" % Test,
    ),
    run / fork := true,
    run / connectInput := true,
    // a forked run's working directory is the PROJECT's, so a
    // relative path to the linked bundle would miss — run from the
    // build root, the way the demo does
    run / baseDirectory := (ThisBuild / baseDirectory).value,
  )
```

Three dependencies carry the whole application:
[`okay-jetty`](modules/okay-jetty.md) (an HTTP server behind okay's
own seam), [`okay-chat`](modules/okay-chat.md) (the model seam, the
SSE framing and the `/chat` route) and
[`okay-ui`](modules/okay-ui.md) (the view as a value). Each pulls
what it needs — okay-chat brings okay-llm, okay-http and okay-conf;
okay-jetty brings okay-http and Jetty.

---

## 3. The backend

`app/src/main/scala/example/Server.scala`:

```scala
package example

import okay.*
import okay.given
import okay.chat.Chat
import okay.conf.Secrets
import okay.http.{Http, Method, Request, Response}
import okay.jetty.Jetty
import okay.llm.Transports
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

object Server:

  /** the page: React from a CDN, our linked Scala.js at /app.js */
  val page: String = """<!doctype html>
<meta charset="utf-8">
<title>chat</title>
<body style="font:15px system-ui;max-width:640px;margin:2rem auto">
<div id="root"></div>
<script crossorigin src="https://cdnjs.cloudflare.com/ajax/libs/react/18.3.1/umd/react.production.min.js"></script>
<script crossorigin src="https://cdnjs.cloudflare.com/ajax/libs/react-dom/18.3.1/umd/react-dom.production.min.js"></script>
<script src="/app.js"></script>
"""

  /** the linked bundle, if `sbt uiJS/fastLinkJS` has been run */
  def appJs: Option[Path] =
    Some(Path.of(sys.env.getOrElse("APP_JS",
      "ui/.js/target/scala-3.7.4/chat-ui-fastopt/main.js"))).filter(Files.exists(_))

  def html(body: String): Response =
    Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
      Http.one(body.getBytes(UTF_8)))

  /** the whole server: okay-chat's route, plus two static ones */
  def routes(model: Chat.Model, budget: Int = 512)
  : PartialFunction[Request, Response ! Async] =
    Chat.chatRoute(model, budget).orElse {
      case r if r.method == Method.Get && r.url == "/" => pure(html(page))
      case r if r.method == Method.Get && r.url == "/app.js" && appJs.isDefined =>
        pure(Response(200, Seq("content-type" -> "text/javascript"),
          Http.one(Files.readAllBytes(appJs.get))))
    }

  @main def serve(): Unit =
    val port = sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(8080)
    provide(Transports.http(), Secrets.env)(
      Resource.run[Unit, Pure](Jetty.serve(port)(routes(Chat.model))().map { s =>
        println(s"chat: http://127.0.0.1:${Jetty.port(s)}  (model: ${Chat.modeName})")
        Thread.sleep(Long.MaxValue)
      }).runWith)
```

What is worth noticing, because it is the library's shape rather than
this application's:

**A route is a `PartialFunction[Request, Response ! Async]`** — a
plain value. `orElse` composes routes because that is what partial
functions do; no router DSL is involved, and `Chat.chatRoute` is just
another one you did not have to write.

**`Chat.Model` is the whole model seam**: `Seq[Message] => Unit !
(Writer % String + Async)` — history in, tokens out as a `Writer`
stream. `Chat.scripted` fits it offline, `Chat.live(key)` speaks to
Anthropic, `Chat.local(base)` to any OpenAI-compatible endpoint, and
`Chat.model` picks among them by what the ambient `Secrets` holds. The
route never learns which one it got.

**`provide(...)` is the wiring** ([capabilities](capabilities.md)).
`Transports.http()` and `Secrets.env` are installed once at the
process edge as context functions; `Chat.model` reads them through
`using`. A test installs different ones — that is section 5, and it
is the reason the tests need no network.

**`Jetty.serve(port)(routes)()` answers a `Resource`**, so the server
is closed by the region ending, not by a `finally` you remember to
write. `Resource.run` runs the region; `.runWith` runs the program.

**`Chat.chatRoute(model, budget)`** gives you the streaming and the
guard for free: tokens are framed as SSE `data:` events, a completed
turn ends with `event: done`, and a turn that runs past `budget`
tokens is cut with `event: cut` naming the rule
([okay-llm](modules/okay-llm.md)'s `Cut`).

---

## 4. The frontend

The split that makes this testable: **the brain is pure and cross-
compiled, the glue is browser-only and decides nothing.**

`ui/src/main/scala/example/ChatUi.scala`:

```scala
package example

import okay.ui.{Event, Style, Ui}

object ChatUi:

  final case class Msg(role: String, text: String, cut: Option[String] = None)

  final case class State(messages: Vector[Msg] = Vector.empty,
                         draft: String = "",
                         busy: Boolean = false)

  /** what the glue should DO after an event — the fold decides, the
   * browser obeys */
  enum Go:
    case Send(history: Vector[Msg])
    case Stay

  def view(s: State): Ui =
    Ui.Column(Vector(
      Ui.Text("okay chat", Style(bold = true)),
      Ui.Column(s.messages.zipWithIndex.map { (m, i) =>
        val bubble = Ui.Text((if m.role == "user" then "you: " else "bot: ") + m.text)
        m.cut match
          case Some(rule) => Ui.Column(Vector(bubble,
            Ui.Text(s"cut: $rule", Style(dim = true))), key = s"m$i")
          case None => Ui.Row(Vector(bubble), key = s"m$i")
      }, key = "log"),
      Ui.Row(Vector(
        Ui.Input(s.draft, key = "draft", label = ""),
        Ui.Button(if s.busy then "..." else "send", key = "send")), key = "bar")))

  def update(s: State, e: Event): (State, Go) = e match
    case Event.Edited("draft", v) => (s.copy(draft = v), Go.Stay)
    case Event.Pressed("send") if !s.busy && s.draft.trim.nonEmpty =>
      val history = s.messages :+ Msg("user", s.draft.trim)
      (State(history :+ Msg("assistant", ""), "", busy = true), Go.Send(history))
    case Event.Edited("$token", t) if s.busy =>
      val last = s.messages.last
      (s.copy(messages = s.messages.init :+ last.copy(text = last.text + t)), Go.Stay)
    case Event.Pressed("$done") => (s.copy(busy = false), Go.Stay)
    case Event.Edited("$cut", rule) =>
      val last = s.messages.last
      (s.copy(busy = false, messages = s.messages.init :+ last.copy(cut = Some(rule))), Go.Stay)
    case _ => (s, Go.Stay)
```

`Ui` is a value — a tree of `Column`/`Row`/`Text`/`Input`/`Button`,
no DOM, no React, nothing platform-specific. The streaming protocol
is three private event keys the glue feeds back in: `$token` appends
to the open bubble, `$done` closes the turn, `$cut` marks the guard's
scissors. They arrive through the SAME `update` as a click, which is
why streaming needs no separate machinery.

`ui/src/main/scala-js/example/Main.scala` — the glue. It mounts
okay-ui's tree on the CDN React, runs the fold on the event loop
(there is no blocking on JS: the loop IS the runner), and pumps the
`fetch` body's reader, turning SSE frames back into events:

```scala
package example

import okay.*
import okay.given
import okay.ui.{Event, React, ReactJs, Ui}
import scala.scalajs.js
import js.Dynamic.global as g

object Main:

  private val bus = Channel[Event]()
  private var current: Ui = Ui.Text("")

  def main(args: Array[String]): Unit =
    val react = g.React
    val root = g.ReactDOM.createRoot(g.document.getElementById("root"))
    def render(ui: Ui): Unit =
      current = ui
      val _ = root.render(ReactJs.element(react, React.elem(ui),
        e => bus.offer(e): Unit, () => current))
      ()

    def loop(s: ChatUi.State): Unit ! Async =
      async(render(ChatUi.view(s))).flatMap { _ =>
        Writer.uncons[Event, Unit, Async](Writer.of(bus)).flatMap {
          case Left(_) => pure(())
          case Right((e, _)) =>
            val (s2, go) = ChatUi.update(s, e)
            go match
              case ChatUi.Go.Send(history) => send(history)
              case ChatUi.Go.Stay => ()
            loop(s2)
        }
      }

    val _ = Async.runAsync(loop(ChatUi.State()))

  /** POST the history, read the SSE frames off the body stream */
  private def send(history: Vector[ChatUi.Msg]): Unit =
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
```

This file is the only one you cannot test without a browser, and it
is the only one with no decisions in it. That is the trade being
made on purpose.

---

## 5. Tests

**The brain, on the JVM, with scripted events.** `Frame.render`
renders a `Ui` tree to text lines, so an assertion about what the
user sees is an assertion about a string:

```scala
package example

import okay.ui.{Event, Frame}
import ChatUi.*

class ChatUiTest extends munit.FunSuite:

  def text(s: State): String = Frame.render(view(s)).mkString("\n")

  test("send opens a turn: history captured, draft cleared, busy") {
    val (s1, _) = update(State(), Event.Edited("draft", "hello"))
    val (s2, go) = update(s1, Event.Pressed("send"))
    assertEquals(go, Go.Send(Vector(Msg("user", "hello"))))
    assertEquals(s2.draft, "")
    assert(s2.busy)
    // a second send while a turn is open is refused by the fold itself
    val (s3, go3) = update(s2.copy(draft = "again"), Event.Pressed("send"))
    assertEquals(go3, Go.Stay)
    assertEquals(s3.messages.length, 2)
  }

  test("tokens append to the open bubble; done closes the turn") {
    var s = update(update(State(), Event.Edited("draft", "hi"))._1, Event.Pressed("send"))._1
    for t <- Vector("Hello", " ", "there") do s = update(s, Event.Edited("$token", t))._1
    s = update(s, Event.Pressed("$done"))._1
    assertEquals(s.messages.last.text, "Hello there")
    assert(!s.busy)
    assert(text(s).contains("bot: Hello there"))
  }
```

**The server, over a real socket.** Port `0` means the OS picks a free
one — never hardcode a port in a test, it is the most reliable way to
manufacture a flake. The wire is a `Transport` that THROWS: the test
proves the offline path never reaches the network, rather than
trusting that it does not.

```scala
package example

import okay.*
import okay.given
import okay.jetty.Jetty
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

class ServerTest extends munit.FunSuite:

  val deadWire: okay.llm.Transport = (url, _, _) =>
    throw new AssertionError(s"an offline test touched the wire: $url")
  val noSecrets: okay.conf.Secrets = okay.conf.Secrets.memory(Map.empty)

  def withServer[A](budget: Int = 512)(f: Int => A): A =
    provide(deadWire, noSecrets)(Resource.run[A, Pure](
      Jetty.serve(0)(Server.routes(okay.chat.Chat.scripted, budget))()
        .map(s => f(Jetty.port(s)))).runWith)

  val client: HttpClient = HttpClient.newHttpClient()

  def post(port: Int, path: String, body: String): HttpResponse[String] =
    client.send(HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port$path"))
      .header("content-type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(body)).build(),
      HttpResponse.BodyHandlers.ofString())

  test("a turn streams SSE frames and ends with done") {
    withServer() { port =>
      val r = post(port, "/chat", """{"messages":[{"role":"user","content":"hi"}]}""")
      assertEquals(r.statusCode(), 200)
      assertEquals(r.headers().firstValue("content-type").orElse(""), "text/event-stream")
      assert(r.body().contains("data: "), r.body())
      assert(r.body().contains("hi"), r.body())
      assert(r.body().trim.endsWith("event: done\ndata:"), r.body())
    }
  }

  test("the budget cuts a long generation, and says which rule cut it") {
    withServer(budget = 3) { port =>
      val r = post(port, "/chat", """{"messages":[{"role":"user","content":"hi"}]}""")
      assert(r.body().contains("event: cut"), r.body())
      assert(r.body().contains("token-budget"), r.body())
    }
  }
```

Run them:

```
sbt uiJVM/test app/test
```

```
[info] Passed: Total 2, Failed 0, Errors 0, Passed 2
[info] Passed: Total 3, Failed 0, Errors 0, Passed 3
```

Note what is NOT here: no browser, no mock server, no HTTP stubbing
library, no docker. The scripted model and the ephemeral port are
enough because the seams are values.

If you later add a test that needs a real model or a real service,
tag it so it stays out of your default gate — the convention this
repository settled on is in
[specs/integration-test-gate.md](../specs/integration-test-gate.md):
one `munitTests()` override per suite, and `--exclude-tags=Live` in
`ThisBuild / Test / testOptions`.

---

## 6. Run it

```
sbt uiJS/fastLinkJS      # link the frontend → ui/.js/target/scala-3.7.4/chat-ui-fastopt/main.js
sbt app/run              # serve on :8080 (PORT to change it)
```

```
chat: http://127.0.0.1:8080  (model: scripted (no model — set OKAY_CHAT_BASE or ANTHROPIC_API_KEY))
```

Verified from the other side of a socket:

```
$ curl -s -o /dev/null -w "%{http_code} %{content_type}\n" http://127.0.0.1:8080/
200 text/html; charset=utf-8

$ curl -s -o /dev/null -w "%{http_code} %{size_download}\n" http://127.0.0.1:8080/app.js
200 1341412

$ curl -s -X POST -H 'content-type: application/json' \
    -d '{"messages":[{"role":"user","content":"привет"}]}' \
    http://127.0.0.1:8080/chat
data: "You "

data: "said: "

data: "привет "
...
```

**Switching the model** costs no code change — `Chat.model` reads the
ambient `Secrets`, which `Secrets.env` fills from the environment:

| | |
|---|---|
| nothing set | the scripted model — deterministic, offline, streams the same way |
| `OKAY_CHAT_BASE=http://127.0.0.1:8089` | any OpenAI-compatible endpoint (a local model server) |
| `ANTHROPIC_API_KEY=…` | Anthropic, streamed |

`fastLinkJS` is the development link (fast, large). For a release,
`fullLinkJS` optimizes — the output lands in `chat-ui-opt/main.js`, so
point `APP_JS` at it.

---

## 7. Where to go from here

Each of these is one dependency and a few lines, and each has a
module page with its own worked shape:

- **Remember the conversation** — [`okay-persist`](modules/okay-persist.md),
  the durable log. Append every turn to a topic; replay it to rebuild
  anything you derive from it.
- **Be monitored** — [`okay-ops`](modules/okay-ops.md) adds
  `/healthz`, `/readyz`, `/stats` and `/metrics` over the log's own
  values, no SDK.
- **Be deployed** — [`okay-deploy`](modules/okay-deploy.md): a
  deployment is a value, rendered to a Dockerfile, a Helm chart and a
  compose file, with a test that the committed files have not drifted
  from it.
- **Do something with the conversation** —
  `Board` is what the demo does with it: a shared task list the
  model reaches only through tools, rebuilt from its own log.
- **Give the model tools** — [`okay-agent`](modules/okay-agent.md)
  and [`okay-mcp`](modules/okay-mcp.md): tools are operations, and an
  MCP server is a `Handler[Tool]`.
- **A different server** — [`okay-netty`](modules/okay-netty.md)
  behind the same seam; the same routes, one dependency swapped.

For the library itself rather than an application built on it: the
[user guide](guide.md) for the concepts and the
[tutorial](tutorial.md) for twenty-two worked chapters.

---

## Troubleshooting

Both of these happened while writing this page; they are here because
they will happen to you.

**`No given instance of type okay.llm.Transport was found`** — there
are two `Transports` objects, and the import decides which. The model
seam wants `okay.llm.Transports.http()`, not the one in `okay.http`.

**`GET /app.js` answers 404 although the link succeeded** — a forked
`run` uses the PROJECT's directory as its working directory
(`chat-app/app/`), so a relative path to `ui/.js/...` resolves in the
wrong place. Either set `run / baseDirectory := (ThisBuild /
baseDirectory).value` as the build above does, or pass an absolute
`APP_JS`.

**`not found: dev.okay#okay-jetty_3;0.1.0-SNAPSHOT`** — `publishLocal`
has not run, or it ran from a different okay checkout than you think.
Check `ls ~/.ivy2/local/dev.okay/`.

**Changes to okay do not show up** — a SNAPSHOT already resolved is
cached for the session. `sbt reload` in your project after
re-publishing.
