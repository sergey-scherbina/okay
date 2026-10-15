package okay.foreign

import java.net.URI
import java.net.http.{HttpClient, WebSocket}
import java.util.concurrent.{CompletionStage, LinkedBlockingQueue, TimeUnit}

import okay.Async
import okay.given
import okay.freer.*
import okay.freer.given
import okay.http.{Request, Response}
import okay.jetty.Jetty
import okay.std.*

/** a far side over a JDK WebSocket: says hello, then answers each request with `answer` (None: closes instead) */
final class WsFarSide(url: String, answer: okay.codec.Json => Option[String]):
  val closed = LinkedBlockingQueue[Int]()
  private val hello = """{"shim": 9, "python": "*", "speaks": {"format": ["json"], "compress": [], "frames": []}}"""
  val socket: WebSocket = HttpClient.newHttpClient().newWebSocketBuilder()
    .buildAsync(URI.create(url), new WebSocket.Listener:
      private val text = StringBuilder()
      override def onOpen(ws: WebSocket): Unit =
        ws.sendText(hello, true): Unit
        ws.request(1)
      override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] =
        text.append(data)
        if last then
          val req = okay.codec.Json.parse(text.toString)
          text.clear()
          answer(req) match
            case Some(a) => ws.sendText(a, true): Unit
            case None => ws.sendClose(WebSocket.NORMAL_CLOSURE, "gone"): Unit
        ws.request(1)
        null
      override def onClose(ws: WebSocket, code: Int, reason: String): CompletionStage[?] =
        closed.put(code)
        null
    ).get(10, TimeUnit.SECONDS)

object TestForeignWs:
  import okay.codec.Json.*
  def field(j: okay.codec.Json, k: String): okay.codec.Json = j match
    case JObj(fs) => fs.toMap.getOrElse(k, JNull)
    case _ => JNull
  def num(j: okay.codec.Json): Long = j match
    case JNum(n) => n.toLong
    case other => throw IllegalArgumentException(s"not a number: $other")

  /** `pairs` as a far side plays it: two choices, their sum; a rest's id carries the first choice, so the same
   * rest continued twice answers twice (multi-shot) */
  def pairs(req: okay.codec.Json): Option[String] =
    val id = num(field(req, "id"))
    field(req, "op") match
      case JStr("program") => Some(s"""{"id": $id, "ok": {"perform": "choose", "args": [[1, 2]], "k": 1}}""")
      case JStr("continue") =>
        val k = num(field(req, "k"))
        val a = num(field(req, "answer"))
        if k == 1 then Some(s"""{"id": $id, "ok": {"perform": "choose", "args": [[10, 20]], "k": ${1 + a}}}""")
        else Some(s"""{"id": $id, "ok": {"done": ${(k - 1) + a}}}""")
      case _ => Some(s"""{"id": $id, "ok": null}""")

/** the okay wire over a WebSocket a Jetty server accepted (foreign-ws): the far side a JDK client playing the
 * transcript — a program to done, a rest continued twice; the socket closed after the session; a far side gone
 * mid-run ending the engine's wait */
class TestForeignWs extends okay.testkit.Munit.Diagnosed {
  import TestForeignWs.*
  import okay.codec.Json.JStr

  private val choose = Foreign.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))
  private val routes: PartialFunction[Request, Response ! Async] = PartialFunction.empty

  private def serving[A](session: ForeignWorker => Unit, failed: Throwable => Unit = _ => ())(body: Int => A): A =
    val ws = ForeignWs.route("/okay", failed)(session)
    Resource.run[A, Pure](Jetty.serve(0)(routes)(ws.sessions, ws.pushes).map(s => body(Jetty.port(s)))).runWith

  test("a far side over a WebSocket: a program to done, a rest continued twice — every branch of two choices") {
    val got = LinkedBlockingQueue[List[Either[Condition, Long]]]()
    serving { w =>
      given okay.Answers[ForeignEval] = w.handler
      given Shape = Shape.python
      val run = Foreign.program[Long]("pairs").calling(Foreign.callbacks(choose))()
      got.put(runChoice(run.program).runWith.toList)
      run.forget.runWith
    } { port =>
      val far = WsFarSide(s"ws://127.0.0.1:$port/okay", pairs)
      val paths = got.poll(20, TimeUnit.SECONDS)
      note(String.valueOf(paths))
      assertEquals(paths, List(Right(11L), Right(21L), Right(12L), Right(22L)))
      assertEquals(Option(far.closed.poll(10, TimeUnit.SECONDS)), Some(1000), "the socket closed after the session")
    }
  }

  test("a far side gone mid-run ends the engine's wait: the in-flight call throws, told to `failed`") {
    val failures = LinkedBlockingQueue[String]()
    serving({ w =>
      given okay.Answers[ForeignEval] = w.handler
      given Shape = Shape.python
      val paths = runChoice(Foreign.program[Long]("pairs").calling(Foreign.callbacks(choose))().program).runWith
      note(s"not reached: $paths")
    }, e => failures.put(String.valueOf(e.getMessage))) { port =>
      WsFarSide(s"ws://127.0.0.1:$port/okay", req => if field(req, "op") == JStr("program") then None else pairs(req))
      val f = failures.poll(20, TimeUnit.SECONDS)
      note(String.valueOf(f))
      assert(f != null, "the session's failure reported")
    }
  }
}

/** (okay-ts, WebSocket): the conformance suite, its far side okay-ts connecting over a WebSocket. LIVE — node and
 * an okay-ts checkout (`OKAY_TS`) */
class TestOkayTsWs extends WireConformance:
  override def munitIgnore: Boolean = !OkayTs.ready
  override def tables: Boolean = false
  override def holds: Boolean = false
  private val engines = LinkedBlockingQueue[ForeignWorker]()
  private val done = java.util.concurrent.CountDownLatch(1)
  private lazy val server =
    val ws = ForeignWs.route("/okay") { w =>
      engines.put(w)
      done.await()
    }
    Jetty.serve(0)(PartialFunction.empty)(ws.sessions, ws.pushes)
  private lazy val port: Int =
    val ready = LinkedBlockingQueue[Int]()
    val t = Thread(() =>
      Resource.run[Unit, Pure](server.map { s =>
        ready.put(Jetty.port(s))
        done.await()
      }).runWith)
    t.setDaemon(true)
    t.start()
    ready.take()
  private lazy val client = OkayTs.client(OkayTs.conformance.replace("connect({ port: Number(process.argv[2]) }",
    "connectWs(`ws://127.0.0.1:${process.argv[2]}/okay`").replace("import { Do, connect,", "import { Do, connectWs,"), port)
  lazy val engine: ForeignWorker =
    client: Unit
    engines.poll(20, TimeUnit.SECONDS)
  override def afterAll(): Unit = if OkayTs.ready then
    done.countDown()
    client.destroy()
