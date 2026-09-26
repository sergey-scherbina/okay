package okay.foreign.workflow

import okay.{!, +, Delim, Pure, Wf}
import okay.Direct.*
import okay.codec.{Schema, WireAuth, WireFormat}
import okay.persist.{Dialogue, MemoryStore}
import okay.foreign.{ForeignEval, ForeignWorker, GoWorker, GoWorkerBinary, PyValue}
import scala.language.implicitConversions

object GoShop:
  val main: String = """package main

import (
	"sync"

	"worker/okay"
)

var mu sync.Mutex
var calls = map[string]int64{}

func count(name string) {
	mu.Lock()
	defer mu.Unlock()
	calls[name]++
}

var functions = okay.Functions{
	"price": func(c *okay.Ctx, args []any) any {
		count("price")
		return map[string]float64{"tea": 4.0, "cake": 2.5}[args[0].(string)]
	},
	"total": func(c *okay.Ctx, args []any) any {
		count("total")
		p, _ := okay.Float(args[0])
		n, _ := okay.Int(args[1])
		return p * float64(n)
	},
	"counted": func(c *okay.Ctx, args []any) any {
		mu.Lock()
		defer mu.Unlock()
		return calls[args[0].(string)]
	},
}

func main() {
	okay.Main(okay.Programs{}, functions)
}
"""

  lazy val binary: java.nio.file.Path =
    val dir = java.nio.file.Files.createTempDirectory("okay-go-shop")
    java.nio.file.Files.writeString(dir.resolve("main.go"), main): Unit
    GoWorker.build(dir)

  /** serve on `port` (0: any), with `env` added; the port and the process */
  def listen(port: Int, env: Map[String, String]): (Int, Process) =
    val pb = ProcessBuilder(binary.toString)
    pb.environment().put("OKAY_LISTEN", s"127.0.0.1:$port")
    env.foreach((k, v) => pb.environment().put(k, v))
    val p = pb.start()
    val first = java.io.BufferedReader(java.io.InputStreamReader(p.getInputStream, "UTF-8")).readLine()
    val bound = okay.codec.Json.parse(first) match
      case okay.codec.Json.JObj(fs) => fs.toMap.get("listening").collect { case okay.codec.Json.JStr(a) => a.split(":").last.toInt }
      case _ => None
    (bound.getOrElse { p.destroy(); throw IllegalStateException(s"the Go shop did not say where it listens: $first") }, p)

  def order(sku: String)(using w: Wf.Asks[ForeignCall, String, String, Pure]): String ! Delim + Pure = direct:
    val price = !ForeignActivity.call[Double]("price")(sku)
    price match
      case Left(c) => s"no price: ${c.kind}"
      case Right(p) =>
        val total = !ForeignActivity.call[Double]("total")(p, 3L)
        total.fold(c => s"no total: ${c.kind}", t => s"total $t")

/** foreign-workflow stage 1 in Go, over the stage 5/6 givens: a SUPERVISED,
 * authenticated, CBOR connection whose server is killed between two
 * activities of a durable workflow */
class TestForeignActivityGo extends munit.FunSuite:
  import GoShop.*
  import okay.given
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !GoWorkerBinary.available

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L, id = "id-1", dice = 0.25)
  given WireFormat = WireFormat.Cbor.cbor
  given WireAuth = WireAuth.secret("tea for two".getBytes)
  private val secret = Map("OKAY_WIRE_SECRET" -> "tea for two")

  test("a durable workflow's Go activities survive the Go SERVER dying between them") {
    val (port, first) = listen(0, secret)
    var server = first
    var restarted = false
    val w = ForeignWorker.supervised(ForeignWorker.connect("127.0.0.1", port))
    given okay.Handler[ForeignEval] = w.handler
    val topic = MemoryStore().topic("orders")
    try
      val oracle = (q: ForeignCall) =>
        if q.address == "total" && !restarted then
          // the server dies, and a new one comes up on the same port
          restarted = true
          server.destroyForcibly().waitFor(): Unit
          server = listen(port, secret)._2
        ForeignActivity.oracle(q)
      val run = Dialogue.workflow[ForeignCall, String, String, Pure](topic, "order-go", "order/1")(order("tea"))
        .runWorkflowIn(q => oracle(q))
      assertEquals(run.runWith, Right("total 12.0"))
      assertEquals(w.restarts, 1)
      // the NEW server did the second activity only
      def counted(name: String) =
        ForeignActivity.answer(ForeignActivity.oracle(ForeignCall("counted", Vector(PyValue.Str(name)))).runWith)
      assertEquals(counted("total"), Right(PyValue.I64(1L)))
      assertEquals(counted("price"), Right(PyValue.I64(0L)))
    finally
      w.close()
      server.destroy()
  }

  test("a worker unreachable through every attempt leaves the step UNANSWERED; the next run does it") {
    val (port, first) = listen(0, secret)
    first.destroyForcibly().waitFor(): Unit          // nobody is listening now
    val w = ForeignWorker.supervised(ForeignWorker.connect("127.0.0.1", port))
    given okay.Handler[ForeignEval] = w.handler
    val topic = MemoryStore().topic("orders")
    def run = Dialogue.workflow[ForeignCall, String, String, Pure](topic, "order-down", "order/1")(order("cake"))
      .runWorkflowIn(q => ForeignActivity.oracle(q))
    val e = intercept[ForeignActivity.Unreachable](run.runWith)
    assertEquals(e.address, "price")
    assertEquals(e.last.kind, "WorkerUnavailable")
    val (_, back) = listen(port, secret)
    try assertEquals(run.runWith, Right("total 7.5"), "nothing was journalled, so the step was done by the next run")
    finally
      w.close()
      back.destroy()
  }
