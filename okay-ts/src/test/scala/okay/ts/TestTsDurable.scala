package okay.ts

import scala.concurrent.Future
import scala.scalajs.js
import okay.{!, Async}
import okay.codec.Schema

object TestTsDurable:
  final case class Receipt(reserved: String, charged: String) derives Schema

  /** a checkout as TypeScript writes it: reserve, then charge what was reserved */
  val flows: js.Dynamic = js.Dynamic.global.eval("""(function () {
    const done = (value) => ({ tag: "done", value });
    const perform = (name, ...args) => ({ tag: "perform", name, args, k: (x) => done(x) });
    const then = (p, f) => p.tag === "done" ? f(p.value)
      : { tag: "perform", name: p.name, args: p.args, k: (x) => then(p.k(x), f) };
    return {
      checkout: (sku) => then(perform("reserve", sku), (r) =>
        then(perform("charge", r), (c) => done({ reserved: r, charged: c }))),
    };
  })()""")

/** typescript-types T10: durable flows, on the in-memory journal (the IndexedDB one runs in Chrome, scripts/ts-durable-browser-check.sh) */
class TestTsDurable extends munit.FunSuite {
  import TestTsDurable.*
  given scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  /** callbacks that count their calls; `failCharge` makes the charge die once, as a reload would */
  final class World(var failCharge: Boolean = false):
    var reserves = 0
    var charges = 0
    val cbs: Ts.Callbacks[Async] = Ts.callbacks[Async](
      Ts.callback[String, String]("reserve")(sku => okay.async { reserves += 1; s"R-$sku" }),
      Ts.callback[String, String]("charge")(r => okay.async {
        if failCharge then { failCharge = false; throw IllegalStateException("the page was reloaded") }
        charges += 1
        s"C-$r"
      }),
    )

  private def checkout(w: World, j: Journal, sku: String = "tea"): Either[Ts.Failure, Receipt] ! Async =
    Ts.durable[Receipt]("order-1", flows.checkout(sku), w.cbs, j)

  private def run[A](p: A ! Async): Future[A] = Async.runAsync(p)

  test("a finished flow replays from the journal: the same answer, and no callback runs again") {
    val j = Journal.memory()
    val w = World()
    for
      first <- run(checkout(w, j))
      again <- run(checkout(w, j))
    yield
      assertEquals(first, Right(Receipt("R-tea", "C-R-tea")))
      assertEquals(again, first)
      assertEquals((w.reserves, w.charges), (1, 1))
  }

  test("a flow interrupted after its first step resumes there: the step before is not repeated") {
    val j = Journal.memory()
    val w = World(failCharge = true)
    for
      died <- run(checkout(w, j)).transform(t => scala.util.Success(t.isFailure))
      resumed <- run(checkout(w, j))
    yield
      assert(died, "the first load should have died at the charge")
      assertEquals(resumed, Right(Receipt("R-tea", "C-R-tea")))
      assertEquals((w.reserves, w.charges), (1, 1))
  }

  test("a recorded step asked differently is refused as drift, never handed the old answer") {
    val j = Journal.memory()
    val w = World()
    for
      _ <- run(checkout(w, j, "tea"))
      drifted <- run(checkout(w, j, "coffee"))
    yield
      assertEquals(drifted.left.map(_.kind), Left("Drift"))
      assert(drifted.left.exists(_.message.contains("step 0 of 'order-1'")), drifted)
      assertEquals(w.reserves, 1)
  }

  test("clear forgets a flow: the next run calls every step again") {
    val j = Journal.memory()
    val w = World()
    for
      _ <- run(checkout(w, j))
      _ <- run(j.clear("order-1"))
      _ <- run(checkout(w, j))
    yield assertEquals((w.reserves, w.charges), (2, 2))
  }
}
