package okay.npm

import scala.scalajs.js

/** typescript-types T9: the package's exports, called as JavaScript calls them (Node runs the tests) */
class TestOkayNpm extends munit.FunSuite {
  given scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  private def str(v: js.Any): String = String.valueOf(js.JSON.stringify(v))

  /** a program the way a TypeScript caller writes it, with the package's own helpers */
  private val program: js.Any =
    Okay.andThen(js.Dynamic.global.Object(Okay.performing("price", "tea")), (price: js.Any) =>
      Okay.andThen(js.Dynamic.global.Object(Okay.performing("stock", "tea")), (stock: js.Any) =>
        Okay.done(js.Dynamic.literal(price = price, stock = stock))))

  private val priceOf: js.Function1[String, Double] = sku => if sku == "tea" then 4.5 else 0.0
  private val stockOf: js.Function1[String, js.Promise[Int]] = _ => js.Promise.resolve[Int](12)
  private val broken: js.Function1[String, Double] = _ => throw js.JavaScriptException(js.RangeError("no price"))
  private val one: js.Function1[String, Int] = _ => 1

  test("run walks a program; a callback answers a value or a Promise of one") {
    val callbacks = js.Dictionary[js.Function](
      "price" -> priceOf,
      "stock" -> stockOf,
    )
    Okay.run(program, callbacks).toFuture.map(v => assertEquals(str(v), """{"price":4.5,"stock":12}"""))
  }

  test("a callback that throws rejects the run with its name and message") {
    val callbacks = js.Dictionary[js.Function](
      "price" -> broken,
      "stock" -> one,
    )
    Okay.run(program, callbacks).toFuture.transform {
      case scala.util.Failure(js.JavaScriptException(e: js.Error)) =>
        assertEquals(e.message, "RangeError: no price")
        scala.util.Success(())
      case other => scala.util.Failure(AssertionError(s"expected a rejection, got $other"))
    }
  }

  test("durable: a journalled flow run twice calls each callback once, and answers the same") {
    var prices = 0
    val counted: js.Function1[String, Double] = _ => { prices += 1; 4.5 }
    val callbacks = js.Dictionary[js.Function]("price" -> counted, "stock" -> stockOf)
    val journal = Okay.memoryJournal()
    for
      first <- Okay.durable("quote", program, callbacks, journal).toFuture
      again <- Okay.durable("quote", program, callbacks, journal).toFuture
      entries <- js.Promise.resolve[js.Any](journal.load("quote")).toFuture
    yield
      assertEquals(str(first), """{"price":4.5,"stock":12}""")
      assertEquals(str(again), str(first))
      assertEquals(prices, 1)
      assert(str(entries).contains("price"), str(entries))
  }

  test("CRDT replicas are JSON states; merge joins two replicas' increments") {
    val g = Okay.gcounter
    val a = g.inc(g.inc(g.empty(), "a"), "a")
    val b = g.inc(g.empty(), "b", 3)
    assertEquals(g.value(g.merge(a, b)): Any, 5.0)
    assertEquals(str(g.merge(a, b)), str(g.merge(b, a)))
  }

  test("an observed-remove set: a concurrent add survives a remove it did not see") {
    val s = Okay.orset
    val base = s.add(s.empty(), "milk")
    val left = s.remove(base, "milk")
    val right = s.add(base, "milk")
    val both = s.merge(left, right)
    assertEquals(s.has(both, "milk"): Any, true)
    assertEquals(str(s.values(s.merge(base, s.add(s.empty(), "eggs")))), """["eggs","milk"]""")
  }

  test("a wrong state is refused with a TypeError naming the operation") {
    val e = intercept[js.JavaScriptException](Okay.gcounter.value(js.Dynamic.literal(nope = 1)))
    e.exception match
      case err: js.Error =>
        assertEquals(err.name, "TypeError")
        assert(err.message.startsWith("gcounter.value: "), err.message)
      case other => fail(s"not an Error: $other")
  }

  test("a channel is an AsyncIterable: what is offered, in order, until close") {
    val c = Okay.channel()
    assertEquals(c.offer("a"): Any, true)
    assertEquals(c.offer(js.Dynamic.literal(n = 2)): Any, true)
    val _ = c.close()
    val it = js.Dynamic.global.Reflect.get(c, js.Symbol.asyncIterator).call(c)
    def next(): scala.concurrent.Future[js.Dynamic] =
      js.Promise.resolve[js.Any](it.next()).toFuture.map(js.Dynamic.global.Object(_))
    for
      one <- next()
      two <- next()
      end <- next()
    yield
      assertEquals(str(one.value), "\"a\"")
      assertEquals(str(two.value), """{"n":2}""")
      assertEquals(end.done: Any, true)
  }

  test("the declarations: the CRDT states' types from their Schemas, and every export") {
    val d = Okay.declarations
    assert(d.contains("export type GCounter = "), d)
    for name <- Vector("done", "perform", "performing", "then", "run", "channel", "durable",
                       "memoryJournal", "indexedDbJournal") do
      assert(d.contains(s"export declare function $name"), name)
    for name <- Vector("gcounter", "pncounter", "orset", "declarations") do
      assert(d.contains(s"export declare const $name"), name)
  }
}
