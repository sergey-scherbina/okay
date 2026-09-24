package okay.py

import okay.{Choose, effect, runChoice, given}
import scala.concurrent.duration.*

object Reliable:
  val conf = Foreign.module("rel", """
    import os, time
    import okay

    def pairs():
        return okay.perform("choose", [1, 2]).then(lambda x:
               okay.perform("choose", [10, 20]).then(lambda y:
               okay.done(x + y)))

    def slow():
        time.sleep(30)
        return 1

    def fast(x):
        return x * 2

    # NOT a pure function of its answers: its second operation is named
    # after the process, so a replay on a fresh process meets another name
    def drifty():
        return okay.perform("choose", [1, 2]).then(lambda x:
               okay.perform("step%d" % os.getpid(), x).then(lambda y: okay.done(y)))

    class Counter:
        def __init__(self): self.n = 0
        def add(self, x):
            self.n += x
            return self.n

    def counter():
        return Counter()

    from okay import okay_call
    def quote(sku):
        return okay_call("price_of", sku)
  """)

/** stage 6 (wire-read-deadline): a deadline, a worker that comes back, and
 * programs as data that survive a real death by replay */
class TestSupervised extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = 3.minutes

  private def fresh()(using WireDeadline): ForeignWorker = ForeignWorker.start(TestPy.python.get, modules = Seq(Reliable.conf))

  /** kill the worker's PROCESS from inside a program: the engine finds out
   * on its next read, exactly as it would after a crash */
  private def crash(using okay.Handler[ForeignEval]): Unit =
    val _ = Foreign.fn[Long]("os:_exit")(0).runWith

  test("a call past its deadline answers timeout as DATA, and the worker is dead after it") {
    given WireDeadline = WireDeadline.after(500.millis)
    val w = fresh()
    given okay.Handler[ForeignEval] = w.handler
    val t0 = System.nanoTime
    val late = Foreign.fn[Long]("rel:slow")().runWith
    assert((System.nanoTime - t0) < 5.seconds.toNanos, "the deadline, not the sleep, decided")
    assert(late.left.exists(_.kind == "timeout"), late.toString)
    assert(!w.alive)
    intercept[IllegalStateException](Foreign.fn[Long]("rel:fast")(2L).runWith): Unit
  }

  test("supervised: after a timeout the next call runs on a fresh worker") {
    given WireDeadline = WireDeadline.after(500.millis)
    val w = ForeignWorker.supervised(fresh())
    given okay.Handler[ForeignEval] = w.handler
    assert(Foreign.fn[Long]("rel:slow")().runWith.left.exists(_.kind == "timeout"))
    assertEquals(Foreign.fn[Long]("rel:fast")(21L).runWith, Right(42L))
    assertEquals(w.restarts, 1)
    w.close()
  }

  test("MULTI-SHOT across a CRASH: the worker dies between two choices, and every branch still comes back") {
    val w = ForeignWorker.supervised(fresh())
    given okay.Handler[ForeignEval] = w.handler
    var crashed = false
    val choose = Foreign.callback[Vector[Long], Long]("choose") { xs =>
      if !crashed && xs == Vector(10L, 20L) then { crashed = true; crash }
      effect[Choose, Long](Choose(xs))
    }
    val pairs = Foreign.program[Long]("rel:pairs").calling(Foreign.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
    assert(crashed)
    assertEquals(w.restarts, 1)
    pairs.forget.runWith
    w.close()
  }

  test("a far side that is not a pure function of its answers is caught on replay, not answered wrongly") {
    val w = ForeignWorker.supervised(fresh())
    given okay.Handler[ForeignEval] = w.handler
    var crashed = false
    val choose = Foreign.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))
    val anyStep = (name: String) => Foreign.callback[Long, Long](name) { x =>
      if !crashed then { crashed = true; crash }
      okay.pure[Choose, Long](x)
    }
    val drifty = Foreign.program[Long]("rel:drifty")
    // the operation's name carries the FIRST process's pid; offer it
    val pid = Foreign.fn[Long]("os:getpid")().runWith.toOption.get
    val run = drifty.calling(Foreign.callbacks(choose, anyStep(s"step$pid")))()
    val got = runChoice(run.program).runWith.toList
    assert(got.exists(_.left.exists(_.kind == "ReplayDrift")), got.toString)
    w.close()
  }

  test("a direct-style call caught mid-ask answers WorkerDied: its far-side frame is gone") {
    val w = ForeignWorker.supervised(fresh())
    given okay.Handler[ForeignEval] = w.handler
    val priceOf = Foreign.callback[String, Double]("price_of") { _ => crash; okay.Free.pure(4.0) }
    val got = Foreign.fn[Double]("rel:quote").calling(Foreign.callbacks(priceOf))("tea").runWith
    assert(got.left.exists(_.kind == "WorkerDied"), got.toString)
    assertEquals(Foreign.fn[Long]("rel:fast")(5L).runWith, Right(10L))
    w.close()
  }

  test("a held object from before a restart is refused by name, never re-pointed") {
    val w = ForeignWorker.supervised(fresh())
    given okay.Handler[ForeignEval] = w.handler
    val c = Foreign.hold("rel:counter")().runWith.toOption.get
    assertEquals(c.call[Long]("add")(3L).runWith, Right(3L))
    crash
    val fresh2 = Foreign.hold("rel:counter")().runWith.toOption.get
    val old = c.call[Long]("add")(3L).runWith
    assert(old.left.exists(c => c.kind == "LookupError" && c.message.contains("belongs to a worker that is gone")), old.toString)
    assertEquals(fresh2.call[Long]("add")(1L).runWith, Right(1L))
    w.close()
  }
