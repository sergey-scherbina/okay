package okay.freer

import okay.*
import okay.given

/**
 * handler-single-pass stage 1 (specs/handler-single-pass.md): a `Handler.Stepped` built-in, walked through its
 * step alone (`init`, `step`, `ret`, `halted`), answers what its own `run` answers. That is the contract the
 * one-pass walk over a stack of handlers builds on.
 */
class TestStepped extends munit.FunSuite:

  /** a walk that knows the handler by its step only: the engine every built-in's `run` is a step on */
  private def viaStep[E[+_], S, O[_], A](h: Handler.Stepped[E, S, O], p: A ! E): O[A] =
    HandleFrames.stateRunOr[E, S, A, O[A], Pure](h.takes, (s, a) => h.ret[A, Pure](s, a))(
      (s, op) => h.step(s, op) match
        case Handler.Halt(s2) => HandleFrames.Stop(h.halted[A, Pure](s2))
        case done: (S, Any) @unchecked => done)(h.init, p).run

  /** the handler as the Stepped it is at run time; a built-in that is not one fails here by name */
  private def stepped[E[+_], O[_]](h: Handler[E, O]): Handler.Stepped[E, Any, O] = h match
    case s: Handler.Stepped[E, Any, O] @unchecked => s
    case other => fail(s"$other is not Handler.Stepped")

  test("State(s): the step answers what State(s) answers") {
    val p = State.get[Int].flatMap(n => State.set(n + 5)).flatMap(_ => State.get[Int]).map(_ * 2)
    assertEquals(viaStep(stepped(State(10)), p), p.handle(State(10)).run)
  }

  test("Reader(r)") {
    val p = Reader.ask[Int].flatMap(a => Reader.ask[Int].map(b => a + b))
    assertEquals(viaStep(stepped(Reader(21)), p), p.handle(Reader(21)).run)
  }

  test("Writer.log") {
    val p = Writer.tell("a").flatMap(_ => Writer.tell("b")).map(_ => 3)
    assertEquals(viaStep(stepped(Writer.log[String]), p), p.handle(Writer.log[String]).run)
  }

  test("Fresh.counter and Supply.from") {
    val f = Fresh.next.flatMap(a => Fresh.next.map(b => (a, b)))
    assertEquals(viaStep(stepped(Fresh.counter), f), f.handle(Fresh.counter).run)
    val s = Supply.next[Int].flatMap(a => Supply.next[Int].map(b => a + b))
    assertEquals(viaStep(stepped(Supply.from(1)(_ * 3)), s), s.handle(Supply.from(1)(_ * 3)).run)
  }

  test("Once.memo: a once's answer shared through the cells the step threads") {
    var runs = 0
    val p = Once.once[Int, Pure](pure { runs += 1; 7 })
    val both = p.flatMap(h => p.map(_ + h))
    val viaSteps = viaStep(stepped(Once.memo), both)
    val runsViaSteps = runs
    runs = 0
    assertEquals(viaSteps, both.handle(Once.memo).run)
    assertEquals(runsViaSteps, runs)
  }

  test("Chronicle.verdict: dictates recorded, a halt stops with the record (`halted`)") {
    val clean = pure[Chronicle % String, Int](1)
    val warned = Chronicle.dictate("w1").flatMap(_ => Chronicle.dictate("w2")).map(_ => 2)
    val failed = Chronicle.dictate("w").flatMap(_ => Chronicle.halt[String, Int]).map(_ + 100)
    for p <- List(clean, warned, failed) do
      assertEquals(viaStep(stepped(Chronicle.verdict[String]), p), p.handle(Chronicle.verdict[String]).run)
  }

  test("the author's forms: Handler.state and Handler.answer are Stepped") {
    val st = Handler.stateOf[State % Int, Int](3)([X] => (s: Int, e: State[Int, X]) => e match
      case State.Get() => (s, s)
      case State.Update(f) => { val (b, n) = f(s); (n, b) })
    val p = State.get[Int].flatMap(n => State.set(n * 4)).flatMap(_ => State.get[Int])
    assertEquals(viaStep(st, p), p.handle(st).run)
    val an = Handler.answerOf[Reader % Int]([X] => (e: Reader[Int, X]) => e match
      case Reader.Ask() => 5
      case Reader.Asks(g) => g(5))
    val q = Reader.ask[Int].map(_ + 1)
    assertEquals(viaStep(an, q), q.handle(an).run)
  }
