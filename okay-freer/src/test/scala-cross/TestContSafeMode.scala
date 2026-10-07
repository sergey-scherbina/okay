package okay.freer

import okay.*
import okay.given

import scala.collection.mutable.ArrayBuffer

/** bodies compiled in a SAFE scope: transformed (k is data), or they would not compile */
object SafeBodies:
  import okay.freer.Cont.safe.given

  /** side effects before and after `k`; `k`'s answer used */
  def nest(n: Int, log: Array[Int]): Cont[Int, Int, Int] =
    if n == 0 then Cont.Pure(0)
    else Cont.shift[Int, Int, Int](k => { log(0) += 1; val r = k(1); log(1) += 1; r + 1 }).flatMap(x => nest(n - 1, log).map(_ + x))

  def ordered(n: Int, log: ArrayBuffer[String]): Cont[Int, Int, Int] =
    if n == 0 then Cont.Pure(0)
    else Cont.shift[Int, Int, Int](k => { log += s"before $n"; val r = k(1); log += s"after $n"; r + 1 }).flatMap(x => ordered(n - 1, log).map(_ + x))

  /** a program answer, `k` passed on: the lazy `k` */
  def passedOn: Cont[Int, Int ! Pure, Int ! Pure] = Cont.shift[Int, Int ! Pure, Int ! Pure](k => pure[Pure, Int](20).flatMap(k))

/** the same bodies as strict leaves, never transformed */
object StrictBodies:
  def ordered(n: Int, log: ArrayBuffer[String]): Cont[Int, Int, Int] =
    if n == 0 then Cont.Pure(0)
    else Cont.shiftLeaf[Int, Int, Int](k => { log += s"before $n"; val r = k(1); log += s"after $n"; r + 1 }).flatMap(x => ordered(n - 1, log).map(_ + x))

  def twice(f: Int => Int): Int = f(1) + 1
  def counted(n: Int, count: Array[Int]): Cont[Int, Int, Int] =
    if n == 0 then Cont.Pure(0)
    else Cont.shift[Int, Int, Int](k => { count(0) += 1; twice(k) }).flatMap(x => counted(n - 1, count).map(_ + x))

/** opaque bodies compiled in a NO-REPLAY scope: kept, never re-executed */
object OnceBodies:
  import okay.freer.Cont.noReplay.given
  def counted(n: Int, count: Array[Int]): Cont[Int, Int, Int] =
    if n == 0 then Cont.Pure(0)
    else Cont.shift[Int, Int, Int](k => { count(0) += 1; StrictBodies.twice(k) }).flatMap(x => counted(n - 1, count).map(_ + x))

/**
 * THE SAFE MODE (cont-safe-mode, specs/cont-js-depth.md stage 5): full trampolining with no re-execution on every
 * platform is a compile-time property — a body in `Cont.safe` is transformed or refused; `Cont.noReplay` keeps an
 * opaque body and never re-executes it; the run-time mode (`Cont.setMode`) decides for the strict leaves left.
 */
class TestContSafeMode extends munit.FunSuite:

  /** `body` under re-execution forced on with a small room — the mode a safe body must be immune to */
  def replayingSmall[A](body: => A): A =
    val (on0, room0) = (ContReplay.on, ContReplay.room)
    ContReplay.on = true
    ContReplay.room = 4
    try body finally { ContReplay.on = on0; ContReplay.room = room0 }

  def inMode[A](m: Cont.Mode)(body: => A): A =
    val m0 = Cont.mode
    Cont.setMode(m)
    try body finally Cont.setMode(m0)

  test("safe scope: effects before and after k, a million deep, each exactly once — under re-execution forced on") {
    val log = Array(0, 0)
    assertEquals(replayingSmall(Cont.reset(SafeBodies.nest(1000000, log))), 2000000)
    assertEquals(log.toList, List(1000000, 1000000))
  }

  test("safe scope: the effects in the strict body's order") {
    val safe = ArrayBuffer.empty[String]
    val strict = ArrayBuffer.empty[String]
    assertEquals(Cont.reset(SafeBodies.ordered(5, safe)), inMode(Cont.Mode.Safe)(Cont.reset(StrictBodies.ordered(5, strict))))
    assertEquals(safe.toList, strict.toList)
  }

  test("safe scope: k passed to a function the macro cannot see into is a compile error naming the fix") {
    val errors = compileErrors("""
      import okay.freer.Cont.safe.given
      def twice(f: Int => Int): Int = f(1) + 1
      val c = Cont.shift[Int, Int, Int](k => twice(k))
    """).replaceAll("\\s+", " ")
    assert(errors.contains("Cont safe mode"), errors)
    assert(errors.contains("noReplay"), errors)
  }

  test("safe scope: a program answer with k passed on compiles, its k lazy") {
    assertEquals(!.run(Cont.reset(SafeBodies.passedOn.map(pure[Pure, Int]))), 20)
  }

  test("noReplay scope: an opaque body with an effect, under re-execution forced on: run exactly once a level") {
    val once = Array(0)
    val replayed = Array(0)
    val a = replayingSmall(Cont.reset(OnceBodies.counted(40, once)))
    val b = replayingSmall(Cont.reset(StrictBodies.counted(40, replayed)))
    assertEquals(a, b)
    assertEquals(once(0), 40)
    // the control: the same body outside the scope IS re-executed there
    assert(replayed(0) > 40, s"run ${replayed(0)}")
  }

  test("run time: Safe never re-executes, Replay does past the room, Auto is the platform's") {
    def strictNest(n: Int, prefix: Array[Int]): Cont[Int, Int, Int] =
      if n == 0 then Cont.Pure(0)
      else Cont.shiftLeaf[Int, Int, Int](k => { prefix(0) += 1; k(1) + 1 }).flatMap(x => strictNest(n - 1, prefix).map(_ + x))
    val room0 = ContReplay.room
    ContReplay.room = 10
    try
      val safe = Array(0)
      assertEquals(inMode(Cont.Mode.Safe)(Cont.reset(strictNest(50, safe))), 100)
      assertEquals(safe(0), 50)
      val replay = Array(0)
      assertEquals(inMode(Cont.Mode.Replay)(Cont.reset(strictNest(50, replay))), 100)
      assert(replay(0) > 50, s"run ${replay(0)}")
      inMode(Cont.Mode.Auto)(assertEquals(ContReplay.on, StackSwitch.replayByDefault))
    finally ContReplay.room = room0
  }
