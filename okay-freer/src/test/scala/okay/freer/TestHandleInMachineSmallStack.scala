package okay.freer

import okay.{Effect}

import okay.freer.Row.*

/**
 * A HANDLER BETWEEN TWO RESETS (handle-frames, specs/handle-frames.md): a
 * `reset` whose body runs `State.handle` around another `reset`, and so on,
 * a hundred thousand deep, on a 128 KB stack — every level a handler
 * loop called from the machine's continuation and a machine forced from
 * the handler's loop, unless the one machine runs both.
 */
class TestHandleInMachineSmallStack extends munit.FunSuite:
  import okay.freer.cps.given_Classic_Free

  type R = Shift % Int + Pure

  def lvl(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else reset[Int, Pure](
      State.handle[Int](1)[Int, R](
        !.tailcall(lvl(n - 1)).at[State % Int + R]
          .flatMap(x => State.modify[Int](_ + x).at[State % Int + R])
          .flatMap(s => shift0[Int, Int, State % Int + Pure](k => k(s)).at[State % Int + R])
      ).map(_._2))

  test("three levels answer what the program says") {
    // lvl(1): state 1, + 0 -> 1, k(1) = 1; lvl(2): 1 + 1 = 2; lvl(3): 3
    assertEquals(!.run(lvl(3)), 3)
  }

  test("100 000 levels of reset / State.handle / reset on 128 KB") {
    SmallStack(128)(!.run(lvl(100000))) match
      case Right(v) => assertEquals(v, 100000)
      // the diagnosis: the frames one level of the nesting repeats
      case Left(e) => fail(e.getStackTrace.take(60).map(f => s"${f.getClassName}.${f.getMethodName}").distinct.mkString("frames:\n  ", "\n  ", ""), e)
  }

  /** no reset at all: a handler's loop forcing a Delay whose code runs another handler */
  def plain(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else State.handle[Int](1)[Int, Pure](
      !.tailcall(plain(n - 1)).at[State % Int + Pure].flatMap(x => State.modify[Int](_ + x))).map(_._2)

  test("100 000 nested State.handle, no reset, on 128 KB") {
    assertEquals(!.run(plain(3)), 3)
    SmallStack(128)(!.run(plain(100000))) match
      case Right(v) => assertEquals(v, 100000)
      case Left(e) => fail(e.getStackTrace.take(60).map(f => s"${f.getClassName}.${f.getMethodName}").distinct.mkString("frames:\n  ", "\n  ", ""), e)
  }

  /** the control form: a handler per level, its clause resuming (and once more on the way) */
  enum Tick[+A] derives Effect:
    case Now extends Tick[Int]

  def controlled(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Classic[Free].handle[Tick, Pure](
      !.tailcall(controlled(n - 1)).at[Tick + Pure].flatMap(x => effect[Tick, Int](Tick.Now).map(_ + x)))(pure(_))(
      [X] => (e: Tick[X]) => e match
        case Tick.Now => Cont.shift[X, Int ! Pure, Int ! Pure](k => k(1)))

  test("100 000 nested Effects.handle on 128 KB") {
    assertEquals(!.run(controlled(3)), 3)
    SmallStack(128)(!.run(controlled(100000))) match
      case Right(v) => assertEquals(v, 100000)
      case Left(e) => fail(e.getStackTrace.take(60).map(f => s"${f.getClassName}.${f.getMethodName}").distinct.mkString("frames:\n  ", "\n  ", ""), e)
  }
