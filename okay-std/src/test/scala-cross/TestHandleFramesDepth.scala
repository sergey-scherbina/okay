package okay.freer


import okay.std.*
import okay.std.given
import okay.{Effect}

import okay.freer.Row.*

/** handle-frames on every platform: a handler per level, a hundred thousand levels, on the engine's own stack */
class TestHandleFramesDepth extends munit.FunSuite:
  import okay.freer.cps.given_Classic_Free

  val n = 100000

  def plain(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else State.handle[Int](1)[Int, Pure](
      !.tailcall(plain(n - 1)).at[State % Int + Pure].flatMap(x => State.modify[Int](_ + x))).map(_._2)

  test("nested State.handle") {
    assertEquals(!.run(plain(n)), n)
  }

  type R = Shift % Int + Pure

  def lvl(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else reset[Int, Pure](
      State.handle[Int](1)[Int, R](
        !.tailcall(lvl(n - 1)).at[State % Int + R]
          .flatMap(x => State.modify[Int](_ + x).at[State % Int + R])
          .flatMap(s => shift0[Int, Int, State % Int + Pure](k => k(s)).at[State % Int + R])
      ).map(_._2))

  test("reset / State.handle / reset") {
    assertEquals(!.run(lvl(n)), n)
  }

  enum Tick[+A] derives Effect:
    case Now extends Tick[Int]

  def controlled(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Classic[Free].handle[Tick, Pure](
      !.tailcall(controlled(n - 1)).at[Tick + Pure].flatMap(x => effect[Tick, Int](Tick.Now).map(_ + x)))(pure(_))(
      [X] => (e: Tick[X]) => e match
        case Tick.Now => Cps.shift[X, Int ! Pure, Int ! Pure](k => k(1)))

  test("nested Effects.handle") {
    assertEquals(!.run(controlled(n)), n)
  }

  // ---- handle-frames-forms: the answer form (relay) and the into form (translate)

  def relayed(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Classic.relay[Int, Int, Tick, Pure](
      !.tailcall(relayed(n - 1)).at[Tick + Pure].flatMap(x => effect[Tick, Int](Tick.Now).map(_ + x)))(pure(_))(
      [X, Y] => (e: Tick[X]) => e match
        case Tick.Now => Cps.Pure[X, Y](1))

  test("nested Effects.relay") {
    assertEquals(!.run(relayed(n)), n)
  }

  def translated(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else Classic.translate[Int, Tick, Pure](
      !.tailcall(translated(n - 1)).at[Tick + Pure].flatMap(x => effect[Tick, Int](Tick.Now).map(_ + x)))(
      [X] => (e: Tick[X]) => e match
        case Tick.Now => pure[Pure, X](1))

  test("nested Effects.translate") {
    assertEquals(!.run(translated(n)), n)
  }
