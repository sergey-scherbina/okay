package okay.freer


/**
 * THE STRICT `k` BY RE-EXECUTION (cont-js-depth stage 4, specs/cont-js-depth.md): a body that uses its `k`'s answer
 * nests the host stack; at the end of the room the run unwinds to its driver and runs the bodies on the way again,
 * their `k`'s answers remembered. Run here with the mechanism ON on every platform (it is the default on Scala.js
 * only) and with small rooms, so most of these cross many suspensions.
 */
class TestContReplay extends munit.FunSuite:

  /** `body` with re-execution on and `room` strict calls deep, as it was after */
  def replaying[A](room: Int)(body: => A): A =
    val (on0, room0) = (ContReplay.on, ContReplay.room)
    ContReplay.on = true
    ContReplay.room = room
    try body finally { ContReplay.on = on0; ContReplay.room = room0 }

  def without[A](body: => A): A =
    val on0 = ContReplay.on
    ContReplay.on = false
    try body finally ContReplay.on = on0

  /** n strict bodies, each using its `k`'s answer: `k(1) + 1`, nested n deep; `prefix` counts the bodies run */
  def nest(n: Int, prefix: Array[Int]): Cps[Int, Int, Int] =
    if n == 0 then Cps.Pure(0)
    else Cps.shiftLeaf[Int, Int, Int](k => { prefix(0) += 1; k(1) + 1 }).flatMap(x => nest(n - 1, prefix).map(_ + x))

  /** Scala Native unwinds a throw at ~55 us a frame (measured: 4 suspensions through 500 levels, 110 ms), so a
   * suspension costs there what its depth does — the reason re-execution is not Native's default (StackSwitch is);
   * the mechanism is checked there at a depth its unwinder takes in seconds */
  val deep: Int = if System.getProperty("java.vm.name") == "Scala Native" then 50000 else 1000000

  test("nested strict bodies using k's answer, a million deep (JVM, Scala.js)") {
    val prefix = Array(0)
    assertEquals(replaying(64)(Cps.reset(nest(deep, prefix))), 2 * deep)
    // every body at least once, and a re-run only where a suspension crossed it: at most once more each
    assert(prefix(0) >= deep && prefix(0) <= 2 * deep, s"bodies run ${prefix(0)}")
  }

  test("no suspension, no re-run: the room never reached") {
    val prefix = Array(0)
    // Scala.js's stack does not hold 500 such levels: that is what the room is for
    assertEquals(replaying(100)(Cps.reset(nest(50, prefix))), 100)
    assertEquals(prefix(0), 50)
  }

  test("re-runs counted: a room of 10, a hundred deep") {
    val prefix = Array(0)
    assertEquals(replaying(10)(Cps.reset(nest(100, prefix))), 200)
    assert(prefix(0) > 100 && prefix(0) <= 200, s"bodies run ${prefix(0)}")
  }

  /** bodies calling `k` twice: every path, the answers as without suspension */
  def twice(n: Int): Cps[Int, Int, Int] =
    if n == 0 then Cps.Pure(1)
    else Cps.shiftLeaf[Int, Int, Int](k => k(1) * 3 + k(2)).flatMap(x => twice(n - 1).map(_ + x))

  test("a body calling k twice, an earlier answer used after a later suspension: as without") {
    val expected = without(Cps.reset(twice(12)))
    assertEquals(replaying(3)(Cps.reset(twice(12))), expected)
  }

  test("a stored k called from a lambda of the run never suspends: the answer as without") {
    def stored(n: Int): Cps[Int, Int, Int] =
      var saved: (Int => Int) | Null = null
      if n == 0 then Cps.Pure(0)
      else Cps.shiftLeaf[Int, Int, Int](k => { saved = k; k(1) + 1 })
        .flatMap(x => stored(n - 1).map(y => if x == 1 && n % 7 == 0 then y + saved.nn(0) % 3 else y + x))
    val expected = without(Cps.reset(stored(28)))
    assertEquals(replaying(4)(Cps.reset(stored(28))), expected)
  }

  test("a throw from the deepest body after suspensions: the same exception") {
    final class Boom extends RuntimeException("boom")
    val boom = Boom()
    def deep(n: Int): Cps[Int, Int, Int] =
      if n == 0 then Cps.shiftLeaf[Int, Int, Int](_ => throw boom)
      else Cps.shiftLeaf[Int, Int, Int](k => k(1) + 1).flatMap(x => deep(n - 1).map(_ + x))
    val e = intercept[Boom](replaying(5)(Cps.reset(deep(100))))
    assert(e eq boom)
  }
