package okay.freer


import okay.std.*
import okay.std.given
import okay.{TailRecM, ==>}
import okay.given

/**
 * `p.foldMap(nt)`: a program folded into any okay `Monad`
 * (specs/effects-foldmap.md) — into a deferring carrier at depth, and
 * into an eager one (`Option`) with its short circuit.
 */
class TestFoldMap extends munit.FunSuite:

  enum Op[+A]:
    case Lookup(key: String) extends Op[Int]
    case Log(msg: String) extends Op[Unit]

  val prog: Int ! Op = for
    a <- effect(Op.Lookup("a"))
    _ <- effect(Op.Log(s"a=$a"))
    b <- effect(Op.Lookup("b"))
  yield a + b

  /** into Option: a missing key stops the fold */
  def table(m: Map[String, Int]): Op ==> Option = [X] => (e: Op[X]) => e match
    case Op.Lookup(k) => m.get(k)
    case Op.Log(_) => Some(())

  test("into Option: answers in program order, and None short-circuits") {
    assertEquals(prog.foldMap(table(Map("a" -> 1, "b" -> 2))), Some(3))
    assertEquals(prog.foldMap(table(Map("b" -> 2))), None)
  }

  test("into another program: operations translated, run by that program's handler") {
    val log = collection.mutable.Buffer.empty[String]
    val toReader: Op ==> ([X] =>> X ! Reader % Int) = [X] => (e: Op[X]) => e match
      case Op.Lookup(k) => Reader.ask[Int].map(_ + k.length)
      case Op.Log(m) => pure { log += m; () }
    val out = Reader.run[Int, Int, Pure](10)(prog.foldMap[[X] =>> X ! Reader % Int](toReader))
    assertEquals(!.run(out), 22)
    assertEquals(log.toList, List("a=11"))
  }

  test("a deferring G folds 100 000 operations without growing the stack") {
    val n = 100000
    val long: Int ! Op = (1 to n).foldLeft(pure[Op, Int](0))((p, _) =>
      p.flatMap(s => effect(Op.Lookup("x")).map(_ + s)))
    val toReader: Op ==> ([X] =>> X ! Reader % Int) = [X] => (e: Op[X]) => e match
      case Op.Lookup(_) => Reader.ask[Int]
      case Op.Log(_) => pure(())
    assertEquals(!.run(Reader.run[Int, Int, Pure](1)(long.foldMap[[X] =>> X ! Reader % Int](toReader))), n)
  }

  test("an eager G on a 128 KB thread: a million operations through Option, both shapes") {
    // foldMap is Option's own loop (TailRecM), so the stack it is given
    // does not matter; the first cut, through foldCont, overflowed this
    // at 1 000 (specs/eager-carrier-depth.md)
    val n = 1000000
    val left: Int ! Op = (1 to n).foldLeft(pure[Op, Int](0))((p, _) =>
      p.flatMap(s => effect(Op.Lookup("x")).map(_ + s)))
    def nonTail(i: Int): Int ! Op =
      if i == 0 then pure(0) else effect(Op.Lookup("x")).flatMap(x => nonTail(i - 1).map(_ + x))
    assertEquals(SmallStack.run(128)(left.foldMap(table(Map("x" -> 1)))), Some(n))
    assertEquals(SmallStack.run(128)(nonTail(n).foldMap(table(Map("x" -> 1)))), Some(n))
  }
