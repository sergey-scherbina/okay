package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE FOUR PATTERNS (delim-patterns, 2026-09-17). Each test is a
 * shape that shows up in ordinary code, written with the named
 * combinator rather than a raw `shift` — and, where it is short
 * enough to be fair, next to the way it is usually written.
 */
class TestDelimPatterns extends munit.FunSuite {

  type R = Delim + okay.Pure

  // ---- 1 · leave early with an answer

  val txs = List(10, 20, 30, 40)
  val rules: List[Int => Boolean] = List(_ > 25, _ % 7 == 0)

  def firstHit: Option[Int] ! okay.Pure = Delim.delimited[Option[Int], okay.Pure]:
    direct:
      for t <- txs; r <- rules do
        if r(t) then !Delim.exit(Some(t))   // out of BOTH loops, with a value
      None

  test("exit: leaving two nested loops with an answer") {
    assertEquals(!.run(firstHit), Some(30))

    // the same answer the usual way, for comparison: the loops become
    // a fold, and the "am I done" question is now in every iteration
    val usual = txs.foldLeft(Option.empty[Int]): (acc, t) =>
      if acc.isDefined then acc
      else if rules.exists(_(t)) then Some(t) else acc
    assertEquals(usual, Some(30))
  }

  test("exit: nothing matches, so the block runs to its end") {
    val none = Delim.delimited[Option[Int], okay.Pure]:
      direct:
        for t <- List(1, 2) do
          if t > 100 then !Delim.exit(Some(t))
        None
    assertEquals(!.run(none), None)
  }

  // ---- 2 · a push producer read as a pull

  enum Tree[+A]:
    case Leaf(a: A)
    case Node(l: Tree[A], r: Tree[A])

  /** an ordinary recursive walk: it emits, and knows nothing else */
  def walk(t: Tree[Int])(using Delim.Emitting[Int]): Unit ! R = direct:
    t match
      case Tree.Leaf(a) => !Delim.emit(a)
      case Tree.Node(l, r) =>
        !walk(l)
        !walk(r)

  test("collect/emit: the producer stays a walk, the caller gets a list") {
    val t = Tree.Node(Tree.Node(Tree.Leaf(1), Tree.Leaf(2)), Tree.Leaf(3))
    assertEquals(!.run(Delim.collect[Int, okay.Pure](walk(t))), List(1, 2, 3))
    assertEquals(!.run(Delim.collect[Int, okay.Pure](walk(Tree.Leaf(7)))), List(7))
    // a producer that emits nothing is not a special case
    assertEquals(!.run(Delim.collect[Int, okay.Pure](direct(()))), List.empty[Int])
  }

  // ---- 3 · stop in the middle, carry on later

  def booking(using Delim.Asking[String, String, String, R]): String ! R = direct:
    val city = !Delim.pause("Which city?")
    val nights = !Delim.pause(s"How many nights in $city?")
    val pay = !Delim.pause(s"Pay ${nights.toInt * 90} for $city?")
    if pay == "yes" then s"Booked $city for $nights nights" else "Cancelled"

  test("pause/resumable: the rest of the dialogue is a value") {
    val start = !.run(Delim.resumable[String, String, String, okay.Pure](booking))

    def answering(as: List[String]): String => String ! okay.Pure =
      var left = as
      _ => { val a = left.head; left = left.tail; okay.pure(a) }

    assertEquals(!.run(Delim.drive(start)(answering(List("Kyiv", "3", "yes")))),
      "Booked Kyiv for 3 nights")
    // the SAME paused dialogue, answered again and differently — it is
    // a value, so it does not get consumed by being resumed
    assertEquals(!.run(Delim.drive(start)(answering(List("Lviv", "2", "no")))),
      "Cancelled")
  }

  test("pause: a program that never asks is Done already") {
    // a body that never pauses needs no evidence at all: it adapts
    val p = !.run(Delim.resumable[String, String, Int, okay.Pure](direct(41 + 1)))
    assert(p.isInstanceOf[Delim.Paused.Done[?, ?, ?, ?]], s"expected Done, got $p")
    assertEquals(!.run(Delim.drive(p)(_ => okay.pure("unused"))), 42)
  }

  // ---- 4 · do something on the way back

  test("onReturn: the rest of the block is a value you can act on") {
    val r = !.run(Delim.delimited[Int, okay.Pure]:
      direct:
        !Delim.onReturn(n => n * 10)   // runs LAST, on whatever comes back
        1 + 2)
    assertEquals(r, 30)
  }

  test("onReturn: a compensation folded into the answer from the middle") {
    // the shape: halfway through, register what to do about the
    // outcome — without the code around it changing at all
    def charge(amount: Int, ok: Boolean): String ! okay.Pure =
      Delim.delimited[String, okay.Pure]:
        direct:
          !Delim.onReturn(s => if s.startsWith("failed") then s"$s; refunded $amount" else s)
          if ok then s"charged $amount" else "failed: card declined"
    assertEquals(!.run(charge(90, true)), "charged 90")
    assertEquals(!.run(charge(90, false)), "failed: card declined; refunded 90")
  }
}
