package okay

import okay.Direct.*
import okay.Shift.Stacked.{delimited, shift}
import scala.language.implicitConversions

/**
 * docs/continuations-in-practice.md, line for line
 * (doc-snippets-pin-all): each example on the page is copied here
 * VERBATIM — its answer comment included — and then asserted, so the
 * page cannot say a thing the library does not do. TestDelimPatterns
 * and TestDelimNesting are the suites the page grew from; this one
 * holds the lines as the PAGE shows them.
 */
class TestDocExamplesContinuationsInPractice extends munit.FunSuite:

  val txs = List(10, 20, 30, 40)
  val rules: List[Int => Boolean] = List(_ > 25, _ % 7 == 0)

  type R = Shift % ? + Pure

  enum Tree[+A]:
    case Leaf(a: A)
    case Node(l: Tree[A], r: Tree[A])

  def walk(t: Tree[Int])(using Shift.Emitting[Int]): Unit ! R = direct:
    t match
      case Tree.Leaf(a) => !Shift.emit(a)
      case Tree.Node(l, r) =>
        !walk(l)
        !walk(r)

  val t = Tree.Node(Tree.Node(Tree.Leaf(1), Tree.Leaf(2)), Tree.Leaf(3))

  test("the obvious exit and the puzzle shift answer the same") {
    val obvious = Shift.delimited[Option[Int], Pure]:
      direct:
        for t <- txs; r <- rules do
          if r(t) then !Shift.exit(Some(t))              // obvious
        None
    val puzzle = Shift.delimited[Option[Int], Pure]:
      direct:
        for t <- txs; r <- rules do
          if r(t) then !Shift.shift[Unit](_ => pure(Some(t)))   // a puzzle
        None
    assertEquals(!.run(obvious), Some(30))
    assertEquals(!.run(puzzle), Some(30))
  }

  def half(using Shift.Asking[String, Int, List[Int], Shift % ? + Pure]) =
    Shift.collecting[Int, Pure]:        // nested: installs only
      direct:
        !Shift.emit(1)
        val more = !Shift.pause("more?")  // crosses the collect's delimiter
        !Shift.emit(more)
        !Shift.emit(3)

  test("a pause crosses the nested collect") {
    val r =
      !.run(Shift.drive(!.run(Shift.resumable(half)))(_ => pure(2)))  // List(1, 2, 3)
    assertEquals(r, List(1, 2, 3))
  }

  test("the stack in the type: reset { shift(k => k(5) * 2) }") {
    type P = Pure
    val r = !.run(delimited[Int, P] { s =>
      import s.given
      shift[Int, Int, P](s.p)(k => k(5).map(_ * 2))
    })   // 10
    assertEquals(r, 10)
  }

  test("exit: out of both loops, with a value") {
    val firstHit =
      Shift.delimited[Option[Int], Pure]:
        direct:
          for t <- txs; r <- rules do
            if r(t) then !Shift.exit(Some(t))   // out of BOTH loops, with a value
          None
    assertEquals(!.run(firstHit), Some(30))
  }

  test("the usual fold, for comparison") {
    val usual =
      txs.foldLeft(Option.empty[Int]): (acc, t) =>
        if acc.isDefined then acc
        else if rules.exists(_(t)) then Some(t) else acc
    assertEquals(usual, Some(30))
  }

  test("collect, and collect until") {
    assertEquals(!.run(
      Shift.collect[Int, Pure](walk(t))    // List(1, 2, 3)
    ), List(1, 2, 3))
    assertEquals(!.run(
      Shift.collectUntil[Int, Vector[Int], Vector[Int], Pure](using FoldUntil.take(2))(walk(t))   // Vector(1, 2) — the third leaf is never visited
    ), Vector(1, 2))
    assertEquals(!.run(
      Shift.collectUntil[Int, Option[Int], Option[Int], Pure](using FoldUntil.find[Int](_ > 1))(walk(t))  // Some(2)
    ), Some(2))
  }

  def booking(using Shift.Asking[String, String, String, R]): String ! R = direct:
    val city   = !Shift.pause("Which city?")
    val nights = !Shift.pause(s"How many nights in $city?")
    val pay    = !Shift.pause(s"Pay ${nights.toInt * 90} for $city?")
    if pay == "yes" then s"Booked $city for $nights nights" else "Cancelled"

  def answering(as: List[String]): String => String ! Pure =
    var left = as
    _ => { val a = left.head; left = left.tail; pure(a) }

  test("stop in the middle, carry on later") {
    val start = !.run(Shift.resumable[String, String, String, Pure](booking))
    val done =
      !.run(Shift.drive(start)(answering(List("Kyiv", "3", "yes"))))
    assertEquals(done, "Booked Kyiv for 3 nights")
  }

  test("the journal outlives the process") {
    val p0 = !.run(Shift.resumable[String, String, String, Pure](booking))
    val (p1, j1) = !.run(Shift.answer(p0, Nil)("Kyiv"))
    val (p2, j2) = !.run(Shift.answer(p1, j1)("3"))    // j2 = List("Kyiv", "3")
    assertEquals(j2, List("Kyiv", "3"))
    assertEquals(p2.asking, Some("Pay 270 for Kyiv?"))
    val back = !.run(Shift.replay[String, String, String, Pure](booking)(j2))
    assertEquals(
      back.asking    // Some("Pay 270 for Kyiv?") — the same place
    , Some("Pay 270 for Kyiv?"))
  }
