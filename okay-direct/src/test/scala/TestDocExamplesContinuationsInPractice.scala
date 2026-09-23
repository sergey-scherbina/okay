package okay

import okay.Direct.*
import okay.Delim.Stacked.{delimited, shift}
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

  type R = Delim + Pure

  enum Tree[+A]:
    case Leaf(a: A)
    case Node(l: Tree[A], r: Tree[A])

  def walk(t: Tree[Int])(using Delim.Emitting[Int]): Unit ! R = direct:
    t match
      case Tree.Leaf(a) => !Delim.emit(a)
      case Tree.Node(l, r) =>
        !walk(l)
        !walk(r)

  val t = Tree.Node(Tree.Node(Tree.Leaf(1), Tree.Leaf(2)), Tree.Leaf(3))

  test("the obvious exit and the puzzle shift answer the same") {
    val obvious = Delim.delimited[Option[Int], Pure]:
      direct:
        for t <- txs; r <- rules do
          if r(t) then !Delim.exit(Some(t))              // obvious
        None
    val puzzle = Delim.delimited[Option[Int], Pure]:
      direct:
        for t <- txs; r <- rules do
          if r(t) then !Delim.shift[Unit](_ => pure(Some(t)))   // a puzzle
        None
    assertEquals(!.run(obvious), Some(30))
    assertEquals(!.run(puzzle), Some(30))
  }

  def half(using Delim.Asking[String, Int, List[Int], Delim + Pure]) =
    Delim.collecting[Int, Pure]:        // nested: installs only
      direct:
        !Delim.emit(1)
        val more = !Delim.pause("more?")  // crosses the collect's delimiter
        !Delim.emit(more)
        !Delim.emit(3)

  test("a pause crosses the nested collect") {
    val r =
      !.run(Delim.drive(!.run(Delim.resumable(half)))(_ => pure(2)))  // List(1, 2, 3)
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
      Delim.delimited[Option[Int], Pure]:
        direct:
          for t <- txs; r <- rules do
            if r(t) then !Delim.exit(Some(t))   // out of BOTH loops, with a value
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
      Delim.collect[Int, Pure](walk(t))    // List(1, 2, 3)
    ), List(1, 2, 3))
    assertEquals(!.run(
      Delim.collectUntil[Int, Vector[Int], Vector[Int], Pure](using FoldUntil.take(2))(walk(t))   // Vector(1, 2) — the third leaf is never visited
    ), Vector(1, 2))
    assertEquals(!.run(
      Delim.collectUntil[Int, Option[Int], Option[Int], Pure](using FoldUntil.find[Int](_ > 1))(walk(t))  // Some(2)
    ), Some(2))
  }

  def booking(using Delim.Asking[String, String, String, R]): String ! R = direct:
    val city   = !Delim.pause("Which city?")
    val nights = !Delim.pause(s"How many nights in $city?")
    val pay    = !Delim.pause(s"Pay ${nights.toInt * 90} for $city?")
    if pay == "yes" then s"Booked $city for $nights nights" else "Cancelled"

  def answering(as: List[String]): String => String ! Pure =
    var left = as
    _ => { val a = left.head; left = left.tail; pure(a) }

  test("stop in the middle, carry on later") {
    val start = !.run(Delim.resumable[String, String, String, Pure](booking))
    val done =
      !.run(Delim.drive(start)(answering(List("Kyiv", "3", "yes"))))
    assertEquals(done, "Booked Kyiv for 3 nights")
  }

  test("the journal outlives the process") {
    val p0 = !.run(Delim.resumable[String, String, String, Pure](booking))
    val (p1, j1) = !.run(Delim.answer(p0, Nil)("Kyiv"))
    val (p2, j2) = !.run(Delim.answer(p1, j1)("3"))    // j2 = List("Kyiv", "3")
    assertEquals(j2, List("Kyiv", "3"))
    assertEquals(p2.asking, Some("Pay 270 for Kyiv?"))
    val back = !.run(Delim.replay[String, String, String, Pure](booking)(j2))
    assertEquals(
      back.asking    // Some("Pay 270 for Kyiv?") — the same place
    , Some("Pay 270 for Kyiv?"))
  }
