package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 5, COMPILED (docs/continuations/05-leave-early.md).
 *
 * Every runnable snippet on that page is here, in the same order and
 * with the same names, so the page cannot drift from the library
 * without this going red. The chapter says so, and this is what makes
 * saying so honest.
 */
class TestBookLeaveEarly extends munit.FunSuite {

  // ---- the domain the chapter uses, which is chapter 1's invoice

  final case class Alloc(centre: String, amount: Int)
  final case class Line(sku: String, allocations: List[Alloc])
  final case class Invoice(id: String, lines: List[Line])
  final case class Rejected(centre: String)

  val closedCentres = Set("CC-OLD", "CC-FROZEN")
  def closed(centre: String): Boolean = closedCentres(centre)

  val invoice = Invoice("inv-1", List(
    Line("a", List(Alloc("CC-1", 10), Alloc("CC-2", 20))),
    Line("b", List(Alloc("CC-3", 30), Alloc("CC-OLD", 40))),
    Line("c", List(Alloc("CC-4", 50)))))

  val clean = Invoice("inv-2", List(
    Line("a", List(Alloc("CC-1", 10))),
    Line("b", List(Alloc("CC-2", 20)))))

  // ---- the chapter's version

  def check(inv: Invoice): Option[Rejected] ! Pure =
    Delim.delimited[Option[Rejected], Pure]:
      direct:
        for line <- inv.lines; alloc <- line.allocations do
          if closed(alloc.centre) then !Delim.exit(Some(Rejected(alloc.centre)))
        None

  test("it leaves both loops with the answer") {
    assertEquals(!.run(check(invoice)), Some(Rejected("CC-OLD")))
  }

  test("nothing closed: the block runs to its end and answers None") {
    assertEquals(!.run(check(clean)), None)
  }

  test("it stops AT the first closed centre, and looks at nothing after it") {
    var seen = 0
    def counting(inv: Invoice): Option[Rejected] ! Pure =
      Delim.delimited[Option[Rejected], Pure]:
        direct:
          for line <- inv.lines; alloc <- line.allocations do
            seen += 1
            if closed(alloc.centre) then !Delim.exit(Some(Rejected(alloc.centre)))
          None
    assertEquals(!.run(counting(invoice)), Some(Rejected("CC-OLD")))
    // four allocations were examined; the fifth was never reached
    assertEquals(seen, 4)
  }

  // ---- the chapter's "usual way", for the comparison it makes

  def checkThreaded(inv: Invoice): Option[Rejected] =
    def line(l: Line): Option[Rejected] =
      l.allocations.foldLeft(Option.empty[Rejected]):
        case (found @ Some(_), _) => found
        case (None, a) => if closed(a.centre) then Some(Rejected(a.centre)) else None
    inv.lines.foldLeft(Option.empty[Rejected]):
      case (found @ Some(_), _) => found
      case (None, l) => line(l)

  test("the usual way agrees, which is the point of the comparison") {
    assertEquals(checkThreaded(invoice), !.run(check(invoice)))
    assertEquals(checkThreaded(clean), !.run(check(clean)))
  }

  // ---- what the chapter warns about: the exit is not an exception

  test("exit is not catchable as an exception, because it is not one") {
    var ranAfter = false
    val r = Delim.delimited[Int, Pure]:
      direct:
        !Delim.exit(1)
        ranAfter = true      // unreachable: the rest was discarded
        2
    assertEquals(!.run(r), 1)
    assert(!ranAfter, "code after the exit ran")
  }
}
