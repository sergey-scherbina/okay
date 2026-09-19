package okay

import Condition.*
import Condition.Decision.*

/**
 * THE BOOK'S CHAPTER 15, COMPILED (docs/continuations/15-resumable-exceptions.md).
 *
 * Chapter 1's importer, finished: it meets a bad row at forty
 * thousand and does NOT decide what that means. The same importer,
 * unchanged, under four different policies.
 */
class TestBookResumable extends munit.FunSuite {

  /** what the importer says when it cannot parse a row */
  final case class Malformed(line: Int, raw: String)

  val file = List("10", "20", "oops", "40")

  /**
   * The importer. Note what is NOT here: no `strict` flag, no
   * `onError` callback, no error enum in the return type, and no
   * decision about what a bad row means.
   */
  def parse(line: Int, raw: String): Int ! Op =
    raw.toIntOption match
      case Some(n) => pure(n)
      case None => signal[Int](Malformed(line, raw))

  /** one restart frame per row, so skipping abandons ONE row and the
   * rest of the file still runs */
  def load(rows: List[String]): Vector[Int] ! Op =
    def go(i: Int, left: List[String]): Vector[Int] ! Op = left match
      case Nil => pure(Vector.empty)
      case r :: rest =>
        for
          head <- within[Option[Int], Pure]("skip")(parse(i, r).map(Some(_)))(_ => None)
          more <- go(i + 1, rest)
        yield head.fold(more)(_ +: more)
    go(1, rows)

  // ---- the SAME importer, four policies, chosen by the caller

  test("a nightly batch skips the bad row and keeps the rest") {
    val got = !.run(Condition.run[Vector[Int], Pure] {
      case (Malformed(_, _), _) => Invoke("skip", ())
      case _ => Fail
    }(load(file)))
    assertEquals(got, Vector(10, 20, 40))
  }

  test("a migration substitutes a default, IN PLACE, and the row survives") {
    val got = !.run(Condition.run[Vector[Int], Pure] {
      case (Malformed(_, _), _) => Resume(0)
      case _ => Fail
    }(load(file)))
    assertEquals(got, Vector(10, 20, 0, 40),
      "the resumed value did not come back at the signal point")
  }

  test("a compliance run refuses, and the report names the row") {
    val e = intercept[Unhandled](!.run(Condition.run[Vector[Int], Pure] {
      (_, _) => Fail
    }(load(file))))
    assertEquals(e.condition, Malformed(3, "oops"))
  }

  test("a policy can decide PER ROW, because it sees the condition") {
    val got = !.run(Condition.run[Vector[Int], Pure] {
      case (Malformed(line, _), _) => if line > 2 then Resume(-1) else Invoke("skip", ())
      case _ => Fail
    }(load(file)))
    assertEquals(got, Vector(10, 20, -1, 40))
  }

  // ---- the property that makes it "resumable" and not "catchable"

  test("THE POINT: work done BEFORE the signal is not lost") {
    var parsed = 0
    def counted(line: Int, raw: String): Int ! Op =
      raw.toIntOption match
        case Some(n) => parsed += 1; pure(n)
        case None => signal[Int](Malformed(line, raw))
    def go(i: Int, left: List[String]): Vector[Int] ! Op = left match
      case Nil => pure(Vector.empty)
      case r :: rest =>
        for
          head <- within[Option[Int], Pure]("skip")(counted(i, r).map(Some(_)))(_ => None)
          more <- go(i + 1, rest)
        yield head.fold(more)(_ +: more)

    val got = !.run(Condition.run[Vector[Int], Pure] {
      case (Malformed(_, _), _) => Resume(99)
      case _ => Fail
    }(go(1, file)))
    assertEquals(got, Vector(10, 20, 99, 40))
    // the two rows parsed before the bad one were NOT re-parsed, and
    // the one after it was still reached: nothing unwound
    assertEquals(parsed, 3)
  }

  test("the menu is part of the report when nobody answers") {
    val e = intercept[Unhandled](!.run(Condition.run[Vector[Int], Pure] {
      (_, _) => Fail
    }(load(file))))
    assert(e.menu.contains("skip"), s"the report does not offer the menu: ${e.menu}")
  }
}
