package okay.lex

import okay.*
import okay.given
import okay.Optic.arrows.*

/**
 * THE BLOCKS ON docs/arrows.md, RUN.
 *
 * The page is a tutorial, and a tutorial whose examples are not
 * executed rots at the first rename. Every code block there has a
 * test here, in the same order, with the same values — so the page
 * fails rather than lies (the discipline `docs/optics.md` and
 * `TestOpticsGuide` set on the same day).
 *
 * It lives in okay-lex because the stream half needs `Mealy`, which
 * is the one carrier where an arrow and a stream genuinely meet.
 */
class TestArrowsGuide extends munit.FunSuite {

  // ---------------------------------------------------------------- 1. the glyphs

  test("the glyphs, on the carrier every reader already has") {
    val inc: Int => Int = _ + 1
    val triple: Int => Int = _ * 3
    val size: String => Int = _.length

    assertEquals((inc >>> triple)(4), 15)              // then
    assertEquals((triple <<< inc)(4), 15)              // the other way round
    assertEquals((inc *** size)((4, "abc")), (5, 3))   // a pair, each half
    assertEquals((inc &&& triple)(4), (5, 12))         // one in, both out
    assertEquals((inc +++ size)(Left(4)), Left(5))     // a sum, each side
    assertEquals((inc ||| size)(Right("abcd")), 4)     // a sum, one answer
  }

  test("an effectful function composes with `>=>`, which is a different arrow") {
    val half: Int => Option[Int] = i => Option.when(i % 2 == 0)(i / 2)
    val dec: Int => Option[Int] = i => Option.when(i > 0)(i - 1)
    assertEquals((half >=> dec)(8), Some(3))
    assertEquals((half >=> dec)(3), None)
  }

  // ---------------------------------------------------------------- 2. arrows and streams

  /** a Mealy machine that counts what it has seen and answers the count */
  private def counting: Mealy[Char, Int] =
    def at(n: Int): Mealy[Char, Int] = Mealy(_ => { val m = n + 1; (at(m), m) })
    at(0)

  /** and one that answers whether the character is a digit */
  private def isDigit: Mealy[Char, Boolean] =
    // LAZY, because the machine names itself: a stateless one answers
    // with ITSELF as the next machine, and a strict `val` cannot
    // refer to the value it is defining. `Mealy.arr` is written the
    // same way for the same reason.
    lazy val m: Mealy[Char, Boolean] = Mealy(c => (m, c.isDigit))
    m

  test("TWO machines over ONE input, in one pass — what `&&&` is for") {
    val both = counting &&& isDigit
    assertEquals(Mealy.runAll(both, "a1b".toSeq),
      Vector((1, false), (2, true), (3, false)))
  }

  test("a machine followed by a plain step, in one pass — what `>>>` is for") {
    // `arr` OF THE MACHINE'S OWN ARROW, not a bare function: `>>>`
    // composes within ONE carrier, and a `Char => String` is an arrow
    // in `Function1`, not in `Mealy`. Lifting is what `arr` is for,
    // and the compiler says so if you forget.
    val M = Mealy.mealyArrow
    val labelled = counting >>> M.arr((n: Int) => s"#$n")
    assertEquals(Mealy.runAll(labelled, "abc".toSeq), Vector("#1", "#2", "#3"))
  }

  test("a machine keeps its state across the composition, which is the whole point") {
    // the same machine twice in one expression is TWO machines: an
    // arrow is a value, and composing it does not share its state
    val twice = counting &&& counting
    assertEquals(Mealy.runAll(twice, "ab".toSeq), Vector((1, 1), (2, 2)))
  }

  // ---------------------------------------------------------------- 3. optics beside them

  final case class Line(sku: String, qty: Int)
  final case class Order(id: String, lines: Vector[Line])
  private val eachLine = Lens[Order](_.lines).andThen(Traversal.each[Line, Line])
  private val order = Order("A-1", Vector(Line("pen", 2), Line("ink", 0)))

  test("an optic composes with `andThen`, and that is deliberate") {
    // no `>>>` for optics: `andThen` is the composition, and a second
    // spelling of one idea is what the glyph policy refuses
    assertEquals(eachLine.toVector(order), order.lines)
  }

  test("the applicative slot: the same optic asks what it WOULD do") {
    // Validated collects every problem instead of the first — the
    // property that makes the slot worth having
    given Semigroup[Vector[String]] with
      def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y
    def check(l: Line): Validated[Vector[String], Line] =
      if l.qty > 0 then Validated.Valid(l) else Validated.Invalid(Vector(s"${l.sku}: ${l.qty}"))
    assertEquals(eachLine.traverseOf[[X] =>> Validated[Vector[String], X]](check)(order),
      Validated.Invalid(Vector("ink: 0")))
  }

  // ---------------------------------------------------------------- 4. an arrow feeding an optic

  test("an arrow builds the function an optic walks with") {
    // where the two meet in practice: `&&&` makes the pair, the optic
    // puts it back. Neither knows about the other.
    val describe = ((l: Line) => l.qty) &&& ((l: Line) => l.sku.length)
    val out = eachLine.foldMap((l: Line) => Vector(describe(l)))(order)
    assertEquals(out, Vector((2, 3), (0, 3)))
  }
}
