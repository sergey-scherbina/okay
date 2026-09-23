package okay

import okay.Direct.*
import scala.language.implicitConversions

/** a GADT whose first case is concrete — the shape that needs `: X` */
enum Ask[+A]:
  case Count() extends Ask[Long]
  case Name(id: Int) extends Ask[String]
  case Flag() extends Ask[Boolean]

object GadtTail:
  type S = State % Long

  /** the spelling that works: the tail match ascribed to the answer */
  def answer[X](q: Ask[X]): X ! S = direct:
    (q match
      case Ask.Count() => !State.modify[Long](_ + 1)
      case Ask.Name(id) => s"n$id"
      case Ask.Flag() => (!State.get[Long]) > 0): X

  def run[X](q: Ask[X]): X = State.run[Long, X](0L)(answer(q))._2

/** the compiler's shape, with no macro and no library in it */
final case class CovBox[+A](a: A)
final case class InvBox[A](a: A)

/**
 * direct-gadt-match-expected-type — REFUTED AS FILED (2026-09-23).
 * The entry asked the `direct` macro to ascribe the block's answer to
 * a tail `match`. It cannot: an inline method's arguments are TYPED
 * BEFORE the macro runs, and the arms have already failed by then.
 * Nor can `apply`'s signature carry it: a leading `using Pin[A]`
 * (contravariant, to maximise `A`) and a prefix object whose `apply`
 * is typed after `A` were both tried, and both fail identically.
 *
 * The mechanism is dotty 3.9's and is general: a `match` over a GADT
 * whose expected type is a type variable bounded only ABOVE — which
 * is what a covariant result (`Free[F, +A]`, `CovBox[+A]`) leaves —
 * has that variable fixed by the first arm, down to its singleton
 * (`Required: (1L : Long)` in the plain probe). An INVARIANT result
 * bounds it on both sides and the arms type at `X`. The last two
 * tests pin both halves with no macro in sight, so the day a Scala
 * release changes this, they say so and the ascription can go.
 */
class TestDirectGadtTail extends munit.FunSuite:
  test("the ascribed tail match runs every arm at its own type") {
    assertEquals(GadtTail.run(Ask.Count()), 1L)
    assertEquals(GadtTail.run(Ask.Name(7)), "n7")
    assertEquals(GadtTail.run(Ask.Flag()), false)
  }

  test("PIN: without the ascription the direct block does not compile") {
    val e = compileErrors("""
      def bare[X](q: Ask[X]): X ! GadtTail.S = direct:
        q match
          case Ask.Count() => !State.modify[Long](_ + 1)
          case Ask.Name(id) => s"n$id"
          case Ask.Flag() => (!State.get[Long]) > 0
    """)
    assert(e.contains("Required: Long"), e)
  }

  test("PIN: the same failure with a covariant box and no macro — the compiler's, not ours") {
    val e = compileErrors("""
      def mk[A](block: => A): CovBox[A] = CovBox(block)
      def cov[X](q: Ask[X]): CovBox[X] = mk:
        q match
          case Ask.Count() => 1L
          case Ask.Name(i) => s"n$i"
          case Ask.Flag() => true
    """)
    assert(e.contains("Required: (1L : Long)"), e)
  }

  test("... and an invariant box types every arm at X") {
    def mk[A](block: => A): InvBox[A] = InvBox(block)
    def inv[X](q: Ask[X]): InvBox[X] = mk:
      q match
        case Ask.Count() => 1L
        case Ask.Name(i) => s"n$i"
        case Ask.Flag() => true
    assertEquals(inv(Ask.Name(3)).a, "n3")
  }
