package okay.laws

import okay.Optic

/**
 * THE ARROW LAWS, ONCE, FOR EVERY CARRIER (specs/arrows-plan.md,
 * Decision 2).
 *
 * Two lanes wanted the same laws on the same day and each was written
 * to wait for the other: `optics-arrow-instances` (BACKLOG) adds
 * `Arrow[Function1]` and the Kleisli, `static-workflow-proc`
 * (specs/static-workflow.md stage 1) adds `Proc`'s. A piece two lanes
 * need and neither owns is the piece that gets written twice, so it
 * is written here first, with NO instance in it: this lane adds laws
 * and nothing else, and each later lane instantiates in one line.
 *
 * The statements are Hughes (2000) in Paterson's normal form, plus
 * the `ArrowChoice` half stated on `Optic.Choice.right` — the mirror
 * of the literature's `left`, because `right` is what a prism needs
 * and therefore what this library has.
 *
 * WHAT MAKES IT REUSABLE IS THE OBSERVATION, AND `TestMealy` FOUND IT
 * FIRST: two arrows are equal when they answer the same over the same
 * INPUTS — and for a machine that must be a SEQUENCE of inputs,
 * because a single step cannot show a state. A pure function is the
 * same statement with a one-element sequence, so one shape serves
 * both and neither carrier needs an `Eq`.
 */
final class ArrowLaws[P[_, _]](val A: Optic.Arrow[P] & Optic.Choice[P],
                               val sample: P[Int, Int],
                               val observe: ArrowLaws.Observe[P],
                               val inputs: Seq[Int] = Vector(1, 2, 3, -1, 0, 7)):

  import A.*

  /** the two arrows answer the same over these inputs, or the answers */
  private def same[X, Y](l: P[X, Y], r: P[X, Y], xs: Seq[X]): Option[String] =
    val a = observe.run(l, xs)
    val b = observe.run(r, xs)
    if a == b then None else Some(s"$a  !=  $b")

  // the pure functions the laws lift; three, so that composition has
  // something to be associative about
  private val f: Int => Int = _ + 1
  private val g: Int => Int = _ * 3
  private val h: Int => Int = _ - 2

  private val pairs: Seq[(Int, Int)] = inputs.map(i => (i, i * 10))
  private val nested: Seq[((Int, Int), Int)] = inputs.map(i => ((i, i * 10), i * 100))

  /** BOTH SIDES OF THE SUM ARE EXERCISED, and that is the point of the
   * choice laws: `right` must step its arrow on a `Right` and leave it
   * untouched on a `Left`, so a sample that is all one shape proves
   * nothing about a stateful carrier. */
  private val eithers: Seq[Either[Int, Int]] =
    inputs.zipWithIndex.map((i, n) => if n % 3 == 0 then Left(i * 100) else Right(i))

  private val sums: Seq[Either[Int, Either[Int, Int]]] =
    inputs.zipWithIndex.map: (i, n) =>
      n % 3 match
        case 0 => Left(i * 100)
        case 1 => Right(Left(i * 10))
        case _ => Right(Right(i))

  private def assocP(ac: ((Int, Int), Int)): (Int, (Int, Int)) = (ac._1._1, (ac._1._2, ac._2))

  private def assocE(e: Either[Int, Either[Int, Int]]): Either[Either[Int, Int], Int] = e match
    case Left(c1) => Left(Left(c1))
    case Right(Left(c2)) => Left(Right(c2))
    case Right(Right(a)) => Right(a)

  // ── Category ─────────────────────────────────────────────────────

  private def catLeftId = same(compose(sample, id[Int]), sample, inputs)
  private def catRightId = same(compose(id[Int], sample), sample, inputs)
  private def catAssoc =
    val a1 = sample
    val a2 = arr(g)
    val a3 = compose(sample, arr(h))
    same(compose(compose(a3, a2), a1), compose(a3, compose(a2, a1)), inputs)

  // ── Arrow (Hughes' nine, in Paterson's normal form) ──────────────

  private def arrId = same(arr[Int, Int](identity), id[Int], inputs)
  private def arrCompose = same(arr(f.andThen(g)), compose(arr(g), arr(f)), inputs)
  private def firstArr =
    same(first[Int, Int, Int](arr(f)),
      arr[(Int, Int), (Int, Int)](ac => (f(ac._1), ac._2)), pairs)
  private def firstCompose =
    same(first[Int, Int, Int](compose(arr(g), sample)),
      compose(first[Int, Int, Int](arr(g)), first[Int, Int, Int](sample)), pairs)
  /** first f >>> arr fst  ==  arr fst >>> f — the law that catches an
   * arrow performing its step twice, which for `Proc` would be a
   * duplicated activity */
  private def firstFst =
    same(compose(arr[(Int, Int), Int](_._1), first[Int, Int, Int](sample)),
      compose(sample, arr[(Int, Int), Int](_._1)), pairs)
  private def firstSecond =
    val idg = (ac: (Int, Int)) => (ac._1, g(ac._2))
    same(compose(arr(idg), first[Int, Int, Int](sample)),
      compose(first[Int, Int, Int](sample), arr(idg)), pairs)
  private def firstAssoc =
    same(compose(arr(assocP), first[(Int, Int), (Int, Int), Int](first[Int, Int, Int](sample))),
      compose(first[Int, Int, (Int, Int)](sample), arr(assocP)), nested)

  // ── Choice, stated on `right` ────────────────────────────────────

  private def rightArr =
    same(right[Int, Int, Int](arr(f)),
      arr[Either[Int, Int], Either[Int, Int]](_.map(f)), eithers)
  private def rightCompose =
    same(right[Int, Int, Int](compose(arr(g), sample)),
      compose(right[Int, Int, Int](arr(g)), right[Int, Int, Int](sample)), eithers)
  /** f >>> arr Right  ==  arr Right >>> right f */
  private def rightPure =
    same(compose(arr[Int, Either[Int, Int]](Right(_)), sample),
      compose(right[Int, Int, Int](sample), arr[Int, Either[Int, Int]](Right(_))), inputs)
  /** the untaken side commutes: mapping the LEFT does not disturb the
   * arrow on the right, in either order */
  private def rightSkip =
    val lg = (e: Either[Int, Int]) => e.left.map(g)
    same(compose(arr(lg), right[Int, Int, Int](sample)),
      compose(right[Int, Int, Int](sample), arr(lg)), eithers)
  private def rightAssoc =
    same(compose(arr(assocE), right[Either[Int, Int], Either[Int, Int], Int](right[Int, Int, Int](sample))),
      compose(right[Int, Int, Either[Int, Int]](sample), arr(assocE)), sums)

  /** every law, by name, in the order a reader of Hughes would meet them */
  def all: List[(String, () => Option[String])] = List(
    "category: id >>> f == f" -> (() => catLeftId),
    "category: f >>> id == f" -> (() => catRightId),
    "category: composition is associative" -> (() => catAssoc),
    "arrow: arr(identity) == id" -> (() => arrId),
    "arrow: arr(g . f) == arr(f) >>> arr(g)" -> (() => arrCompose),
    "arrow: first(arr f) == arr(f x id)" -> (() => firstArr),
    "arrow: first(f >>> g) == first(f) >>> first(g)" -> (() => firstCompose),
    "arrow: first(f) >>> arr(fst) == arr(fst) >>> f" -> (() => firstFst),
    "arrow: first(f) >>> arr(id x g) == arr(id x g) >>> first(f)" -> (() => firstSecond),
    "arrow: first(first f) >>> arr(assoc) == arr(assoc) >>> first(f)" -> (() => firstAssoc),
    "choice: right(arr f) == arr(_.map(f))" -> (() => rightArr),
    "choice: right(f >>> g) == right(f) >>> right(g)" -> (() => rightCompose),
    "choice: f >>> arr(Right) == arr(Right) >>> right(f)" -> (() => rightPure),
    "choice: mapping the Left commutes with right(f)" -> (() => rightSkip),
    "choice: right(right f) >>> arr(assoc) == arr(assoc) >>> right(f)" -> (() => rightAssoc),
  )

  /** the laws that do NOT hold, named — what makes the suite itself
   * testable: a law suite nobody has seen refuse proves nothing
   * (no-failing-test-no-fix) */
  def violations: List[String] = all.flatMap((name, check) => check().map(why => s"$name — $why"))

object ArrowLaws:

  /**
   * HOW AN ARROW IS OBSERVED. `Out` is a member rather than `Any`
   * because the two sides of a law are only ever compared with each
   * other: a `Mealy` answers `Vector[Y]`, a logging function answers
   * `Seq[(Vector[String], Y)]`, and neither needs to be widened for
   * `==` to mean what it should.
   */
  trait Observe[P[_, _]]:
    type Out[_]
    def run[X, Y](p: P[X, Y], xs: Seq[X]): Out[Y]

/**
 * The laws as a munit suite: one test per law, so a failure NAMES the
 * law rather than reporting that fifteen of them together are not
 * true. A carrier instantiates it in one line.
 */
abstract class ArrowLawsSuite[P[_, _]] extends munit.FunSuite:
  def laws: ArrowLaws[P]
  laws.all.foreach: (name, check) =>
    test(s"law — $name"):
      check().foreach(why => fail(s"$name\n  $why"))
