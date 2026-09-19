import okay.Optic
import okay.laws.{ArrowLaws, ArrowLawsSuite}

/**
 * A FUNCTION THAT WRITES AS IT GOES — the smallest carrier at which a
 * WRONG arrow instance can be written down, which is what this file
 * exists for.
 *
 * `okay.laws.ArrowLaws` is a law suite, and a law suite nobody has
 * seen refuse anything proves nothing (no-failing-test-no-fix: a
 * guard's acceptances cannot fail, only a refusal shows it works). So
 * the suite is instantiated here TWICE — at an instance that is right
 * and at one that is wrong in a specific, interesting way — and the
 * second is asserted to be caught, by name.
 *
 * It is a local test fixture and NOT an instance of the library's:
 * `Arrow[Function1]` and the Kleisli belong to `optics-arrow-instances`
 * and `Proc`'s to `static-workflow-proc` (specs/arrows-plan.md,
 * Decision 2 — this lane adds laws and no instance).
 */
final case class Log[A, B](run: A => (Vector[String], B))

object Log:

  /** the arrow that is RIGHT */
  object good extends LogArrow:
    def first[A, B, C](p: Log[A, B]): Log[(A, C), (B, C)] =
      Log: ac =>
        val (w, b) = p.run(ac._1)
        (w, (b, ac._2))

  /**
   * THE ARROW THAT IS WRONG, and wrong the way that matters: `first`
   * runs its argument TWICE and keeps the second answer. Every answer
   * it gives is correct — only the writing is doubled — so a test
   * comparing results alone would pass it.
   *
   * That is exactly the defect `Proc` must not have: an arrow carrying
   * a workflow's leaf, run twice under a `first`, is an activity
   * performed twice. The laws catch it because `first(f) >>> arr(fst)`
   * must equal `arr(fst) >>> f`, and one of those writes once.
   */
  object doubled extends LogArrow:
    def first[A, B, C](p: Log[A, B]): Log[(A, C), (B, C)] =
      Log: ac =>
        val (w1, _) = p.run(ac._1)
        val (w2, b) = p.run(ac._1)
        (w1 ++ w2, (b, ac._2))

  /** everything both instances agree on */
  trait LogArrow extends Optic.Arrow[Log] with Optic.Choice[Log]:
    def dimap[A, B, C, D](p: Log[A, B])(f: C => A, g: B => D): Log[C, D] =
      Log: c =>
        val (w, b) = p.run(f(c))
        (w, g(b))

    def right[A, B, C](p: Log[A, B]): Log[Either[C, A], Either[C, B]] =
      Log:
        case Left(c) => (Vector.empty, Left(c))
        case Right(a) =>
          val (w, b) = p.run(a)
          (w, Right(b))

    def arr[A, B](f: A => B): Log[A, B] = Log(a => (Vector.empty, f(a)))

    def compose[A, B, C](g: Log[B, C], f: Log[A, B]): Log[A, C] =
      Log: a =>
        val (w1, b) = f.run(a)
        val (w2, c) = g.run(b)
        (w1 ++ w2, c)

  /** the sample arrow, and it WRITES — an arrow whose every value is
   * an `arr` cannot tell the two instances above apart */
  def sample: Log[Int, Int] = Log(i => (Vector(s"step($i)"), i + 1))

  def observe: ArrowLaws.Observe[Log] = new ArrowLaws.Observe[Log]:
    type Out[Y] = Seq[(Vector[String], Y)]
    def run[X, Y](p: Log[X, Y], xs: Seq[X]): Seq[(Vector[String], Y)] = xs.map(p.run)

/** the laws hold at the right instance */
class TestArrowLaws extends ArrowLawsSuite[Log]:
  def laws: ArrowLaws[Log] = ArrowLaws(Log.good, Log.sample, Log.observe)

/** and REFUSE the wrong one — the half that makes the suite evidence */
class TestArrowLawsRefuse extends munit.FunSuite:

  test("the suite refuses an arrow whose `first` runs its step twice"):
    val broken = ArrowLaws(Log.doubled, Log.sample, Log.observe)
    val named = broken.violations
    assert(named.nonEmpty,
      "the law suite accepted an instance that performs its step twice — " +
        "it can no longer be evidence for any carrier")
    // EXACTLY TWO LAWS CATCH IT, and the first draft of this test
    // claimed four. Watching it fail is what said which — and the
    // reason is worth more than the list: a law catches a doubled
    // `first` only when `first` appears a DIFFERENT NUMBER OF TIMES
    // on its two sides.
    //   - `first(f) >>> arr(fst)` has one `first`, `arr(fst) >>> f`
    //     has none: one side doubles, the other cannot. It fires.
    //   - `first(first f) >>> arr(assoc)` has two against one. Fires,
    //     and shows four writes against two.
    //   - `first(f >>> g) == first(f) >>> first(g)` has one against
    //     two — but `g` is an `arr`, which writes nothing, so doubling
    //     it is invisible and both sides read the same.
    //   - `first(f) >>> arr(id x g)` has one `first` on each side, so
    //     the defect is applied to both equally and cancels.
    val fire = List(
      "arrow: first(f) >>> arr(fst) == arr(fst) >>> f",
      "arrow: first(first f) >>> arr(assoc) == arr(assoc) >>> first(f)")
    fire.foreach: law =>
      assert(named.exists(_.startsWith(law)), s"'$law' did not fire:\n${named.mkString("\n")}")
    // the matched control, so the claim is exact in both directions
    val quiet = List(
      "arrow: first(f >>> g) == first(f) >>> first(g)",
      "arrow: first(f) >>> arr(id x g) == arr(id x g) >>> first(f)")
    quiet.foreach: law =>
      assert(!named.exists(_.startsWith(law)),
        s"'$law' fired, so the comment above is now wrong about why it cannot")
    assertEquals(named.size, fire.size, named.mkString("\n"))

  test("a doubled `first` is invisible to the ANSWERS alone"):
    // why the laws are needed at all: the wrong instance answers
    // exactly what the right one does, and only the writing differs
    val right = Log.good.first[Int, Int, Int](Log.sample).run((3, 9))
    val wrong = Log.doubled.first[Int, Int, Int](Log.sample).run((3, 9))
    assertEquals(wrong._2, right._2)
    assertEquals(right._1.size, 1)
    assertEquals(wrong._1.size, 2)

  test("`arr` alone cannot tell the two instances apart"):
    // the reason `sample` writes: at an `arr` both instances agree
    val law = "arrow: first(arr f) == arr(f x id)"
    val onArr = ArrowLaws(Log.doubled, Log.good.arr((i: Int) => i + 1), Log.observe)
    assertEquals(onArr.violations, Nil,
      "a sample with nothing to write hides the defect — that is why Log.sample writes")
    assert(ArrowLaws(Log.doubled, Log.sample, Log.observe).violations.forall(!_.startsWith(law)),
      "the arr law is not the one that catches this, and should not be claimed to be")
