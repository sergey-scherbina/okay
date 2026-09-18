import okay.*
import okay.Optic.arrows.*

/**
 * WHAT A WALK COSTS — the number specs/static-workflow.md's stage-5
 * cursor is gated on, and which nobody had taken
 * (`static-workflow-walk-cost`).
 *
 * The cursor chapter's trigger is "a term whose WALK is measured to
 * cost more than its next leaf's activity", and the Decisions section
 * answers it by citing the appendix: "the appendix already priced
 * O(1) restore against a microsecond prefix and found the benefit
 * inverted". Read the appendix and the number is about something
 * else — chapter 22's forty answers through the MONADIC replay, a
 * program being re-run. `Proc.walk` is a different function over a
 * different structure, and it had never been timed. A number about a
 * neighbouring shape, standing where this one's should be.
 *
 * So this measures the thing the trigger names, at journal lengths a
 * run might actually reach, and prints what it walked beside how long
 * it took — a walk that quietly did nothing would otherwise be the
 * fastest of all.
 *
 * THE BAR is the same sentence's other half: a LEAF'S ACTIVITY. A
 * leaf here is an outside call — a payment, a shipment, a person
 * answering a question — and the cheapest honest floor for one is
 * about 100 µs (a local round trip); a realistic one is milliseconds.
 * So the trigger fires at whatever journal length makes a walk cost
 * more than that.
 */
class MeasureProcWalk extends munit.FunSuite:

  type P = okay.Pure
  type Sig = Wf.Asked[String, String]

  val A: Optic.Arrow[[X, Y] =>> Proc[Sig, X, Y]] & Optic.Choice[[X, Y] =>> Proc[Sig, X, Y]] =
    Proc.procArrow[Sig]

  /**
   * A loop that asks once per round and stops after `n` — the shape a
   * long journal actually comes from, since a straight-line term has
   * as many nodes as records and nobody writes ten thousand.
   *
   * IT CARRIES AN `Int` AND NOT A `List`, and that is not a detail.
   * The first version of this file carried the answers in a list and
   * did `got :+ answer` and `got.length` per round — both O(k) at
   * round k — so it measured its own list handling and reported the
   * WALK as quadratic: 4.05 seconds at 40 000 records, 72.6x the time
   * of a tenth of the journal. The control caught the superlinearity;
   * it could not say whose it was. An instrument needs a term whose
   * own cost per round is constant, or it is measuring itself.
   */
  def loop(n: Int): Wf.Proc[String, String, Int, Int] =
    Proc.iter(
      Proc.alongside(Wf.Proc.ask[String, String, Int](k => s"q$k")) >>>
        A.arr: (p: (Int, String)) =>
          val next = p._1 + 1
          if next < n then Left(next) else Right(next))

  def journal(n: Int): List[Wf.Ans[String]] = List.fill(n)(Right("a"))

  /** the minimum of several rounds: one round lies */
  def best(rounds: Int)(body: => Int): (Long, Int) =
    var least = Long.MaxValue
    var seen = 0
    for _ <- 0 until rounds do
      val t0 = System.nanoTime()
      val got = body
      val t1 = System.nanoTime()
      if t1 - t0 < least then least = t1 - t0
      seen = got
    (least, seen)

  /**
   * TWO measurements whose RATIO is the answer, taken ALTERNATELY.
   *
   * WHY, measured: the control below compares a 4 000-record walk
   * against a 400-record one and wants about ten. Run in a
   * full-matrix gate it read 1.86x, 2.75x and 2.42x on three separate
   * occasions, against 9.1x every other way it has ever been run —
   * including inside a gate that passed, at 9.14x. `best(7)` does not
   * save it, because the seven rounds of the SMALL arm run as one
   * block: a bad window a few hundred microseconds wide covers all
   * seven, inflates the small arm alone, and the ratio collapses
   * toward the floor guard while the large arm, measured later, is
   * clean.
   *
   * Alternating is the fix and it is the same discipline as taking a
   * minimum: a transient now lands on BOTH arms or on neither, so it
   * cancels out of the ratio instead of forging it. Neither guard is
   * weakened — both still see a minimum over seven rounds.
   */
  def bestPair(rounds: Int)(small: => Int, large: => Int): (Long, Long) =
    var leastSmall = Long.MaxValue
    var leastLarge = Long.MaxValue
    for _ <- 0 until rounds do
      val t0 = System.nanoTime()
      val _ = small
      val t1 = System.nanoTime()
      val _ = large
      val t2 = System.nanoTime()
      if t1 - t0 < leastSmall then leastSmall = t1 - t0
      if t2 - t1 < leastLarge then leastLarge = t2 - t1
    (leastSmall, leastLarge)

  test("MEASURED: what a walk costs, against the activity it would save"):
    val sizes = Vector(40, 400, 4_000, 40_000)
    // warm the JIT on the shape, not on the sizes being reported
    for _ <- 0 until 50 do
      val _ = Wf.Proc.walk(loop(40))(0, journal(40))
    println("records   walk        per record   vs a 100 µs activity")
    for n <- sizes do
      val term = loop(n)
      val j = journal(n)
      val (ns, walked) = best(7):
        Wf.Proc.walk(term)(0, j) match
          // COUNT WHAT IT WALKED. A walk that stopped at the first
          // record would be very fast and mean nothing; the journal
          // is exactly `n` answers and the term wants exactly `n`, so
          // a correct walk ends DONE with `n` rooms in its hand.
          case Right(Wf.Proc.Standing.Done(got)) => got
          case other => fail(s"the walk did not finish: $other")
      assertEquals(walked, n, "the walk did not consume the journal it was given")
      val per = ns.toDouble / n
      println(f"$n%7d   ${ns / 1000.0}%8.1f µs  $per%7.0f ns   ${ns / 100_000.0}%5.2fx")
    // AND THE CONTROL: the detector must be able to say "slow". A
    // walk of ten times the journal takes about ten times as long, so
    // a number that does not move with `n` would be measuring
    // something else.
    def control(): (Long, Long) = bestPair(7)(
      Wf.Proc.walk(loop(400))(0, journal(400)) match
        case Right(Wf.Proc.Standing.Done(got)) => got
        case other => fail(s"$other"),
      Wf.Proc.walk(loop(4_000))(0, journal(4_000)) match
        case Right(Wf.Proc.Standing.Done(got)) => got
        case other => fail(s"$other"))

    // AND ONE RETRY, for the same reason the arms alternate. The two
    // guards below are laws about the WALK; a box that defeats one
    // whole control must not be allowed to state them. A transient
    // that survives two independent controls is the walk, one that
    // does not is the box — and this costs a tenth of a second.
    val first = control()
    val (small, large) =
      if first._2 > first._1 * 3 && first._2 < first._1 * 40 then first else control()
    println(f"control: 4000 records take ${large.toDouble / small}%.1fx the time of 400")
    // THE CROSSOVER, which is the answer the trigger actually wants:
    // a walk costs `per` nanoseconds a record, so it exceeds an
    // activity of A microseconds at A * 1000 / per records. Printed
    // for the three activities anybody would name.
    val per = large.toDouble / 4_000
    println(f"per record (steady): $per%.0f ns")
    for a <- Vector(100, 1_000, 10_000) do
      println(f"an activity of $a%6d µs is worth ${a * 1000 / per}%9.0f records of journal")
    assert(large > small * 3,
      s"ten times the journal took ${large.toDouble / small} times as long — " +
        "this is not measuring the walk")
    assert(large < small * 40,
      s"ten times the journal took ${large.toDouble / small} times as long — " +
        "a walk that is superlinear in the journal is the cursor's trigger firing, " +
        "and this file must say so rather than report a number as if it were linear")
