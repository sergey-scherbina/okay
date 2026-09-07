package okay.intent

/**
 * The agreement estimator, on data where the right answer is known by
 * construction (specs/intent-autonomy.md §2.2). What it must do:
 * find the reliable labeler without being told, ignore abstentions,
 * survive a labeler that is always wrong, and be deterministic.
 */
class TestAgreement extends munit.FunSuite {

  private def v(labeler: String, intent: String, conf: Double = 1.0) =
    Agreement.Vote(labeler, intent, conf)

  /** three labelers over 12 messages: `good` is right every time,
   * `noisy` is right two thirds of the time, `wrong` never is */
  private val truth = Vector.fill(4)(Vector("A", "B", "C")).flatten
  private val votes: Seq[Vector[Agreement.Vote]] =
    truth.zipWithIndex.map { (t, i) =>
      val noisy = if i % 3 == 0 then "C" else t
      Vector(v("good", t), v("noisy", noisy), v("wrong", if t == "A" then "B" else "A"))
    }

  test("the reliable labeler is found without being told which it is") {
    val w = Agreement.estimate(votes)
    assert(w.of("good") > w.of("noisy"), w.show)
    assert(w.of("noisy") > w.of("wrong"), w.show)
    assert(w.of("good") >= 0.9, w.show)
  }

  test("the consensus under those weights is the truth") {
    val w = Agreement.estimate(votes)
    val got = votes.map(vs => Agreement.best(vs, w.byLabeler))
    assertEquals(got.count(_.isDefined), votes.length)
    assertEquals(got.zip(truth).count((g, t) => g.contains(t)), truth.length)
  }

  test("abstention is silence, not a vote: a labeler that says nothing changes nothing") {
    val w = Agreement.estimate(votes)
    val withSilent = votes.map(vs => vs :+ Agreement.Vote("silent", "", 0.0)).map(_.filterNot(_.intent.isEmpty))
    val w2 = Agreement.estimate(withSilent)
    assertEquals(w2.of("good"), w.of("good"))
    // and a message nobody answered has no consensus at all
    assertEquals(Agreement.best(Vector.empty, w.byLabeler), None)
    assertEquals(Agreement.decide(Vector.empty, w), None)
  }

  test("a labeler that is always wrong sinks to the floor but is not dropped") {
    val w = Agreement.estimate(votes)
    assertEquals(w.of("wrong"), Agreement.floor)
    assert(w.byLabeler.contains("wrong"), "kept, so a growing corpus can revise it")
  }

  test("the margin is a real knob: unanimity is wide, a split is narrow") {
    val w = Agreement.estimate(votes)
    val unanimous = Vector(v("good", "A"), v("noisy", "A"), v("wrong", "A"))
    val split = Vector(v("good", "A"), v("noisy", "B"))
    val (i1, m1) = Agreement.decide(unanimous, w).get
    val (_, m2) = Agreement.decide(split, w).get
    assertEquals(i1, "A")
    assert(m1 > m2, f"unanimous $m1%.2f should beat split $m2%.2f")
    assertEquals(m1, 1.0, "everyone agreeing leaves nothing for a runner-up")
  }

  test("deterministic: the same votes give the same weights, and it converges") {
    val a = Agreement.estimate(votes)
    val b = Agreement.estimate(votes)
    assertEquals(a.byLabeler, b.byLabeler)
    assert(a.passes <= 8, s"converged in ${a.passes} passes")
    // more passes must not change a fixed point
    assertEquals(Agreement.estimate(votes, passes = 40).byLabeler, a.byLabeler)
  }
}
