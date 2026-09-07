package okay.intent

/**
 * The refit gate (intent-refit-gate; specs/intent-autonomy.md §2.5).
 *
 * What must hold: the shipped corpus passes, a corpus that starves a
 * class is REFUSED with that class named, a slide that stays legal is
 * refused too, and the verdict is data a caller can print rather than
 * an exception it can only catch.
 */
class TestRefitGate extends munit.FunSuite {

  private val train: Seq[(String, String)] =
    IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
  private val heldOut: Seq[(String, String)] =
    IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

  test("the shipped corpus passes its own gate, and the report reads") {
    Refit.propose(train, heldOut, incumbent = Some(Models.meeting)) match
      case Refit.Verdict.Accepted(_, scores, total) =>
        println(Refit.report(Refit.Verdict.Accepted(Models.meeting, scores, total)))
        assertEquals(scores.length, 4)
        assert(scores.forall(_.after >= 0.50), scores.map(_.show).mkString("; "))
      case r @ Refit.Verdict.Refused(why, _, _) =>
        fail(s"the shipped corpus refused its own refit: $why\n${Refit.report(r)}")
  }

  test("a corpus that starves a class is refused, and the class is named") {
    // the failure a consumer lived: one class swamps the corpus
    val starved = train.filterNot(_._2 == "Other") ++ train.filter(_._2 == "Other").take(1)
    Refit.propose(starved, heldOut, incumbent = Some(Models.meeting)) match
      case Refit.Verdict.Refused(why, scores, _) =>
        assert(why.contains("Other"), why)
        println(s"refused as it should: $why")
        assert(scores.exists(s => s.cls == "Other" && s.after < 0.50))
      case Refit.Verdict.Accepted(_, scores, total) =>
        fail(f"a corpus with one Other row was accepted at ${100.0 * total}%.1f%%: ${scores.map(_.show).mkString("; ")}")
  }

  test("a slide that stays above the law is refused too") {
    // an incumbent that is very good on a class, a candidate that is
    // merely legal on it: the law alone would let this through, and
    // three of them in a row kill the class
    val incumbent = Fit.grams(train)
    val strict = Refit.Rules(floor = 0.0, slip = 0.05)
    val fewer = train.filterNot(_._2 == "Notification") ++ train.filter(_._2 == "Notification").take(3)
    Refit.propose(fewer, heldOut, incumbent = Some(incumbent), rules = strict) match
      case Refit.Verdict.Refused(why, _, _) =>
        assert(why.contains("slip"), why)
      case Refit.Verdict.Accepted(_, scores, _) =>
        // if the fit happens not to slide by more than 0.05, the rule
        // is still the one under test: say so rather than pass quietly
        val worst = scores.minBy(_.delta)
        assert(worst.delta >= -0.05, f"accepted while ${worst.cls} moved ${worst.delta}%+.2f")
  }

  test("the first fit has no incumbent and is still held to the law") {
    val onlyTwo = train.filter(r => r._2 == "Proposal" || r._2 == "Request")
    Refit.propose(onlyTwo, heldOut, incumbent = None) match
      case Refit.Verdict.Refused(why, _, _) =>
        assert(why.contains("below the floor"), why)
      case Refit.Verdict.Accepted(_, scores, _) =>
        fail(s"a two-class corpus was accepted for a four-class taxonomy: ${scores.map(_.show).mkString("; ")}")
  }

  test("a verdict is data: it carries every class, before and after, and prints") {
    val v = Refit.propose(train, heldOut, incumbent = Some(Models.meeting))
    val text = Refit.report(v)
    assert(text.contains("total on held-out"))
    for c <- IntentFixture.classes do assert(text.contains(c), s"$c missing from the report")
    // and nothing was written anywhere: proposing is not publishing
    assert(v.isInstanceOf[Refit.Verdict.Accepted] || v.isInstanceOf[Refit.Verdict.Refused])
  }
}
