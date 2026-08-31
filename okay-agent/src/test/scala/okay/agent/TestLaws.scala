package okay.agent

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/**
 * The compactor's invariants under random conversations. A budget
 * that holds on the turns one thought of is not a budget; the point
 * of the policy is that it holds on the ones one did not.
 */
class TestLaws extends munit.ScalaCheckSuite {

  val turn: Gen[Turn] = Gen.oneOf(
    Gen.alphaStr.map(Turn.User(_)),
    Gen.alphaStr.map(Turn.Assistant(_, Nil)),
    Gen.alphaStr.map(s => Turn.Result("id", s)),
    Gen.alphaStr.map(Turn.System(_)))

  val conversation: Gen[List[Turn]] = Gen.listOf(turn)
  val budgets: Gen[Int] = Gen.oneOf(10, 40, 200, 1000)

  property("the presented view never exceeds the budget") {
    forAll(conversation, budgets) { (turns: List[Turn], budget: Int) =>
      val policy = Compact.window(budget)(Compact.chars)
      val view = policy.present(turns.foldLeft(policy.init)(policy.add))
      // system turns are PINNED, so they are the one thing that may
      // push a view past the budget — the policy says so, and the
      // property must say the same rather than pretend otherwise
      val pinned = view.collect { case s: Turn.System => Compact.chars(s) }.sum
      view.map(Compact.chars).sum <= budget + pinned
    }
  }

  property("the view is a suffix of the conversation, plus pins and a marker") {
    forAll(conversation, budgets) { (turns: List[Turn], budget: Int) =>
      val policy = Compact.window(budget)(Compact.chars)
      val view = policy.present(turns.foldLeft(policy.init)(policy.add))
      val recent = view.filterNot {
        case _: Turn.System => true
        case _: Turn.Summary => true
        case _ => false
      }
      // whatever survived, survived in order and from the END
      turns.filterNot(_.isInstanceOf[Turn.System]).endsWith(recent)
    }
  }

  property("system turns are never evicted") {
    forAll(Gen.alphaStr, conversation) { (sys: String, rest: List[Turn]) =>
      val policy = Compact.window(20)(Compact.chars)
      val turns = Turn.System(sys) :: rest
      val view = policy.present(turns.foldLeft(policy.init)(policy.add))
      view.contains(Turn.System(sys))
    }
  }

  property("merge of a LOSSLESS policy is exactly the sequential fold") {
    // Compact.all keeps everything, so it is a genuine Monoid and the
    // P1 contract applies to it in full
    forAll(conversation) { (turns: List[Turn]) =>
      val policy = Compact.all
      val whole = turns.foldLeft(policy.init)(policy.add)
      (0 to turns.length).forall { at =>
        val (l, r) = turns.splitAt(at)
        policy.present(policy.merge(
          l.foldLeft(policy.init)(policy.add),
          r.foldLeft(policy.init)(policy.add))) == policy.present(whole)
      }
    }
  }

  property("merge of the WINDOW yields a valid window, not the same one") {
    // A correction the generator forced, and it is about a CLAIM
    // rather than a line of code: a sliding window cannot be
    // split-point agnostic. Evicting inside the right half throws
    // away turns the whole fold could have kept, so merging two
    // windows re-applies the window to the join and lands on a
    // legitimate window over it — which is all a lossy policy can
    // promise. The exact-merge contract belongs to Compact.all and to
    // the statistics aggregators (variance, count), whose merges lose
    // nothing. The earlier example test passed only because its turns
    // were all the same size.
    forAll(conversation, budgets) { (turns: List[Turn], budget: Int) =>
      val policy = Compact.window(budget)(Compact.chars)
      (0 to turns.length).forall { at =>
        val (l, r) = turns.splitAt(at)
        val merged = policy.merge(
          l.foldLeft(policy.init)(policy.add),
          r.foldLeft(policy.init)(policy.add))
        val view = policy.present(merged)
        val recent = view.filterNot {
          case _: Turn.System | _: Turn.Summary => true
          case _ => false
        }
        val pinned = view.collect { case s: Turn.System => Compact.chars(s) }.sum
        // within budget, in order, and a suffix of what it was given
        view.map(Compact.chars).sum <= budget + pinned &&
          turns.filterNot(_.isInstanceOf[Turn.System]).endsWith(recent)
      }
    }
  }

  property("elision is reported whenever anything was dropped") {
    forAll(conversation, budgets) { (turns: List[Turn], budget: Int) =>
      val policy = Compact.window(budget)(Compact.chars)
      val view = policy.present(turns.foldLeft(policy.init)(policy.add))
      val kept = view.count {
        case _: Turn.System | _: Turn.Summary => false
        case _ => true
      }
      val dropped = turns.count(!_.isInstanceOf[Turn.System]) - kept
      // something fell out if and only if the view says so
      (dropped > 0) == view.exists(_.isInstanceOf[Turn.Summary])
    }
  }
}
