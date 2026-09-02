package okay.agent

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import okay.codec.Json

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

  // ---- Compact.skillState (SKILL.state, arxiv 2608.26263) ----

  private def norm(j: Json): Json = j match
    case Json.JObj(fs) => Json.JObj(fs.map((k, v) => (k, norm(v))).sortBy(_._1))
    case Json.JArr(vs) => Json.JArr(vs.map(norm))
    case other => other

  private val jsonScalar: Gen[Json] = Gen.oneOf(
    Gen.alphaStr.map(Json.JStr(_)),
    Gen.choose(-100, 100).map(n => Json.JNum(n.toDouble)),
    Gen.const(Json.JNull))

  /** a small object over a FIXED three-key alphabet: enough overlap
   * for later patches to overwrite or delete earlier ones, which is
   * the whole point of exercising this against a random walk */
  private val jsonPatch: Gen[Json] =
    for
      keys <- Gen.someOf(Seq("a", "b", "c"))
      vals <- Gen.listOfN(keys.size, jsonScalar)
    yield Json.JObj(keys.zip(vals).toVector)

  private val skillTurn: Gen[Turn] = Gen.oneOf(
    Gen.alphaStr.map(Turn.User(_)),
    Gen.alphaStr.map(Turn.Assistant(_, Nil)),
    Gen.alphaStr.map(s => Turn.Result("id", s)),
    Gen.alphaStr.map(Turn.System(_)),
    jsonPatch.map(Turn.StatePatch(_)))

  private val skillConversation: Gen[List[Turn]] = Gen.listOf(skillTurn)

  property("skillState's view is O(1): the pins, plus exactly one rendered turn") {
    forAll(skillConversation) { (turns: List[Turn]) =>
      val policy = Compact.skillState(Json.print)
      val view = policy.present(turns.foldLeft(policy.init)(policy.add))
      val pins = turns.collect { case s: Turn.System => s }
      view.dropRight(1) == pins && view.lastOption.exists(_.isInstanceOf[Turn.User])
    }
  }

  property("Sigma is exactly the sequential RFC 7396 fold of the StatePatch turns, others skipped") {
    forAll(skillConversation) { (turns: List[Turn]) =>
      val policy = Compact.skillState(Json.print)
      val acc = turns.foldLeft(policy.init)(policy.add)
      val zero: Json = Json.JObj(Vector.empty)
      val expected = turns.collect { case Turn.StatePatch(p) => p }.foldLeft(zero)(Json.mergePatch)
      norm(acc.sigma) == norm(expected)
    }
  }

  property("the accumulator's observation is the LAST Result/User content, nothing older") {
    forAll(skillConversation) { (turns: List[Turn]) =>
      val policy = Compact.skillState(Json.print)
      val acc = turns.foldLeft(policy.init)(policy.add)
      val lastObs = turns.reverse.collectFirst {
        case r: Turn.Result => r
        case u: Turn.User => u
      }
      acc.observation == lastObs
    }
  }

  property("skillState's merge matches sequential folding when patches never delete across the split") {
    // the documented caveat (Compact.scala, Json.mergePatch) is about
    // a right-side deletion of a key only the LEFT side ever set —
    // restricted to patches that never delete, merge and sequential
    // folding must always agree
    val noDeletes: Gen[Json] =
      for
        keys <- Gen.someOf(Seq("a", "b", "c"))
        vals <- Gen.listOfN(keys.size, Gen.alphaStr.map(Json.JStr(_)))
      yield Json.JObj(keys.zip(vals).toVector)
    val turnsNoDeletes: Gen[List[Turn]] = Gen.listOf(Gen.oneOf(
      Gen.alphaStr.map(Turn.System(_)),
      noDeletes.map(Turn.StatePatch(_))))
    forAll(turnsNoDeletes, turnsNoDeletes) { (left: List[Turn], right: List[Turn]) =>
      val policy = Compact.skillState(Json.print)
      val sequential = (left ++ right).foldLeft(policy.init)(policy.add)
      val merged = policy.merge(
        left.foldLeft(policy.init)(policy.add),
        right.foldLeft(policy.init)(policy.add))
      norm(sequential.sigma) == norm(merged.sigma) && sequential.pinned == merged.pinned
    }
  }
}
