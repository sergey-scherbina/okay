package okay.intent

/**
 * What the numbers would look like to somebody else
 * (specs/intent-classify.md).
 *
 * Everything this module measures was written by one hand, and one of
 * those measurements now ships. This suite does not fix that — a
 * second corpus by the same author measures the same thing twice — it
 * measures the GAP, in the two ways available without new data: where
 * in the corpus the score actually lives, and what a mechanical shift
 * of register costs.
 *
 * It PRINTS rather than asserts most of what it finds, because the
 * numbers are the deliverable and pinning them would freeze a
 * measurement whose whole point is that it is provisional. The two
 * assertions are the claims that ship.
 */
class TestSecondAuthor extends munit.FunSuite {

  private val train = IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
  private val held = IntentFixture.labelled.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
  private val router = Router.Router.offline()

  private def decided(m: String): Option[String] = router.route(m) match
    case Router.Action.Act(i, _) => Some(i)
    case Router.Action.Ask(i, _, _, _) => Some(i)
    case Router.Action.Escalate(_, _) => None

  private def score(rows: Seq[(String, String)]): (Int, Int) =
    val answered = rows.flatMap((m, g) => decided(m).map(_ -> g))
    (answered.count((a, g) => a == g), answered.size)

  // --- 1. where the score lives -----------------------------------

  private def trigrams(s: String): Set[String] =
    val t = s.toLowerCase.replaceAll("\\s+", " ")
    if t.length < 3 then Set(t) else (0 to t.length - 3).map(i => t.substring(i, i + 3)).toSet

  private def jaccard(a: Set[String], b: Set[String]): Double =
    if a.isEmpty && b.isEmpty then 1.0
    else (a intersect b).size.toDouble / (a union b).size

  test("does the score live in the near neighbours") {
    val trainGrams = train.map((m, _) => trigrams(m))
    val scored = held.map { (m, gold) =>
      val g = trigrams(m)
      (trainGrams.map(jaccard(g, _)).max, m, gold)
    }.sortBy(_._1)
    val half = scored.size / 2
    val (far, near) = (scored.take(half), scored.drop(half))
    def acc(rows: Seq[(Double, String, String)]) =
      val (right, n) = score(rows.map((_, m, g) => (m, g)))
      f"${100.0 * right / n}%5.1f%% of $n"
    println(f"[near] nearest-training similarity, median ${scored(half)._1}%.3f")
    println(s"[near] far  half (least like anything trained on): ${acc(far)}")
    println(s"[near] near half: ${acc(near)}")
    println(f"[near] most similar pair: ${scored.last._1}%.3f — ${scored.last._2}")
  }

  // --- 2. what a register shift costs -----------------------------

  private val hedges = Vector("hmm, ", "so ", "ok so ", "quick one — ")
  private val tails = Vector(", if that works", " — no rush", ", but no pressure", " btw")

  /** one transposition in the longest word, deterministically */
  private def typo(s: String): String =
    val words = s.split(" ")
    val i = words.indices.maxBy(j => words(j).length)
    val w = words(i)
    if w.length < 4 then s
    else
      val k = w.length / 2
      words.updated(i, w.take(k - 1) + w(k) + w(k - 1) + w.drop(k + 1)).mkString(" ")

  /** the politeness frame removed — for a Request this deletes the
   * cue the tier fires on, which is the point */
  private def blunt(s: String): String =
    val t = s.replaceAll("(?i)^(could|would|can) you (please )?", "")
      .replaceAll("(?i)^please ", "")
      .replaceAll("(?i)^(shall|can|could) we ", "")
      .replaceAll("(?i)^(i would appreciate it if you could|would you mind) ", "")
    if t == s then s else s"${t.head.toUpper}${t.tail}"

  private val shifts: Vector[(String, (String, Int) => String)] = Vector(
    "as written" -> ((m, _) => m),
    "lower" -> ((m, _) => m.toLowerCase.replaceAll("[.?!]+$", "")),
    "hedge" -> ((m, i) => hedges(i % hedges.size) + m.head.toLower + m.tail),
    "tail" -> ((m, i) => m.replaceAll("[.?!]+$", "") + tails(i % tails.size)),
    "typo" -> ((m, _) => typo(m)),
    "blunt" -> ((m, _) => blunt(m)))

  test("what a mechanical shift of register costs, per tier") {
    // per tier, because the two fail differently: the cues are
    // literal phrases and a typo silences them, while character
    // n-grams are supposed to be robust to exactly that — this is
    // where that claim gets checked rather than repeated
    shifts.foreach { (name, f) =>
      val rows = held.zipWithIndex.map { case ((m, gold), i) => (f(m, i), gold) }
      val (right, _) = score(rows)
      val cued = rows.flatMap((m, g) => Patterns.classify(Models.cues, m, 0.4).map(_ -> g))
      val gramsRight = rows.count((m, g) => CharGrams.score(Models.meeting, m).exists(_.best == g))
      println(f"[shift] $name%10s  composite ${100.0 * right / rows.size}%5.1f%%  |  cues fired ${cued.size}%2d right ${cued.count((a, g) => a == g)}%2d  |  grams alone ${100.0 * gramsRight / rows.size}%5.1f%%")
    }
  }

  test("an example of each shift, so the reader can judge the transformation") {
    val m = "Could you please send me the agenda before Thursday?"
    shifts.foreach((name, f) => println(f"[example] $name%10s  ${f(m, 0)}"))
  }

  // --- 3. what ships ----------------------------------------------

  test("the shipped number is what the shipped door delivers") {
    val (right, _) = score(held)
    assertEquals(right, 45, "the doc comment's 75.0% is this")
  }

  test("and it does not survive a shift of register unchanged") {
    // the claim that replaces the bare 76.7% wherever it is quoted:
    // the number holds for messages written like its training data
    val asWritten = score(held)._1
    val lowered = score(held.map((m, g) => (m.toLowerCase.replaceAll("[.?!]+$", ""), g)))._1
    assert(lowered <= asWritten,
      s"lowercasing improved the score ($lowered against $asWritten), which would be a finding of its own")
  }
}
