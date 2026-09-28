package okay.dlm

/**
 * Typo tolerance, sitting between an exact rule and the vector layer.
 *
 * Word order is not this layer's job — a trigger word decides wherever
 * it sits, because every rule is unanchored. What is left is spelling:
 * «нужн работа» or «ищю программиста» match no exact rule and no
 * authored example closely enough, and used to fall straight to the
 * vector layer, which is expensive and was never trained to be
 * typo-robust in the first place.
 *
 * The technique is RapidFuzz's `token_set_ratio`: treat the utterance
 * as a set of tokens, and let a bounded edit distance absorb a
 * misspelling or an inflection ending — «ищу»/«ищем»/«ищешь» share a
 * root, and a fixed edit budget catches that without a stemmer.
 *
 * The trigger vocabulary is not authored twice. Every rule of the
 * simple shape `\b(?:w1|w2|...)\b` has its bare words extracted at
 * load time (`literalTriggers`), so this layer's vocabulary is always
 * exactly what the exact-rule layer already knows about.
 */
object Fuzzy:

  final case class Token(text: String, start: Int, end: Int)

  private val wordRe = "[\\p{L}\\p{N}]+".r

  def tokenize(s: String): Vector[Token] =
    wordRe.findAllMatchIn(s).map(m => Token(m.matched, m.start, m.end)).toVector

  /**
   * Bounded Levenshtein distance. Returns `max + 1` as soon as the
   * true distance is certain to exceed `max`, so a wildly different
   * pair of words costs a row, not the full O(n·m) table — the check
   * runs once per token per candidate trigger, so this matters.
   */
  def distance(a: String, b: String, max: Int): Int =
    if math.abs(a.length - b.length) > max then max + 1
    else
      val al = a.toLowerCase; val bl = b.toLowerCase
      val prev = Array.tabulate(bl.length + 1)(identity)
      val curr = new Array[Int](bl.length + 1)
      var i = 1
      var truncated = false
      while i <= al.length && !truncated do
        curr(0) = i
        var rowBest = curr(0)
        var j = 1
        while j <= bl.length do
          val cost = if al(i - 1) == bl(j - 1) then 0 else 1
          curr(j) = math.min(math.min(prev(j) + 1, curr(j - 1) + 1), prev(j - 1) + cost)
          if curr(j) < rowBest then rowBest = curr(j)
          j += 1
        if rowBest > max then truncated = true
        else { Array.copy(curr, 0, prev, 0, curr.length); i += 1 }
      if truncated then max + 1 else prev(bl.length)

  /**
   * How many edits a word this long may absorb before tolerance risks
   * turning it into a DIFFERENT word.
   *
   * Two letters or fewer: none — «да» and «не» are one edit apart and
   * mean opposite things. Three to five: one — where the most common
   * trigger verbs live. Six or more: two, since a longer word carries
   * more of its own identity per remaining letter.
   *
   * A caller's own suite should check this choice against the ACTUAL
   * authored vocabulary rather than trust the arithmetic: no two
   * trigger words from DIFFERENT intents may be closer than the sum of
   * their own tolerances, or a typo of one becomes a false hit on the
   * other (`collisions`).
   */
  def tolerance(len: Int): Int =
    if len <= 2 then 0 else if len <= 5 then 1 else 2

  /** the closest canonical word this token could be, if within its
   * tolerance — `None` when the token is simply not a near miss of
   * anything in the vocabulary */
  def bestMatch(token: String, canon: Vector[String]): Option[(String, Int)] =
    canon.iterator.map(c => c -> distance(token, c, tolerance(c.length)))
      .filter((c, d) => d <= tolerance(c.length))
      .minByOption(_._2)

  /**
   * Extract the bare trigger words from an ALREADY-authored rule, when
   * the rule is exactly the shape `\b(?:w1|w2|...)\b`, optionally with
   * a `\w*`/`\w+` stem suffix. A rule of any other shape — a phrase, a
   * digit pattern, an email regex — answers empty and is simply not
   * fuzzy-matched; it keeps working through the exact layer alone.
   * That is a feature: an exact-format slot must never be
   * typo-corrected into a different one.
   */
  def literalTriggers(rule: String): Vector[String] =
    val prefix = "(?iU)\\b(?:"
    if !rule.startsWith(prefix) then Vector.empty
    else
      val rest = rule.drop(prefix.length)
      val close = rest.indexOf(')')
      if close < 0 then Vector.empty
      else
        val inside = rest.take(close)
        val tail = rest.drop(close + 1)
        if !Set("\\b", "\\w*\\b", "\\w+\\b").contains(tail) then Vector.empty
        else inside.split('|').toVector
          .filter(w => w.nonEmpty && w.forall(c => c.isLetter || c.isDigit))

  /**
   * The vocabulary's own consistency: pairs of trigger words from
   * DIFFERENT intents that lie within each other's tolerance, so that
   * a typo of one is a hit on the other. A caller asserts this empty
   * over its authored rules — the check that turns "the arithmetic
   * looks right" into a fact about the data.
   */
  def collisions(vocab: Map[String, Vector[String]],
                 languagesOf: String => Set[String] = _ => Set.empty): Vector[(String, String, String, String)] =
    val words = vocab.toVector.flatMap((i, ws) => ws.map(w => (i, w.toLowerCase)))
    for
      (ia, a) <- words
      (ib, b) <- words if ia < ib
      // a language never collides with another: the router isolates them
      if languagesOf(a).isEmpty || languagesOf(b).isEmpty || (languagesOf(a) & languagesOf(b)).nonEmpty
      tol = math.min(tolerance(a.length), tolerance(b.length))
      if distance(a, b, tol) <= tol
    yield (ia, a, ib, b)
