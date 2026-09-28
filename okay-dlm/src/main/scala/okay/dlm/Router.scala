package okay.dlm

import okay.rag.Embedding

/**
 * THE FOUR-LAYER ROUTER: what the person taught, then a rule, then a
 * typo of a rule's word, then a probability — and a question when
 * none of them is sure (specs/dlm.md, "Router").
 *
 * The scores are DERIVED and never compared across layers: a rule
 * answers 1.00 because a rule matched, not because we are probably
 * right. `Support` says which layer spoke and why.
 *
 * The rule layer is unanchored on purpose: a trigger word decides
 * wherever it sits in the sentence, because «Я программист, ищу
 * работу» puts the verb in the middle. Where two intents' rules both
 * match, the one whose rule matches EARLIEST in the text wins — the
 * one tie-break that needs no authored priority list and generalises
 * to any two intents that ever share a word.
 *
 * The semantic layer is a probe fitted from the exemplars at
 * construction, accepted on a MARGIN of probabilities rather than on a
 * score: a message equally close to two intents is ambiguous no matter
 * how confident the winner looks, and a clarifying question is worth
 * more there than a tool call.
 *
 * NO WORDS LIVE HERE. `Route.Missing` says a value is missing; what to
 * ask about it is the caller's, in the caller's language.
 */
final class Router(val intents: Intents,
                   /** the vector layer's judge: ours over the compiled
                    * exemplars by default (`Router.apply`), a remote one
                    * by `Router.judged`; `None` is a router on rules and
                    * typos alone, which is a supported deployment */
                   val judge: Option[Judge] = None,
                   /** the encoder the near band compares lessons with;
                    * `None` leaves the band off whatever `nearBar` says */
                   embed: Option[String => Embedding] = None,
                   /** how much daylight the winner needs: a difference
                    * of PROBABILITIES between the top two. Being wrong
                    * is the more expensive of the two mistakes, so the
                    * bar sits where the coverage/accuracy curve pays
                    * accuracy for coverage */
                   val margin: Float = 0.5f,
                   /**
                    * How much text the VECTOR layer needs before it is
                    * allowed to decide. «For a job» — three words
                    * answering a question the caller had just asked —
                    * scored high enough to be recorded as an offer. A
                    * fragment resembles many things and means none of
                    * them on its own. The rule layer still reads any
                    * length, because a rule that matches has read the
                    * whole message.
                    */
                   val minSemanticLetters: Int = 12,
                   /** the near band's bar: `None` is OFF, not a number
                    * chosen for it. A lesson applied to a paraphrase is
                    * served only once a caller has measured a bar */
                   val nearBar: Option[Float] = None,
                   /** which languages a trigger word could belong to —
                    * typo tolerance never crosses a language, because a
                    * real word of one is not a misspelling of a real
                    * word of another. `Alphabet.none` isolates nothing */
                   val alphabet: Alphabet = Alphabet.none):

  /** the one question the vector layer asks: which of the intents
   * that opted in — a command with an exact argument is never among
   * the options, so a judge cannot reach it however it reads */
  val question: Judge.Question = Judge.Question.of(intents.semantic.map(_.name))

  private val rules: Vector[(Intent, Vector[scala.util.matching.Regex])] =
    intents.intents.map(i => i -> i.rules.map(_.r))

  private val slotPatterns: Map[String, Vector[(Slot, scala.util.matching.Regex)]] =
    intents.intents.map(i => i.name -> i.slots.map(s => s -> s.pattern.r)).toMap

  /**
   * The typo-tolerant vocabulary, mined from the SAME rules the exact
   * layer already checks (`Fuzzy.literalTriggers`). An intent whose
   * rules are all structural contributes nothing here and is simply
   * never reached by typo tolerance — by construction, not by a list
   * someone had to remember to exclude it from.
   */
  val fuzzyVocab: Map[String, Vector[String]] =
    intents.intents.map(i => i.name -> i.rules.flatMap(Fuzzy.literalTriggers).distinct)
      .filter(_._2.nonEmpty).toMap

  private val fuzzyByLang: Map[String, Vector[(String, Set[String])]] =
    fuzzyVocab.map((n, ws) => n -> ws.map(w => w -> alphabet.languagesOf(w)))

  /** the semantic layer is live when a judge is, and there is
   * something to ask it about */
  val semantic: Boolean = judge.isDefined && question.options.nonEmpty

  private def judged(t: String): Option[Judge.Choice] =
    if !semantic then None else judge.flatMap(_.choose(t, question))

  def slotsOf(intent: String, text: String): Map[String, String] =
    slotPatterns.getOrElse(intent, Vector.empty).flatMap { (slot, re) =>
      re.findFirstMatchIn(text)
        .map(m => (if m.groupCount >= 1 then m.group(1) else m.matched).trim)
        .filter(_.nonEmpty)
        .orElse(Option.when(slot.fallback)(text.trim).filter(_.nonEmpty))
        .map(slot.name -> _)
    }.toMap

  /** scores per intent, best first — exposed because an operator
   * tuning a threshold needs to SEE the numbers, not guess them */
  def scores(text: String): Vector[(String, Float)] =
    judged(text).map(_.probabilities.map((i, p) => i -> p.toFloat)).getOrElse(Vector.empty)

  private def complete(i: Intent, text: String, support: Support): Route =
    val got = slotsOf(i.name, text)
    i.require.find(r => !got.contains(r)) match
      case Some(missing) => Route.Missing(i.name, missing)
      case None => Route.Fires(i.name, got, support)

  /**
   * A near-miss of a trigger word, when no exact rule matched. Only
   * DISTANCE ≥ 1 is looked at — an exact hit would have fired above.
   * Firing requires the winning intent's best near miss to be STRICTLY
   * closer than every other intent's: a tie between two candidate
   * typos is exactly the case "not sure what you mean" exists for.
   */
  private def fuzzyRoute(t: String, lang: Option[String]): Option[(String, Int)] =
    val tokens = Fuzzy.tokenize(t)
    val perIntent = fuzzyByLang.toVector.flatMap { (name, entries) =>
      val canon = lang match
        case Some(l) if !alphabet.isEmpty => entries.filter((_, ls) => ls.contains(l)).map(_._1)
        case _ => entries.map(_._1)
      val hits = tokens.flatMap(tok => Fuzzy.bestMatch(tok.text, canon)).map(_._2).filter(_ >= 1)
      if hits.nonEmpty then Some(name -> hits.min) else None
    }.sortBy(_._2)
    perIntent match
      case (name, d) +: rest if rest.headOption.forall(_._2 > d) => Some(name -> d)
      case _ => None

  /** the intent whose rule matches EARLIEST in the text, not the one
   * that happens to sit first in the file */
  private def earliestRuleMatch(t: String): Option[(Intent, String)] =
    rules.flatMap { (i, rs) =>
      rs.flatMap(r => r.findFirstMatchIn(t).map(m => (r.regex, m.start)))
        .minByOption(_._2).map((rule, start) => (i, rule, start))
    }.minByOption(_._3).map((i, rule, _) => (i, rule))

  /**
   * EVERYTHING the layers noticed in a message, not only the winner:
   * every intent a rule matched, in reading order; the typo layer's
   * hit; the probe's top two, when the vector layer is allowed to
   * look. The head is exactly what `route` fires — `route` decides,
   * this only reports — so a caller may keep deciding on `route` and
   * still SEE the second fact a message carried.
   */
  def noticed(text: String, lang: Option[String] = None,
              memory: Memory = Memory.empty, who: String = ""): Vector[(String, Support)] =
    val t = text.trim
    val byMemory: Vector[(String, Support)] =
      Memory.exact(memory, who, t).map((p, near) => p.intent -> Support.Remembered(p.offset, near)).toVector ++
        near(memory, who, t).map((p, c) => p.intent -> Support.Remembered(p.offset, c)).toVector
    val byRule: Vector[(String, Support)] =
      rules.flatMap { (i, rs) =>
        rs.flatMap(r => r.findFirstMatchIn(t).map(m => (i.name, r.regex, m.start))).minByOption(_._3)
      }.sortBy(_._3).map((name, rule, _) => name -> Support.Exact(Some(rule)))
    val byTypo: Vector[(String, Support)] =
      if byRule.nonEmpty then Vector.empty
      else fuzzyRoute(t, lang).toVector.map((name, d) => name -> Support.Typo(d))
    val bySemantic: Vector[(String, Support)] =
      if byRule.nonEmpty || byTypo.nonEmpty || t.count(_.isLetter) < minSemanticLetters then Vector.empty
      else judged(t).map { c =>
        val ranked = c.probabilities.take(2)
        ranked.zipWithIndex.map { case ((name, p), k) =>
          name -> Support.Semantic(p.toFloat, ranked.lift(k + 1).map(_._1)) }
      }.getOrElse(Vector.empty)
    (byMemory ++ byRule ++ byTypo ++ bySemantic).distinctBy(_._1)

  /**
   * THE MEMORY'S EXACT BAND, BEFORE THE RULES. A pair the person
   * taught — «this sentence meant THAT» — decides ahead of every
   * authored rule, because the person's own sentence overruled a
   * rule. The slots are read from the sentence at hand by the intent's
   * own patterns, never copied from the teaching turn.
   */
  private def remembered(memory: Memory, who: String, t: String): Option[Route] =
    Memory.exact(memory, who, t).flatMap((p, near) =>
      intents.byName(p.intent).map(complete(_, t, Support.Remembered(p.offset, near))))

  /** THE NEAR BAND: this person's OWN lessons, to the intents the
   * vector layer may reach, by cosine — off until `nearBar` is set */
  private def near(memory: Memory, who: String, t: String): Option[(Lesson, Float)] =
    nearBar.flatMap { bar =>
      if t.count(_.isLetter) < minSemanticLetters then None
      else embed.flatMap(f => Memory.near(memory, who, t, f,
        i => intents.byName(i).exists(_.semantic), bar))
    }

  private def nearRoute(memory: Memory, who: String, t: String): Option[Route] =
    near(memory, who, t).flatMap((p, c) =>
      intents.byName(p.intent).map(complete(_, t, Support.Remembered(p.offset, c))))

  def route(text: String, lang: Option[String] = None,
            memory: Memory = Memory.empty, who: String = ""): Route =
    val t = text.trim
    remembered(memory, who, t).getOrElse(routed(t, lang, memory, who))

  private def routed(t: String, lang: Option[String], memory: Memory, who: String): Route =
    earliestRuleMatch(t) match
      case Some((i, rule)) => complete(i, t, Support.Exact(Some(rule)))
      case None => fuzzyRoute(t, lang) match
        case Some((name, d)) =>
          // the same slot machinery an exact rule uses: an exact-format
          // slot is unaffected by a misspelled VERB, and free text falls
          // back to the whole message exactly as it does for a rule
          intents.byName(name).map(complete(_, t, Support.Typo(d)))
            .getOrElse(Route.Unclear(Vector.empty, 0f))
        case None if t.count(_.isLetter) < minSemanticLetters =>
          // too little to route on, and asking is the honest answer
          nearRoute(memory, who, t).getOrElse(Route.Unclear(scores(t).take(2).map(_._1), 0f))
        case None =>
          nearRoute(memory, who, t).getOrElse(judged(t) match
            case Some(v) if v.margin >= margin =>
              intents.byName(v.best).map(complete(_, t, Support.Semantic(v.probability.toFloat, v.runnerUp)))
                .getOrElse(Route.Unclear(Vector.empty, v.probability.toFloat))
            case Some(v) =>
              // the two it could not separate, which is what a
              // clarifying question is FOR
              Route.Unclear(Vector(v.best) ++ v.runnerUp, v.probability.toFloat)
            case None => Route.Unclear(Vector.empty, 0f))

  /**
   * AN EXACT COMMAND: an intent that carries an exact argument (it
   * opted out of the vector layer) and that a rule — or a lesson —
   * fired on. A message the rules place is not an answer to a
   * free-text question of the caller's, and this is the one predicate
   * every free-text slot asks before swallowing a message.
   */
  def command(text: String, lang: Option[String] = None,
              memory: Memory = Memory.empty, who: String = ""): Option[String] =
    route(text, lang, memory, who) match
      case Route.Fires(i, _, Support.Exact(_) | Support.Remembered(_, _))
        if intents.byName(i).exists(!_.semantic) => Some(i)
      case _ => None

object Router:

  /**
   * OURS: the vector layer as a probe over the compiled exemplars,
   * through the encoder given — the shape the first consumer wrote.
   * Without exemplars or an encoder the layer is off and the router
   * runs on its rules, which is a supported deployment.
   */
  def apply(intents: Intents,
            exemplars: Option[Exemplars] = None,
            embed: Option[String => Embedding] = None,
            margin: Float = 0.5f,
            minSemanticLetters: Int = 12,
            nearBar: Option[Float] = None,
            alphabet: Alphabet = Alphabet.none): Router =
    val judge = for e <- exemplars if e.rows.nonEmpty; f <- embed yield Judge.probe(e, f)
    new Router(intents, judge, embed, margin, minSemanticLetters, nearBar, alphabet)

  /**
   * ANY JUDGE for the vector layer — a remote one, or ours behind a
   * remote one (`Judge.orElse`) — and the encoder from scope for the
   * near band. The rules and the typos decide first exactly as
   * before: a judge is only ever asked what a rule could not place.
   */
  def judged(intents: Intents, judge: Judge,
             margin: Float = 0.5f, minSemanticLetters: Int = 12,
             nearBar: Option[Float] = None, alphabet: Alphabet = Alphabet.none)
            (using e: Embedder): Router =
    new Router(intents, Some(judge), Some(e(_)), margin, minSemanticLetters, nearBar, alphabet)

  /** the model's own configuration: the exemplars judged as the scope
   * says (`Judge.Fit`), the near band over the encoder in scope */
  def of(intents: Intents, exemplars: Option[Exemplars],
         margin: Float = 0.5f, minSemanticLetters: Int = 12,
         nearBar: Option[Float] = None, alphabet: Alphabet = Alphabet.none)
        (using fit: Judge.Fit, e: Embedder): Router =
    new Router(intents, exemplars.filter(_.rows.nonEmpty).map(fit(_)), Some(e(_)),
      margin, minSemanticLetters, nearBar, alphabet)
