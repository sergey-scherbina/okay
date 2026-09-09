package okay.intent

/**
 * A RULE, BUILT RATHER THAN WRITTEN.
 *
 * A routing rule is an anchored, Unicode-aware regular expression,
 * and typed as a string it makes every property somebody needs from
 * it a discipline rather than a construction. Lifted here from
 * okay-chat, where it was written after one week's worth of exactly
 * those disciplines failing:
 *
 *  - Java's `\b` is ASCII-only without `(?U)`, so a Cyrillic rule
 *    with a bare `\b` silently never fires — every pattern has to
 *    start `(?iU)`, and somebody has to remember;
 *  - a gap written `.*` reached across a question mark into the next
 *    sentence, and a person asking about concerts was answered with
 *    the intake for offering one;
 *  - the same list of event nouns was written eight times in two
 *    spellings, and nothing could say the two had drifted.
 *
 * Here none of those is a discipline. `(?iU)` and the boundaries are
 * emitted, never typed. There is no combinator for `.*`: a gap is a
 * sentence, a bounded run of characters, or whitespace. A vocabulary
 * is a `val`, so it is written once and shared by every rule that
 * needs it.
 *
 * Nothing in this file knows any service. In okay-chat every one of
 * its 156 rules is built with this and generated into the file the
 * router loads; `Proof.Bytes` and `Proof.Behaviour` are how that
 * migration was held to the file it replaced, rule by rule.
 */
object Dsl:

  /**
   * A piece of a pattern that matches WORDS.
   *
   * `Stem` is the one that earns the type: `недел\w*` written by hand
   * is a `\w` away from `недел.*`, and under `(?U)` the two are very
   * different promises.
   */
  enum Term:
    /** exactly these characters */
    case Lit(text: String)
    /** a word and its endings: `недел` -> `недел\w*` */
    case Stem(text: String)
    /** a word and an optional tail: `event`, "s" -> `events?` */
    case Opt(text: String, tail: String)
    /** any one of them */
    case Any(of: Vector[Term])
    /** words in order, whitespace between: `this week` -> `this\s+week` */
    case Words(parts: Vector[Term])
    /** any one of them, and then the endings: `(?:a|b)\w*`. The file
     * writes the suffix once, outside the group, and that is a
     * different string from a stem per item — the same language, and
     * `Proof.Bytes` can only be claimed by saying it the same way */
    case AnyStem(of: Vector[Term])
    /** a word and at least one letter after it: `работ\w+` */
    case StemPlus(text: String)
    /** parts with NOTHING between them — for the places a rule glues
     * an optional onto the group after it */
    case Seq(parts: Vector[Term])
    /** this word and the space after it, or neither: `(?:там\s+)?`.
     * The trailing space belongs INSIDE the option, which is what
     * makes «что там с работой» and «что с работой» one rule */
    case MaybeThen(t: Term)
    /** this, or nothing: `(?:my )?`. Where `MaybeThen` owns the space
     * after the word, `Maybe` owns nothing but the term — an ending of
     * more than one letter (`удали(?:ть)?`), a word glued to the next */
    case Maybe(t: Term)
    /** this word and the space after it, any number of times:
     * `(?:(?:мою|все|эту)\s+)*` — the qualifiers a person puts before
     * «заявку», which `MaybeThen` says once and a person says twice */
    case ManyThen(t: Term)
    /** ONE OF THESE CHARACTERS: `[юяи]`, and `[юяи]?` when optional.
     * Not a stem: `мо[юяи]` is «мою», «моя», «мои» and NOT «моего»,
     * which `мо\w*` would also be. The set is spelled as the file
     * spells it — the letters, a hyphen, a comma, a space */
    case Chars(set: String, optional: Boolean)
    /** a stem that may not run on: `мо\w{1,3}` — «мой», «моей», «моими»
     * and not «монитор» */
    case StemUpTo(text: String, n: Int)
    /** NOT THESE, and then this: `(?!найти|искать)\w+(?:ть|ти)` — «хочу
     * СДЕЛАТЬ» is an offer and «хочу НАЙТИ» is a need, and the only way
     * to say so at the word is to name the words it must not be. The
     * lookahead consumes nothing; `t` is what the rule then reads */
    case Unless(not: Term, t: Term)
    /**
     * A fragment this builder cannot yet say, and the reason it
     * cannot — an inline optional, an alternation of whole rules.
     *
     * NOT a loophole: a `Raw` carries WHY, a caller's tests can count
     * them, and the count is a migration's own progress bar. What it must
     * never carry is a gap: `.*` is the mistake this file exists to
     * make unsayable.
     */
    case Raw(fragment: String, why: String)

  object Term:
    def render(t: Term): String = t match
      case Lit(s) => s
      case Stem(s) => s + "\\w*"
      case Opt(s, tail) => s + tail + "?"
      case Words(ps) => ps.map(render).mkString("\\s+")
      case Any(of) => "(?:" + of.map(render).mkString("|") + ")"
      case AnyStem(of) => "(?:" + of.map(render).mkString("|") + ")\\w*"
      case StemPlus(t) => t + "\\w+"
      case Seq(ps) => ps.map(render).mkString
      case MaybeThen(t) => "(?:" + render(t) + "\\s+)?"
      // an alternation is already a group: `(?:ть|ти)?`, not `(?:(?:ть|ти))?`
      case Maybe(a: Any) => render(a) + "?"
      case Maybe(t) => "(?:" + render(t) + ")?"
      case ManyThen(t) => "(?:" + render(t) + "\\s+)*"
      case Chars(set, optional) => "[" + set + "]" + (if optional then "?" else "")
      case StemUpTo(t, n) => t + "\\w{1," + n + "}"
      // an alternation needs no group inside a lookahead: `(?!a|b)`
      case Unless(Any(of), t) => "(?!" + of.map(render).mkString("|") + ")" + render(t)
      case Unless(n, t) => "(?!" + render(n) + ")" + render(t)
      case Raw(f, _) => f

    /** every `Raw` under this term, with its reason */
    def raws(t: Term): Vector[(String, String)] = t match
      case Raw(f, why) => Vector(f -> why)
      case Any(of) => of.flatMap(raws)
      case AnyStem(of) => of.flatMap(raws)
      case Words(ps) => ps.flatMap(raws)
      case Seq(ps) => ps.flatMap(raws)
      case MaybeThen(t) => raws(t)
      case Maybe(t) => raws(t)
      case ManyThen(t) => raws(t)
      case Unless(n, t) => raws(n) ++ raws(t)
      case _ => Vector.empty

  /**
   * WHAT MAY STAND BETWEEN TWO TERMS — and `.*` is not on the list.
   *
   * That absence is the whole point. The live defect this file was
   * written after was a trigger word in one sentence reaching an
   * event noun in the NEXT one, and the only thing that allowed it
   * was a gap that does not know what a sentence is.
   */
  enum Gap:
    /** whitespace, and the two terms stay in one breath */
    case Space
    /** whitespace or none: «спроси 3» and «спроси3» are one command */
    case Loose
    /** anything that is not the end of a sentence */
    case Sentence
    /** …and at most `n` characters of it */
    case Within(n: Int)
    /** at most `n` whole words between them */
    case Near(n: Int)

  object Gap:
    def render(g: Gap): String = g match
      case Space => "\\s+"
      case Loose => "\\s*"
      case Sentence => "\\b[^.!?]*\\b"
      case Within(n) => s"\\b[^.!?]{0,$n}\\b"
      case Near(n) => s"\\s+(?:\\w+\\s+){0,$n}"

  /**
   * WHERE A RULE MAY START.
   *
   * `Word` is the ordinary one: the trigger may sit anywhere in the
   * sentence, because a person writes «в субботу играем» as readily
   * as «играем в субботу».
   *
   * `Opening` is a COMMAND: it must open the message. That is not a
   * detail of spelling — it is what keeps «отмени» the command and
   * «я не хотел бы отменить» an ordinary sentence, and in the file it
   * is only visible as a `^\s*` somebody remembered to type.
   */
  enum Anchor:
    case Word, Opening

  /**
   * HOW A RULE ENDS.
   *
   * `Boundary` is the ordinary one. `Alone` says the message is JUST
   * this — «телеграм» on its own line is a request to connect one,
   * «телеграм у меня есть» is a sentence — and `AlonePunctuated`
   * allows what a person's thumb adds: «telegram?», «code.».
   */
  enum Ending:
    case Boundary, Alone, AlonePunctuated, Open, Colon

  /**
   * A rule: a term, then any number of gap-and-term steps. The
   * boundaries and the flags are the builder's business, not the
   * author's — which is what makes "every pattern starts (?iU)" a
   * fact about this file rather than a rule somebody remembers.
   */
  final case class Rule(head: Term, tail: Vector[(Gap, Term)] = Vector.empty,
                        anchor: Anchor = Anchor.Word,
                        ending: Ending = Ending.Boundary):
    def ~(step: (Gap, Term)): Rule = copy(tail = tail :+ step)
    /** the message is just this */
    def alone: Rule = copy(ending = Ending.Alone)
    /** …give or take what a thumb adds */
    def alonePunctuated: Rule = copy(ending = Ending.AlonePunctuated)
    /** it ends on an open token; there is nothing to close */
    def open: Rule = copy(ending = Ending.Open)
    /** it ends on a colon: «can: fix bikes» — the label form of an offer */
    def colon: Rule = copy(ending = Ending.Colon)
    def pattern: String =
      (if anchor == Anchor.Opening then "(?iU)^\\s*" else "(?iU)\\b") +
        Term.render(head) +
        tail.map((g, t) => Gap.render(g) + Term.render(t)).mkString +
        (ending match
          case Ending.Boundary => "\\b"
          case Ending.Alone => "\\s*$"
          case Ending.AlonePunctuated => "\\s*[?!.]*\\s*$"
          // NOTHING TO CLOSE: a rule that ends on an open token has
          // already consumed to the next space, and a `\\b` after it
          // would demand the last character be a word one — «сценарий
          // deal!» would stop matching
          case Ending.Open => ""
          case Ending.Colon => "\\s*:")
    def raws: Vector[(String, String)] =
      Term.raws(head) ++ tail.flatMap((_, t) => Term.raws(t))

  /**
   * A whole rule this builder cannot say yet — an alternation of two
   * rules, an optional segment in the middle. It carries its reason
   * and it is counted, like every other `Raw`.
   */
  final case class RawRule(pattern0: String, why: String):
    def pattern: String = pattern0
    def raws: Vector[(String, String)] = Vector(pattern0 -> why)

  /**
   * WHICH PROOF HOLDS A RULE, said in the code rather than in a head.
   *
   * `Bytes` is the strong one: the builder writes the same characters
   * the file has, so nothing about the running service can differ.
   * `Behaviour` is for the rules where the spelling differs ON
   * PURPOSE — the file ends an alternation of stems without a closing
   * boundary, which is how «концерты» matches a rule that says
   * «концерт»; written as stems they render `концерт\w*` and close,
   * which is the same promise said where a reader can see it. Those
   * are proven by what the two patterns DO over every phrase we have,
   * and the reason travels with the rule.
   */
  enum Proof:
    case Bytes
    case Behaviour(why: String)

  /**
   * EITHER OF THESE RULES — an alternation of whole rules, which the
   * file writes as `\b…\b|\b…\b` under one `(?iU)`. Five rules had this
   * shape and were quoted for it alone. A branch may itself be a
   * `RawRule` — four of the five keep a `.*` branch that stays quoted
   * with its reason — so what `either` buys is that the OTHER branch
   * is built, and the quote shrinks to the part that earned it.
   */
  final case class EitherRule(alts: Vector[Rule | RawRule]):
    def pattern: String =
      "(?iU)" + alts.map(a => patternOf(a).stripPrefix("(?iU)")).mkString("|")
    def raws: Vector[(String, String)] = alts.flatMap(rawsOf)

  type AnyRule = Rule | RawRule | EitherRule

  /** a rule together with the proof that it says what the file says */
  final case class Entry(rule: AnyRule, proof: Proof = Proof.Bytes)
  extension (r: AnyRule) def by(p: Proof): Entry = Entry(r, p)
  def patternOf(r: AnyRule): String = r match
    case x: Rule => x.pattern
    case x: RawRule => x.pattern
    case x: EitherRule => x.pattern
  def rawsOf(r: AnyRule): Vector[(String, String)] = r match
    case x: Rule => x.raws
    case x: RawRule => x.raws
    case x: EitherRule => x.raws
  def either(alts: (Rule | RawRule)*): EitherRule = EitherRule(alts.toVector)

  // ---- the words a rule is written with ------------------------------
  def lit(s: String): Term = Term.Lit(s)
  def stem(s: String): Term = Term.Stem(s)
  def opt(s: String, tail: String = "s"): Term = Term.Opt(s, tail)
  def any(ts: Term*): Term = Term.Any(ts.toVector)
  def anyOf(ts: Vector[Term]*): Term = Term.Any(ts.toVector.flatten)
  def words(ts: Term*): Term = Term.Words(ts.toVector)
  def anyStem(ts: Term*): Term = Term.AnyStem(ts.toVector)
  def anyStemOf(ts: Vector[Term]): Term = Term.AnyStem(ts)
  def stemPlus(s: String): Term = Term.StemPlus(s)
  def seq(ts: Term*): Term = Term.Seq(ts.toVector)
  def maybeThen(t: Term): Term = Term.MaybeThen(t)
  def maybe(t: Term): Term = Term.Maybe(t)
  def manyThen(t: Term): Term = Term.ManyThen(t)
  def chars(set: String): Term = Term.Chars(set, optional = false)
  def maybeChars(set: String): Term = Term.Chars(set, optional = true)
  def stemUpTo(s: String, n: Int): Term = Term.StemUpTo(s, n)
  /** not these words, and then this */
  def unless(not: Term)(t: Term): Term = Term.Unless(not, t)
  /** THE ARGUMENT a command carries. A deal number and a word are
   * shapes rather than vocabulary, and naming them here is what keeps
   * `\d+` out of the places a reader would have to decode it */
  val number: Term = Term.Raw("\\d+", "a number — a shape, not a word")
  val token: Term = Term.Raw("\\S+", "one token, whatever it is called")
  def raw(fragment: String, why: String): Term = Term.Raw(fragment, why)

  /** a rule that starts here */
  def rule(t: Term): Rule = Rule(t)
  /** …and one that must OPEN the message, which is what a command is */
  def command(t: Term): Rule = Rule(t, anchor = Anchor.Opening)
  extension (r: Rule)
    infix def sentence(t: Term): Rule = r ~ (Gap.Sentence -> t)
    infix def space(t: Term): Rule = r ~ (Gap.Space -> t)
    infix def loose(t: Term): Rule = r ~ (Gap.Loose -> t)
    infix def within(n: Int): WithinStep = WithinStep(r, n)
    infix def near(n: Int): NearStep = NearStep(r, n)
  final case class WithinStep(r: Rule, n: Int):
    infix def of(t: Term): Rule = r ~ (Gap.Within(n) -> t)
  final case class NearStep(r: Rule, n: Int):
    infix def of(t: Term): Rule = r ~ (Gap.Near(n) -> t)
