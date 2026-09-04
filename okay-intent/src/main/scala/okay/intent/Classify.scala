package okay.intent

import okay.codec.{Json, Schema, JsonSchema}

/**
 * Intent classification (specs/intent-classify.md): ONE `Schema[I]`
 * derivation is the taxonomy, the frame and the parser.
 *
 * A label is not what a caller can act on. "Proposal" does not answer
 * an email; `Proposal(when, who, where)` does. Both come from the same
 * derivation, because FrameNet's Frame Elements ARE a product's
 * required fields — the very thing `ToolSpec.jsonSchema` already
 * computes from "not `Option`, no default". So the enumeration the
 * model is shown and the decoder that reads its answer are one value,
 * and a label outside the taxonomy is a DECODE ERROR rather than a
 * class of parsing bug to defend against.
 *
 * Two axes are kept apart, because conflating them is the standard
 * mistake and the conflation is invisible afterwards. MULTI-INTENT
 * ("charged twice AND the app crashes") is two spans, both to be acted
 * on. AMBIGUITY is one span with ranked alternatives, one to be
 * chosen. A single flat list expresses neither, and a caller holding
 * one cannot tell which case it is in.
 */

/**
 * Categorical, never numeric: a model has no calibrated probability,
 * so a `Double` here would invent precision it cannot have. The
 * NUMERIC margin belongs to the (deferred) vector tier and is a
 * different quantity with a different threshold — deliberately not
 * this field.
 */
enum Conf:
  case Low, Medium, High

object Conf:
  /** on the wire a confidence is its own name, lowercased — and an
   * unrecognised one is a decode error, not a silent `Low` */
  given Schema[Conf] = Schema.refine[Conf, String](
    s => Conf.values.find(_.toString.equalsIgnoreCase(s))
      .toRight(s"unknown confidence '$s'"),
    _.toString.toLowerCase)

  /** Low < Medium < High, by declaration order */
  def atLeast(c: Conf, floor: Conf): Boolean = c.ordinal >= floor.ordinal

  /** the vocabulary, FROM the enum — a prompt that lists these cannot
   * drift from the decoder that accepts them */
  def vocabulary: String = Conf.values.map(_.toString.toLowerCase).mkString(", ")

/**
 * One candidate reading of a span.
 *
 * `conf` comes FIRST, and that is a measured decision rather than a
 * stylistic one. With `intent` first, nine of ten remaining
 * undecodable replies were the same malformation: the model wrote
 * `{"MeetingRequest": {...}, "conf": "high"}` — closing the intent's
 * object one brace too late and swallowing the sibling field. Emitted
 * before the nested object, `conf` has no object to fall into.
 */
final case class Alt[I](conf: Conf, intent: I)

/**
 * One stretch of the message carrying one intent.
 *
 * `why` precedes `alts` in this declaration AND therefore on the wire,
 * which is the point: measured on 24 labelled messages, reasoning
 * first scored 0.615 macro F1 against 0.479 for the label first, at a
 * cost of ~130 characters (specs/intent-classify.md, Results). Field
 * order is load-bearing here; do not reorder to tidy it.
 */
final case class Span[I](text: String, why: String, alts: List[Alt[I]])

/** what the model returns: the message, segmented */
final case class Reading[I](spans: List[Span[I]])

object Classify {

  given alt[I](using Schema[I]): Schema[Alt[I]] = Schema.derived
  given span[I](using Schema[I]): Schema[Span[I]] = Schema.derived
  given reading[I](using Schema[I]): Schema[Reading[I]] = Schema.derived

  /**
   * The class label of a value: the case name, and for a nested
   * taxonomy the path through its groups ("Proposal/NewSlot").
   *
   * `depth` is what makes a hierarchy pay: at depth 1 the confusion
   * matrix is over GROUPS, at full depth over leaves, and an error
   * inside a group is then visibly a different thing from an error
   * across groups. `theCase` types the value at its own case, so this
   * walks a sum without a cast.
   */
  def label[I](i: I, depth: Int = Int.MaxValue)(using s: Schema[I]): String =
    path(i)(using s).take(depth).mkString("/")

  private def path[I](i: I)(using s: Schema[I]): List[String] = s match
    case su: Schema.SSum[I] =>
      su.theCase(i)([X <: I] => (n: String, sc: Schema[X], x: X) => n :: group(x)(using sc))
    case _ => Nil

  /**
   * Descend one level of the hierarchy.
   *
   * A GROUP is a case that exists only to name a group: exactly one
   * field, and that field is itself a taxonomy. A case whose single
   * field is a plain value is a LEAF — its fields are slots, not a
   * sub-taxonomy, and the walk stops. Both kernels used here type the
   * value at its own type (`theCase`, `eachField`), so the whole walk
   * takes no cast.
   */
  private def group[X](x: X)(using s: Schema[X]): List[String] = s match
    case p: Schema.SProduct[X] if p.fields.length == 1 =>
      p.eachField(x)([Y] => (_: String, sc: Schema[Y], y: Y) =>
        sc match
          case _: Schema.SSum[Y] => path(y)(using sc)
          case _ => Nil).headOption.getOrElse(Nil)
    case _ => Nil

  /** the taxonomy as the model is shown it — the SAME schema value the
   * reply is decoded with, rendered, so the two cannot drift apart */
  def taxonomy[I](using s: Schema[I]): String =
    Json.print(JsonSchema.of(s))

  /**
   * An EXAMPLE ANSWER, built from the schema rather than written
   * beside it.
   *
   * A JSON Schema tells a model what is legal; it does not show it
   * what to type. Measured in this lane: shown only the schema, a 4B
   * model wrote `"intent": "Proposal"` where the encoding wants
   * `{"Proposal": {...}}`, dropped `alts`, and merged `conf` into the
   * intent object — 20 of 24 replies undecodable on a bare prompt. The
   * same lesson had already appeared in `inDomainPrompt`, where a
   * schema for a two-field object came back as the schema itself.
   *
   * Optional fields are omitted (that is what optional means to a
   * reader), a list shows exactly one element, and a sum shows its
   * FIRST case — enough to fix the shape without implying the choice.
   *
   * It is a SHAPE, not a valid value, and the prompt says so: the leaf
   * placeholders do not survive a refined schema (`"..."` is not a
   * confidence and not an ISO-8601 date), because nothing generic can
   * invent a value that satisfies an arbitrary `SIso`. Callers who
   * want a valid example pass a real one through `prompt`'s
   * `examples` — which is also, measured, the far bigger win.
   */
  def example[A](using s: Schema[A]): String = Json.print(skeleton(s))

  private def skeleton(s: Schema[?]): Json = s match
    case Schema.SString | Schema.SChar => Json.JStr("...")
    case Schema.SBytes => Json.JStr("")
    case Schema.SInt | Schema.SLong | Schema.SDouble => Json.JNum(0)
    case Schema.SBool => Json.JBool(true)
    case Schema.SOption(of) => skeleton(of())
    case Schema.SList(of) => Json.JArr(Vector(skeleton(of())))
    case Schema.SVector(of) => Json.JArr(Vector(skeleton(of())))
    case Schema.SIso(under, _, _) => skeleton(under())
    case p: Schema.SProduct[?] =>
      Json.JObj(p.fields.collect {
        // an optional field is left out: showing it invites a null
        case (n, f) if !f().isInstanceOf[Schema.SOption[?]] => (n, skeleton(f()))
      })
    case su: Schema.SSum[?] =>
      // `.map` rather than a match on the Option: destructuring the
      // pair in a pattern loses exhaustivity against the existential
      // in `cases`, and a suppressed warning would be worse than this
      su.cases.headOption
        .map(c => Json.JObj(Vector((c._1, skeleton(c._2())))))
        .getOrElse(Json.JObj(Vector.empty))

  /**
   * The instruction for one message.
   *
   * Everything variable in it is generated: the taxonomy from
   * `Schema[I]`, the confidence vocabulary from the enum. Adding a
   * case to the taxonomy changes the prompt, the parser and the tool
   * declaration together or not at all.
   */
  def prompt[I](message: String, examples: List[(String, I)] = Nil)
               (using s: Schema[I]): String =
    val r = JsonSchema.of(reading[I](using s))
    val shown =
      if examples.isEmpty then ""
      else examples.map((m, i) => s"""  "$m" -> ${Json.write(i)(using s)}""")
        .mkString("Examples of the intent for a single-span message:\n", "\n", "\n\n")
    s"""Segment the message and classify the intent of each segment.
       |
       |Answer with ONE JSON object and nothing else, matching this schema:
       |${Json.print(r)}
       |
       |The SHAPE of an answer, with placeholder values you must replace
       |(and as many spans and alts as the message needs):
       |${Classify.example(using reading[I](using s))}
       |
       |Rules:
       |- One span per intent. A message carrying two intents has two spans.
       |- Within a span, list alternatives in `alts` best first; give the
       |  reason in `why` BEFORE them.
       |- `conf` is one of: ${Conf.vocabulary}.
       |- If nothing in the taxonomy fits, say so through its own case
       |  rather than choosing the nearest positive class.
       |
       |$shown""".stripMargin + s"Message: $message"

  /**
   * The in-domain gate: one yes/no question asked BEFORE the taxonomy.
   *
   * It exists because of a measurement, not a hunch. Asked to choose
   * among positive classes, a model chooses one — the out-of-domain
   * bucket collapsed to recall 0.17 (specs/intent-classify.md). A
   * separate binary question does not offer it that choice.
   *
   * REACH FOR IT SECOND. A later measurement over the same 120
   * messages found that naming the domain in the taxonomy's own case
   * names does the same work for free: `MeetingProposal` /
   * `NotAboutMeetings` scored macro F1 0.907 with `Other` F1 0.92 and
   * ONE call per message, against 0.906 / 0.86 for generic names with
   * this gate and two calls. Worse, the two do not compose — gating an
   * already-named taxonomy dropped it to 0.830, because a second judge
   * over-rejects what the first accepted. So: name the domain in the
   * type; use the gate when the taxonomy cannot be renamed (someone
   * else's types, a wire format, a taxonomy shared with a system that
   * has its own names).
   *
   * `why` precedes the verdict for the same reason it does in a span,
   * and the field order is what puts it there.
   */
  final case class InDomain(why: String, inDomain: Boolean)

  given Schema[InDomain] = Schema.derived

  def inDomainPrompt[I](message: String)(using s: Schema[I]): String =
    // an EXAMPLE VALUE, not the schema: shown a schema for a shape
    // this small, a model echoes the schema back with its answer
    // buried in `properties` — measured, and it cost a whole arm of
    // the Other-collapse experiment before it was seen
    val shape = Json.write(InDomain("one sentence saying why", true))
    s"""Decide whether the message is about any of the following at all.
       |
       |The subject matter, as a schema:
       |${Json.print(JsonSchema.of(s))}
       |
       |Answer with ONE JSON object and nothing else, of exactly this
       |shape (the same two fields, your own values):
       |$shape
       |
       |Answer `false` when the message is about something else entirely,
       |however polite, urgent or well-written it is.
       |
       |Message: $message""".stripMargin

  /** decode the gate's answer */
  def readInDomain(reply: String): Either[String, InDomain] =
    Json.decode(summon[Schema[InDomain]])(Json.parseValue(reply))

  /**
   * Decode a reply.
   *
   * The reply is assumed to BE the JSON — the same honest scope
   * `Structured` states for the streaming case, and for the same
   * reason: a heuristic that digs an object out of prose is a guess,
   * and a guess here silently changes what was classified.
   */
  def read[I](reply: String)(using s: Schema[Reading[I]]): Either[String, Reading[I]] =
    Json.decode(s)(Json.parseValue(reply))

  /** what a caller acts on */
  enum Decision[+I]:
    /** every span cleared the floor: its text and the intent to act on */
    case Act(spans: List[(String, I)])
    /** one span did not: ask, showing the alternatives to choose between */
    case Clarify[I](span: Span[I]) extends Decision[I]
    /** nothing was found to act on */
    case Empty

  /**
   * The gate. `Act` only when EVERY span cleared the floor: a message
   * whose second intent is a guess must not have its first acted on
   * silently, because the reply the caller sends will read as an
   * answer to the whole message.
   */
  def decide[I](r: Reading[I], floor: Conf = Conf.Medium): Decision[I] =
    if r.spans.isEmpty then Decision.Empty
    else
      val unsure = r.spans.find(sp => sp.alts.headOption.forall(a => !Conf.atLeast(a.conf, floor)))
      unsure match
        case Some(sp) => Decision.Clarify(sp)
        case None => Decision.Act(r.spans.flatMap(sp => sp.alts.headOption.map(a => (sp.text, a.intent))))
}
