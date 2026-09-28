package okay.dlm

/**
 * A DETERMINISTIC DIALOGUE LANGUAGE MODEL, as one value (specs/dlm.md).
 *
 * Not a network and not a generator: one function
 *
 *     (state, utterance) -> (state, action, what to say)
 *
 * assembled from deterministic layers — rules, then typos of a rule's
 * word, then centroids over a frozen encoder, then what the person
 * taught — and from heads that answer one question each. Its
 * "weights" are tables of exemplars compiled from phrases a person
 * wrote; its learning is a person in the loop; its proof is a replay
 * from a journal that reaches the same decisions.
 *
 * What this value holds is the MECHANISM. Every word it says, every
 * rule it routes by, every phrase it was compiled from and every slot
 * it reads is the caller's data, passed in: the boundary is the point.
 *
 * @param intents  the authored set the router reads
 * @param router   which intent, and why
 * @param heads    one question each, by the caller's own name for it
 *                 ("acts", "answers", "presence", "frames", …)
 * @param detector which language, from the authored phrasings
 * @param phrasing the caller's read-backs and narrowing questions
 * @param confirm  yes, no, or tell me more — read by rule
 */
final case class Dlm(intents: Intents,
                     router: Router,
                     heads: Map[String, Head] = Map.empty,
                     detector: Option[Language.Detector] = None,
                     phrasing: Phrasing = Phrasing.empty,
                     confirm: Option[Confirm] = None):

  /** a head by name; one that never answers where none was given, so
   * a caller reads one signal and not two */
  def head(name: String): Head = heads.getOrElse(name, Head.off)

  /** which layers are live — what a banner says at boot */
  def tiers: Vector[String] =
    Vector("rules", "typos") ++
      Option.when(router.semantic)("vectors") ++
      heads.collect { case (n, h) if h.live => n } ++
      detector.filter(_.nonEmpty).map(_ => "language")

object Dlm:

  /** the rule layer alone: a supported deployment, and what every
   * test that needs no encoder builds */
  def rules(intents: Intents): Dlm = Dlm(intents, Router(intents))

  /**
   * THE WHOLE OF IT, AS THE SCOPE SAYS. The encoder and the judge come
   * from the givens in scope — ours by default, so this line with
   * nothing imported is a model that needs nothing and reaches no
   * network; a `given Embedder` for a model on disk and a `given
   * Judge.Fit` for a remote judge change what every head and the
   * router's vector layer run on, and nothing else moves.
   *
   * One encoder for the router and every head, because two tables
   * fitted by different encoders is a mismatch nobody notices until a
   * live turn — and a table compiled by another encoder than the one
   * in scope is refused by name, not read.
   */
  def of(intents: Intents,
         exemplars: Option[Exemplars] = None,
         heads: Map[String, (Exemplars, Float)] = Map.empty,
         margin: Float = 0.5f,
         alphabet: Alphabet = Alphabet.none,
         nearBar: Option[Float] = None,
         detector: Option[Language.Detector] = None)
        (using e: Embedder, fit: Judge.Fit): Either[String, Dlm] =
    val foreign = (exemplars.toVector ++ heads.values.map(_._1)).filter(x => x.rows.nonEmpty && x.encoder != e.name)
    if foreign.nonEmpty then
      Left(s"a table compiled by «${foreign.head.encoder}», this model runs «${e.name}» — refused")
    else Right(Dlm(intents,
      Router.of(intents, exemplars, margin = margin, nearBar = nearBar, alphabet = alphabet),
      heads.map((n, h) => n -> Head.of(Some(h._1), h._2)),
      Some(detector.getOrElse(Language.Detector.of(intents.byLang, alphabet = alphabet)))))
