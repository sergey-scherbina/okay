package okay.dlm

import okay.rag.Embedding

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

  /** the whole of it, over one encoder: the same `embed` for the
   * router and every head, because two artifacts fitted by different
   * encoders is a mismatch nobody notices until a live turn */
  def of(intents: Intents,
         embed: String => Embedding,
         exemplars: Option[Exemplars] = None,
         heads: Map[String, (Exemplars, Float)] = Map.empty,
         margin: Float = 0.5f,
         alphabet: Alphabet = Alphabet.none,
         nearBar: Option[Float] = None): Dlm =
    Dlm(intents,
      Router(intents, exemplars, Some(embed), margin = margin, nearBar = nearBar, alphabet = alphabet),
      heads.map((n, e) => n -> Head(Some(e._1), Some(embed), e._2)),
      Some(Language.Detector.of(intents.byLang, alphabet = alphabet)))
