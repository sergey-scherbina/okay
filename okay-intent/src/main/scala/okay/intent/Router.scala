package okay.intent

import okay.frame.Frame
import okay.rag.Embedding

/**
 * One message in, one action out — the composed door
 * (specs/intent-classify.md).
 *
 * Nine tiers were measured here and the ORDER they should be consulted
 * in was measured too, and then it lived in one file in `okay-demo`.
 * A caller outside that demo had to re-derive it by reading twenty
 * Results sections: cues first because they are 90.6% right where they
 * fire and cost nothing, the vector tier next because it answers
 * everything at 85-88% but needs an embedder, the shipped n-gram model
 * last because it needs nothing at all and is right about six times in
 * ten. Under the last margin nobody guesses — a person sees the
 * candidates.
 *
 * That order is what this object is. It adds no new classification;
 * it is composition, and the reason it is worth a type is that every
 * one of those sentences is a MEASUREMENT that a caller would
 * otherwise have to rediscover.
 *
 * The four outcomes are the demo's, which had a year of nothing else
 * to be: act, ask one question, escalate to a person, and — the one
 * that is easy to leave out — say WHY it escalated.
 */
object Router {

  /** what a router does with a message, rather than what it thinks */
  enum Action:
    /** enough is known to act, and the frame says so */
    case Act(intent: String, frame: Frame[String])
    /** the class is known and a slot is not: ask THIS, in their
     * language, and say how many more there are — a caller should not
     * have to count, and a person deserves to know */
    case Ask(intent: String, slot: String, question: String, remaining: Int)
    /** not confident enough to act: show the alternatives to a person.
     * `why` is not decoration — "nothing fired and no model is loaded"
     * and "the top two are inside the margin" call for different
     * fixes, and a caller that cannot tell them apart fixes neither */
    case Escalate(candidates: Seq[String], why: String)

  /**
   * Where each tier stops guessing.
   *
   * Defaults are the measured ones, not round numbers: 0.4 is where
   * the cue tier's precision holds and 0.02 is the centroid margin the
   * bake-off settled on.
   *
   * `grams` defaults to ZERO — the shipped model answers whatever it
   * has — and that is a measurement rather than laziness, twice over.
   *
   * On held-out English the margin barely separates right from wrong:
   * raising the floor from 0.0 to 0.5 lifts precision among answered
   * messages from 76.7% to 83.7% while coverage falls from 60/60 to
   * 43/60, about four tenths of a point per abstention.
   *
   * And on the case a floor exists for, it does not work at all.
   * Margins on NONSENSE ("zzz qqq xxx", "asdf", "qwerty uiop") run
   * 0.13 to 0.89, median 0.437, against median 0.434 on real
   * messages — the model is exactly as confident about garbage as
   * about English. No threshold separates them, so a non-zero floor
   * here buys the LOOK of caution and none of it.
   *
   * WHAT TO DO INSTEAD, if a caller needs abstention: leave `grams`
   * out of the router entirely, and the tier below is a person; or use
   * `NoModel`, whose threshold is a conformal quantile with a promise
   * attached rather than a margin someone picked.
   */
  final case class Floors(cue: Double = 0.4, vector: Double = 0.02,
                          grams: Double = 0.0)

  /**
   * The router: a taxonomy, whichever tiers the caller has, and the
   * frame each class needs.
   *
   * `frames` defaults to a frame with no slots, so a caller who wants
   * a CLASS and nothing else gets `Act` immediately rather than having
   * to describe forms it does not have.
   *
   * Built through `Router.of`, which checks that every tier speaks the
   * taxonomy — a model fitted on other names is the silent
   * disagreement this door exists to prevent.
   */
  final case class Router private (taxon: Taxon,
                                   cues: Option[Patterns.Cues],
                                   grams: Option[CharGrams.Trained],
                                   vectors: Option[(Centroid.Trained, String => Embedding)],
                                   frames: String => Frame[String],
                                   floors: Floors):

    /** the tiers this router can actually consult, in order, for a
     * caller that wants to know what it is holding */
    def tiers: Vector[String] =
      Vector("cues" -> cues.isDefined, "vectors" -> vectors.isDefined,
        "grams" -> grams.isDefined).collect { case (n, true) => n }

    def route(message: String): Action =
      decide(message) match
        case Left((candidates, why)) => Action.Escalate(candidates, why)
        case Right(intent) =>
          // fill from the message BEFORE asking: "Are you free
          // Wednesday afternoon?" carries its own `when`, and a router
          // that asks for it is asking the person who just said
          val frame = frames(intent).fillFrom(message)
          frame.missing.headOption match
            case Some((slot, question)) =>
              Action.Ask(intent, slot, question, frame.remaining)
            case None => Action.Act(intent, frame)

    /** the class, or the candidates and the reason nobody chose */
    private def decide(message: String): Either[(Seq[String], String), String] =
      cues.flatMap(c => Patterns.classify(c, message, floors.cue)) match
        case Some(cls) => Right(cls)
        case None => byVector(message).orElse(byGrams(message)).getOrElse(Left(nothing))

    private def byVector(message: String): Option[Either[(Seq[String], String), String]] =
      vectors.flatMap { (model, embed) =>
        Centroid.score(model, embed(message)).map {
          case v if v.margin >= floors.vector => Right(v.best)
          case v => Left((Seq(v.best) ++ v.runnerUp,
            f"the vector tier's top two are within ${floors.vector}%.2f"))
        }
      }

    private def byGrams(message: String): Option[Either[(Seq[String], String), String]] =
      grams.flatMap { model =>
        CharGrams.score(model, message).map {
          case v if v.margin >= floors.grams => Right(v.best)
          case v => Left((Seq(v.best) ++ v.runnerUp,
            f"the model's top two are within ${floors.grams}%.2f"))
        }
      }

    /** nothing answered at all, which is a different report from a
     * close call and usually a different fix */
    private def nothing: (Seq[String], String) =
      if tiers.isEmpty then (Seq.empty, "this router holds no tier that could answer")
      else (Seq.empty, s"no tier answered: ${tiers.mkString(", ")}")

  object Router:
    /**
     * Every tier must speak the taxonomy.
     *
     * A cue set carries its own `Taxon` and can be checked outright; a
     * fitted model carries only the class names it was trained on, so
     * what is checked is that they are all in the taxonomy. A model
     * that knows FEWER classes is legal — a taxonomy may hold a class
     * nothing has learnt yet — and `silent` is where a caller sees it.
     */
    def of(taxon: Taxon,
           cues: Option[Patterns.Cues] = None,
           grams: Option[CharGrams.Trained] = None,
           vectors: Option[(Centroid.Trained, String => Embedding)] = None,
           frames: String => Frame[String] = i => Frame.of(i),
           floors: Floors = Floors()): Either[String, Router] =
      def stray(what: String, names: Seq[String]) =
        val bad = names.distinct.filterNot(taxon.has)
        Option.when(bad.nonEmpty)(s"$what names classes not in the taxonomy: ${bad.sorted.mkString(", ")}")
      val problems = Vector(
        cues.flatMap(c => stray("the cues", c.all.map(_.cls))),
        grams.flatMap(g => stray("the model", g.classes)),
        vectors.flatMap((m, _) => stray("the centroids", m.byClass.keys.toSeq))).flatten
      if problems.nonEmpty then Left(problems.mkString("; "))
      else Right(new Router(taxon, cues, grams, vectors, frames, floors))

    /**
     * The one that needs nothing: the shipped cue set and the shipped
     * model, over the taxonomy they were both built against.
     *
     * 76.7% at full coverage on held-out English, no network, no
     * embedder, no fitting — see `Models` for exactly what that number
     * is and is not.
     */
    def offline(frames: String => Frame[String] = i => Frame.of(i),
                floors: Floors = Floors()): Router =
      of(Patterns.canonical, Some(Models.cues), Some(Models.meeting),
        frames = frames, floors = floors)
        .fold(m => throw new IllegalStateException(s"the shipped tiers disagree: $m"), identity)
}
