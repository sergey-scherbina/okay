package okay.intent

/**
 * A model you can actually load (specs/intent-classify.md).
 *
 * Everything in this module was measured and nothing was SHIPPED:
 * every fitted tier existed inside the test that fitted it, so a
 * caller had the types, the accuracy tables and no way to get from a
 * pile of messages to a working classifier without reading twenty
 * Results sections first.
 *
 * This is the one artifact that can ship. The vector tiers need an
 * embedder — a gateway on the network or a distilled table somebody
 * has to build — and the cue tier needs nothing but is not fitted at
 * all. `CharGrams` is the tier in between: hashed character n-grams
 * over the text itself, fitted here, serialised here, and decoded at
 * load with no network, no gateway and no fitting on the startup path.
 *
 * WHAT IT IS, exactly, because a model with a vague provenance is
 * worse than none:
 *
 *   - fitted on 60 author-written ENGLISH messages, the odd half of
 *     this repository's own fixture
 *   - over four meeting classes: `Proposal`, `Request`,
 *     `Notification`, `Other`
 *   - 61.7% on the 60 held-out messages ALONE, which is why it is not
 *     offered alone
 *   - 76.7% at FULL COVERAGE behind the cue tier — cues answer the
 *     53% they fire on at 90.6%, this answers the rest at 61%
 *
 * WHAT IT IS NOT: a general intent model, and not multilingual. A fit
 * over all six languages of the fixture scores 33-67% per language on
 * fifteen held-out rows each — too thin to stand behind, and it costs
 * English three points to buy. `intent-language-fixture-growth` is the
 * lane that would change that.
 *
 * For a caller's own corpus: `Fit.grams(rows)`, `Fit.save`,
 * `Fit.grams(json)`. This object is what that path produces, run once
 * and committed.
 */
object Models {

  /**
   * The shipped classifier, decoded on first use.
   *
   * A `lazy val` because decoding is real work — 43KB of JSON and a
   * matrix — and a caller that only wants the cue tier should not pay
   * for it. It is also why the artifact is a string in a generated
   * source rather than a classpath resource: this module is
   * cross-built, and a resource is a JVM-only way to load a model into
   * something whose whole claim is that it needs nothing.
   */
  lazy val meeting: CharGrams.Trained =
    Fit.grams(MeetingModel.json).fold(
      // unreachable unless the generated artifact was hand-edited: it
      // is produced by `MakeModel` and its round trip is a test
      m => throw new IllegalStateException(s"the shipped model did not decode: $m"),
      identity)

  /** the taxonomy the shipped model speaks, so a caller can rename it
   * onto their own with `Cues.renamed` rather than by hand */
  val taxon: Taxon = Patterns.canonical

  /** the cue tier that needs nothing at all, beside the model that
   * answers where the cues are silent */
  val cues: Patterns.Cues = Patterns.meeting
}
