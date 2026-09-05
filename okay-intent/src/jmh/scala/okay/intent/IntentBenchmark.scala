package okay.intent

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.rag.{Embedding, embedding}

/**
 * What a caller actually pays, measured (specs/intent-classify.md).
 *
 * This module quoted microseconds for every tier — 76us probe, 90us
 * centroid, 92us chargrams — and each was a `System.nanoTime` around
 * a loop inside a test: no warmup, no JIT accounting, one run, on
 * whatever the machine happened to be doing. By this repository's own
 * standard, kept in `src/jmh/history.tsv`, those are not
 * measurements. These are.
 *
 * Three questions a caller has, and the third had never been asked:
 * what does one message cost to classify, what does fitting cost, and
 * WHAT DOES LOADING THE SHIPPED MODEL COST — which every startup path
 * pays and nothing had timed.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class IntentBenchmark {

  /** a message of the shape the tiers were measured on */
  final val message = "Could you send me the agenda before Thursday's review?"

  /** deterministic synthetic rows, so the fit benchmarks state their
   * own size instead of depending on a fixture that lives in the test
   * scope and changes when a corpus lane lands */
  final val N = 60
  private val rows: Seq[(String, String)] = (0 until N).map { i =>
    if i % 2 == 0 then (s"could you send the thing number $i", "Request")
    else (s"shall we meet on day number $i", "Proposal")
  }
  private val vectors: Seq[(Embedding, String)] = rows.zipWithIndex.map { case ((_, c), i) =>
    val v = Array.fill(256)(0.0f)
    v(i % 256) = 1.0f
    v(255 - (i % 256)) = 0.5f
    (embedding(v), c)
  }

  private val grams = CharGrams.train(rows, dim = 1024)
  private val probe = Probe.train(vectors, epochs = 50)
  private val centroid = Centroid.train(vectors)
  private val nearest = Nearest.train(vectors)
  private val vector = vectors.head._1
  private val router = Router.Router.offline()
  private val modelJson = Fit.save(grams)

  // ── one message, one answer ────────────────────────────────────────

  @Benchmark def cues(): Option[String] =
    Patterns.classify(Models.cues, message, floor = 0.4)

  @Benchmark def charGrams(): Option[String] =
    CharGrams.score(Models.meeting, message).map(_.best)

  @Benchmark def centroidScore(): Option[String] =
    Centroid.score(centroid, vector).map(_.best)

  @Benchmark def probeScore(): Option[String] =
    Probe.score(probe, vector).map(_.best)

  @Benchmark def nearestScore(): Option[String] =
    Nearest.score(nearest, vector).map(_.best)

  /** the whole shipped door on a message the CUES answer — the cheap
   * path, and the common one */
  @Benchmark def routeOffline(): Router.Action = router.route(message)

  /** a message no cue fires for */
  final val silent = "The quarterly figures landed in the shared folder yesterday."

  /** and the door on THAT, which falls through to the model. It has
   * two costs, and quoting one of them would be the same half-truth
   * this lane exists to remove. */
  @Benchmark def routeOfflineNoCue(): Router.Action = router.route(silent)

  // ── fitting, at a stated size ──────────────────────────────────────

  @Benchmark def fitCharGrams(): CharGrams.Trained = CharGrams.train(rows, dim = 1024)
  @Benchmark def fitCentroid(): Centroid.Trained = Centroid.train(vectors)
  @Benchmark def fitProbe(): Probe.Trained = Probe.train(vectors, epochs = 50)

  // ── loading, which every startup pays and nobody had timed ─────────

  @Benchmark def loadShippedModel(): CharGrams.Trained =
    Fit.grams(modelJson).getOrElse(sys.error("the shipped model did not decode"))
}
