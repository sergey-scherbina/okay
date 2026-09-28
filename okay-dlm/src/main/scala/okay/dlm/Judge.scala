package okay.dlm

import okay.rag.Embedding

/**
 * THE JUDGE, AS A SEAM (specs/dlm.md, "Backends").
 *
 * One typed question — which of these options is this text — answered
 * with a probability over every option and, where the judge has one,
 * a confidence. Every head asks exactly this question of its own
 * options; the router's vector layer asks it of the intents that
 * opted in. Models are judges here, never authors: a judge picks
 * among classes the caller named, or abstains, and cannot produce a
 * sentence, a fact or a class that does not exist.
 *
 * OURS BY DEFAULT: a linear probe over a table of exemplars
 * (`Judge.probe`), fitted at boot in milliseconds, no network, a
 * price of nothing per call. A remote "System One" model — TypeSafe's
 * Jev, Convai's Laya — answers the same question over the wire with a
 * calibrated confidence beside the probabilities (`okay-dlm-remote`),
 * and plugs in as a `given Judge.Fit` without a line of the model
 * changing.
 *
 * What a judge answers is a `Choice`; how sure it must be before a
 * head acts on it is the head's `margin`, so the same judge serves at
 * two bars in two places.
 */
trait Judge:
  /** who decided — written beside the verdict, so a replay and a
   * calibration know which judge a number came from */
  def name: String
  /** the answer, or `None` when this judge cannot answer this question
   * — a table with none of the options, a wire that did not reply */
  def choose(text: String, question: Judge.Question): Option[Judge.Choice]

object Judge:

  /**
   * One typed question. `options` are the classes and, for a judge
   * that reads words, what each means — a remote judge sends the
   * descriptions, ours ignores them and answers from its exemplars.
   * `instructions` is the question itself in the caller's words,
   * empty where the options speak for themselves.
   */
  final case class Question(options: Vector[(String, String)], instructions: String = ""):
    def names: Vector[String] = options.map(_._1)

  object Question:
    def of(names: Seq[String], instructions: String = ""): Question =
      Question(names.toVector.map(_ -> ""), instructions)

  /**
   * What a judge says: the best option, every option's probability
   * best first, and a confidence where the judge is calibrated to
   * have one — `None` from ours, which owes a margin and nothing more.
   */
  final case class Choice(probabilities: Vector[(String, Double)], confidence: Option[Double] = None):
    def best: String = probabilities.head._1
    def probability: Double = probabilities.head._2
    def runnerUp: Option[String] = probabilities.lift(1).map(_._1)
    /** the gap between the top two: a message equally close to two
     * classes is ambiguous however confident the winner looks */
    def margin: Double = probability - probabilities.lift(1).map(_._2).getOrElse(0.0)

  object Choice:
    /** ranked, best first, from any order */
    def of(probabilities: Iterable[(String, Double)], confidence: Option[Double] = None): Option[Choice] =
      val ranked = probabilities.toVector.sortBy(-_._2)
      Option.when(ranked.nonEmpty)(Choice(ranked, confidence))

  /**
   * HOW A JUDGE IS MADE FOR A TABLE — the configuration seam. Ours
   * fits a probe over the exemplars; a remote one ignores the table
   * and answers the question from its own weights. `Dlm.of` and every
   * head factory summon this, so the choice is one `given` at the
   * composition root and nothing else moves.
   */
  trait Fit:
    def apply(exemplars: Exemplars): Judge

  object Fit:
    /** OURS BY DEFAULT: the probe over the table, with the encoder in scope */
    given ours(using e: Embedder): Fit = exemplars => probe(exemplars, e)
    /** a judge that does not read the table: the remote ones */
    def constant(j: Judge): Fit = _ => j
    def apply(f: Exemplars => Judge): Fit = f(_)

  /**
   * OURS: multinomial logistic regression over frozen embeddings
   * (`okay.intent.Probe`), fitted at construction from the table and
   * answering in a few dot products. Restricted to the options ASKED
   * and renormalised: a head that asks about three of five classes
   * gets probabilities over three.
   */
  def probe(exemplars: Exemplars, embed: String => Embedding, name: String = "probe"): Judge =
    val n = name
    new Judge:
      val name = n
      private lazy val fitted: Option[okay.intent.Probe.Trained] =
        Option.when(exemplars.rows.nonEmpty)(okay.intent.Probe.train(exemplars.labelled))
      def choose(text: String, question: Question): Option[Choice] =
        fitted.flatMap { t =>
          val asked = question.names.toSet
          val ranked = okay.intent.Probe.ranked(t, embed(text)).filter((c, _) => asked.isEmpty || asked(c))
          val total = ranked.map(_._2).sum
          Choice.of(if total > 0 then ranked.map((c, p) => c -> p / total) else ranked)
        }

  /** the same, with the encoder from scope */
  def probe(exemplars: Exemplars)(using e: Embedder): Judge = probe(exemplars, e, "probe")

  /** a judge that never answers: what a head with nothing behind it holds */
  val silent: Judge = new Judge:
    val name = "silent"
    def choose(text: String, question: Question): Option[Choice] = None

  /** the first that answers: a remote judge with ours behind it, so a
   * wire that is down costs a fallback and not an outage */
  def orElse(first: Judge, second: Judge): Judge = new Judge:
    val name = s"${first.name}|${second.name}"
    def choose(text: String, question: Question): Option[Choice] =
      first.choose(text, question).orElse(second.choose(text, question))

  /**
   * THE DOOR'S DISCIPLINE AROUND A JUDGE THAT LEAVES THE PROCESS: a
   * throw or a timeout is a strike, `retireAfter` strikes running
   * retire it for `cooldownMs` and it answers `None` meanwhile, so a
   * provider with no credits does not cost every turn its timeout.
   * The same rule `ModelChain` keeps for a lane, because a network
   * classifier fails exactly as a network generator does.
   */
  def guarded(judge: Judge, timeoutMs: Long = 5000L, retireAfter: Int = 3,
              cooldownMs: Long = 5 * 60 * 1000L, now: () => Long = () => System.currentTimeMillis(),
              report: Throwable => Unit = _ => ()): Judge = new Judge:
    val name = judge.name
    private val strikes = java.util.concurrent.atomic.AtomicInteger(0)
    @volatile private var retiredUntil = 0L
    private val pool = java.util.concurrent.Executors.newCachedThreadPool(r => {
      val t = Thread(r, s"judge-${judge.name}"); t.setDaemon(true); t
    })
    def choose(text: String, question: Question): Option[Choice] =
      if retiredUntil > now() then None
      else
        val fut = pool.submit[Option[Choice]](() => judge.choose(text, question))
        try
          val got = fut.get(timeoutMs, java.util.concurrent.TimeUnit.MILLISECONDS)
          strikes.set(0)
          got
        catch
          case e: Throwable =>
            fut.cancel(true)
            report(Option(e.getCause).getOrElse(e))
            if strikes.incrementAndGet() >= retireAfter then
              retiredUntil = now() + cooldownMs
              strikes.set(0)
            None
