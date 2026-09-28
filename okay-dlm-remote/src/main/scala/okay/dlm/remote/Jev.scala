package okay.dlm.remote

import okay.dlm.Judge

/**
 * JEV — TypeSafe AI's hosted System One model (typesafe.ai): typed
 * decisions with calibrated probabilities, no text. The wire is
 * `SystemOne`'s; what is Jev's is the host, the key, and the name a
 * verdict carries in the journal.
 *
 * The host below is where the vendor's documentation of 2026-09
 * points and is a `Config` field, not a fact this module vouches for:
 * an early-access product moves. `TYPESAFE_API_KEY` is the variable
 * the vendor's own SDK reads.
 */
object Jev:
  val name = "jev"
  val defaultBase = "https://api.typesafe.ai"
  val keyVariable = "TYPESAFE_API_KEY"

  def config(apiKey: String, base: String = defaultBase): SystemOne.Config =
    SystemOne.Config(name, base, Some(apiKey))

  def client(apiKey: String, base: String = defaultBase)(using Wire): SystemOne.Client =
    SystemOne.Client(config(apiKey, base))

  /** the judge, guarded like any door that leaves the process: a run
   * of failures retires it for a while and the model falls back to
   * whatever stands behind it */
  def judge(apiKey: String, base: String = defaultBase, report: String => Unit = _ => (),
            timeoutMs: Long = 5000L)(using Wire): Judge =
    Judge.guarded(client(apiKey, base).judge(report), timeoutMs = timeoutMs,
      report = e => report(s"$name: ${e.getMessage}"))

  /** from the environment, or `None` and the model stays ours */
  def fromEnv(report: String => Unit = _ => ())(using Wire): Option[Judge] =
    sys.env.get(keyVariable).filter(_.nonEmpty).map(k =>
      judge(k, sys.env.getOrElse("TYPESAFE_BASE", defaultBase), report))
