package okay.dlm.remote

import okay.dlm.Judge

/**
 * LAYA — Convai Innovations' open System One model (Apache-2.0,
 * 322M parameters over mmBERT, `pip install "laya[serve]"`), served
 * from a container of one's own on `POST /v1/systemone`, the wire it
 * documents as identical to Jev's. What is Laya's here is the host
 * (a local one by default), the optional bearer key its server reads
 * as `LAYA_API_KEY`, and the name a verdict carries.
 *
 * The one thing a caller should know before plugging it in: the
 * source implementation READ Laya and declined it as the default
 * semantic tier (okay-chat specs/model.md §7) — rules take 85% of
 * verdicts with zero corrections and the probe was wrong twice in
 * 130, at 2.4 ms against 193–464 ms. It is here so the choice is the
 * operator's and measured, not the library's and assumed.
 */
object Laya:
  val name = "laya"
  val defaultBase = "http://127.0.0.1:8000"
  val keyVariable = "LAYA_API_KEY"

  def config(base: String = defaultBase, apiKey: Option[String] = None): SystemOne.Config =
    SystemOne.Config(name, base, apiKey)

  def client(base: String = defaultBase, apiKey: Option[String] = None)(using Wire): SystemOne.Client =
    SystemOne.Client(config(base, apiKey))

  def judge(base: String = defaultBase, apiKey: Option[String] = None,
            report: String => Unit = _ => (), timeoutMs: Long = 5000L)(using Wire): Judge =
    Judge.guarded(client(base, apiKey).judge(report), timeoutMs = timeoutMs,
      report = e => report(s"$name: ${e.getMessage}"))

  /** from the environment: `LAYA_BASE` names the server, or the local
   * default stands; the key is optional, as the server's is */
  def fromEnv(report: String => Unit = _ => ())(using Wire): Option[Judge] =
    sys.env.get("LAYA_BASE").filter(_.nonEmpty).map(b =>
      judge(b, sys.env.get(keyVariable).filter(_.nonEmpty), report))
