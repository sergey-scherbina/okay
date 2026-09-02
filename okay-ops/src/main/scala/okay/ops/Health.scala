package okay.ops

import okay.persist.Store

/**
 * Two booleans with a reason (specs/persist.md, Operations; this
 * spec, specs/ops.md): liveness answers "does this store respond at
 * all", readiness "is it past recovery and serving" — the same
 * question for a `Store` that opens synchronously, since opening IS
 * running the recovery scan. Computed by CALLING the store, never a
 * cached flag: a probe that answers from a stale bit is worse than
 * one that costs one method call.
 */
final case class Health(live: Boolean, ready: Boolean, reason: Option[String] = None)

object Health:
  def of(store: Store): Health =
    try
      store.stats
      Health(live = true, ready = true)
    catch case e: Throwable =>
      Health(live = false, ready = false, reason = Some(
        Option(e.getMessage).getOrElse(e.getClass.getSimpleName)))
