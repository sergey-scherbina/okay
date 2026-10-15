package okay

/**
 * A GUARDED OPERATION'S RELEASE, REACHABLE WITHOUT RUNNING IT
 * (drive-discontinue, 2026-09-26). The callback drive, cancelled between
 * two operations, stops before the next one, and the scope that forwarded
 * that operation would never continue. The drive therefore asks the next
 * operation to `discontinue`: release the scopes it was guarded by,
 * innermost first, and run NO user code. This is OCaml 5's
 * `discontinue`, done by the runner rather than by a handler's author.
 */
trait Discontinue:
  def discontinue(): Unit

