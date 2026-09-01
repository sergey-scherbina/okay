package okay.cache

import okay.{!, Async}

/**
 * Regime 2, the write path (specs/cache.md): truth lives in a
 * foreign system and writes flow through us, so INSIDE the same code
 * path that commits — and strictly AFTER the commit — the entry is
 * invalidated. The ordering is the whole point: invalidate-first
 * lets a concurrent reader re-load the PRE-commit value and cache it
 * past the commit, resurrecting the old truth indefinitely.
 *
 * The honest window is stated, not hidden: between their COMMIT and
 * our invalidate, a reader can still be served the old value.
 * Write-through shrinks that window to microseconds; nothing
 * eliminates it without their transaction knowing about us. (Across
 * processes the invalidation becomes an EVENT on a persist topic —
 * regime 2's stage-2 story, not this helper's.)
 */
object WriteThrough:

  /** run the committing write, then invalidate — the order held by
   * construction, so a review points here instead of auditing call
   * sites */
  def write[K, V, A](cache: Cache[K, V], k: K)(commit: => A ! Async): A ! Async =
    commit.flatMap(a => cache.invalidate(k).map(_ => a))
