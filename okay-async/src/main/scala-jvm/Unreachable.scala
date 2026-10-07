package okay


import okay.freer.given
import java.lang.ref.Cleaner

/** the collector as the last door of a release (abandoned-lazylist-
 * releases-nothing): a `java.lang.ref.Cleaner` runs `action` once the
 * object nobody references any more has been found — on its own
 * daemon thread, after a GC, not at a time anyone chooses. The action
 * must not reference the object, or it never becomes unreachable. */
private[okay] object Unreachable:
  private val cleaner = Cleaner.create()
  def onCollected(o: AnyRef, action: () => Unit): Unit =
    cleaner.register(o, () => action()): Unit
