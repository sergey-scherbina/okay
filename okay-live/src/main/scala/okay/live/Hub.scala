package okay.live

import okay.{Channel, TList}

/**
 * Broadcast (specs/live.md): every `subscribe()` mints a fresh
 * channel and remembers it; `publish` offers a value to every
 * channel remembered so far. A subscriber added AFTER an earlier
 * publish never sees it — publish reaches only CURRENT subscribers.
 * A closed/abandoned subscriber's channel stays remembered until
 * process end — stated, not hidden; human-scale viewer counts do
 * not need eviction. Over `TList` (live-tdict), the cross-platform
 * single-cell list: `publish` walks the snapshot of the moment, which
 * is exactly the copy-on-write reading it had.
 */
final class Hub[A]:
  private val subscribers = TList.empty[Channel[A]]

  def subscribe(): Channel[A] =
    val c = Channel[A]()
    subscribers.append(c)
    c

  def publish(a: A): Unit =
    subscribers.snapshot.foreach(c => c.offer(a): Unit)
