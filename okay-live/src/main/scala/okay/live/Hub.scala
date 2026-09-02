package okay.live

import okay.Channel

/**
 * Broadcast (specs/live.md): every `subscribe()` mints a fresh
 * channel and remembers it; `publish` offers a value to every
 * channel remembered so far. A subscriber added AFTER an earlier
 * publish never sees it — publish reaches only CURRENT subscribers.
 * A closed/abandoned subscriber's channel stays remembered until
 * process end — stated, not hidden; human-scale viewer counts do
 * not need eviction.
 */
final class Hub[A]:
  private val subscribers = java.util.concurrent.CopyOnWriteArrayList[Channel[A]]()

  def subscribe(): Channel[A] =
    val c = Channel[A]()
    subscribers.add(c)
    c

  def publish(a: A): Unit =
    subscribers.forEach(c => c.offer(a): Unit)
