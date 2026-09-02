package okay.live

import okay.Channel

/**
 * A channel per key (specs/live.md): `apply(key)` creates one
 * lazily on first use and reuses it after — the same key always
 * answers the same channel. No removal — same honest limit as
 * `Hub`; a real eviction need is a BACKLOG item, not a speculative
 * build.
 */
final class Registry[K, A]:
  private val channels = java.util.concurrent.ConcurrentHashMap[K, Channel[A]]()

  def apply(key: K): Channel[A] =
    channels.computeIfAbsent(key, _ => Channel[A]())
