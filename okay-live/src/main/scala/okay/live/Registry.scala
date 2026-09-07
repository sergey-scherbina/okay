package okay.live

import okay.{Channel, TDict}

/**
 * A channel per key (specs/live.md): `apply(key)` creates one
 * lazily on first use and reuses it after — the same key always
 * answers the same channel. No removal — same honest limit as
 * `Hub`; a real eviction need is a BACKLOG item, not a speculative
 * build.
 *
 * Over `TDict` (live-tdict), the cross-platform single-cell dict:
 * every racer on a missing key observes the one winning channel.
 * `TDict.computeIfAbsent`'s stated cost applies here — a CAS loser
 * has already built its `Channel()` before losing, and that channel
 * is discarded — and it is paid only on a first-use race for one
 * key, never on the steady state.
 */
final class Registry[K, A]:
  private val channels = TDict.empty[K, Channel[A]]

  def apply(key: K): Channel[A] =
    channels.computeIfAbsent(key)(Channel[A]())
