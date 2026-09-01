package okay.cache

import okay.{!, Async, pure}
import okay.persist.{Ack, Topic}

/**
 * Cross-node regime 2 (specs/cache.md stage 2): the invalidation is
 * an EVENT on a persist topic — every node's cache a consumer, so
 * audit and replay come free and no second bus is invented. The
 * named trade against Redis pub/sub, restated where the code is:
 * pub/sub is fire-and-forget (a disconnected node misses the event
 * and serves stale until budget or reconnect); the TOPIC replays —
 * a node that was down drains from its offset and CONVERGES.
 */
object Invalidations {

  /** node A's write path: the key, as the event (WriteThrough first,
   * then append — the same ordering argument, one seam further out) */
  def append(topic: Topic, key: String, ack: Ack = Ack.Durable): Long =
    topic.append(0, key.getBytes("UTF-8"), Array.empty, ack)

  /**
   * node B's read path: consume [from, end), invalidate locally,
   * answer the next offset — the caller paces (a loop, a wakeup),
   * and the offset journaled by the caller is what makes a restart
   * converge instead of guessing.
   */
  def drain[K, V](topic: Topic, cache: Cache[K, V], keyOf: String => K,
                  from: Long, max: Int = 512): Long ! Async =
    topic.read(0, from, max) match
      case Topic.Read.TooEarly(begin) => drain(topic, cache, keyOf, begin, max)
      case Topic.Read.Records(rs) =>
        def go(rest: List[okay.persist.Record]): Long ! Async = rest match
          case Nil => pure(rs.lastOption.map(_.offset + 1).getOrElse(from))
          case r :: more =>
            cache.invalidate(keyOf(String(r.key, "UTF-8"))).flatMap(_ => go(more))
        go(rs.toList)
}
