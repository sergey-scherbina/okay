package okay.cache

import okay.{!, Async, async}
import okay.persist.{Record, Topic}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/**
 * Regime 1 (specs/cache.md): for data whose truth lives in
 * okay-persist, the invalidation problem does not need solving — a
 * cache over a log is a CONSUMER, and a consumer is never invalid,
 * only BEHIND, by a measurable amount. `latest` is the fold's
 * state at the consumed offset; `lag` is end minus consumed — the
 * same number as consumer lag, because it is consumer lag;
 * invalidation is `refresh`, which is just reading on.
 *
 * Pairs the persist stage-1 machinery: over a COMPACTED keyed
 * topic a cold rebuild refolds only what compaction kept, and
 * agrees with the warm view — the snapshot story, told as a cache.
 * A fold answering None is the tombstone: the key leaves the view.
 */
trait View[K, V]:
  /** the fold's current state for this key — serves without
   * touching the log; how behind it is, `lag` says */
  def latest(k: K): Option[V] ! Async

  /** end minus consumed, summed over partitions: THE staleness
   * number, and it is consumer lag */
  def lag: Long

  /** reading on — the whole of regime-1 "invalidation" */
  def refresh(): Unit ! Async

object View:

  /** the fold of a (compacted) keyed topic; `key` renders a lookup
   * key the way the producer rendered record keys */
  def apply[K, V](topic: Topic)(key: K => Array[Byte])
                 (fold: (Option[V], Record) => Option[V]): View[K, V] =
    new Folded[K, V](topic, key, fold)

  private final class Folded[K, V](topic: Topic, keyOf: K => Array[Byte],
                                   fold: (Option[V], Record) => Option[V])
    extends View[K, V]:

    private val state = mutable.HashMap.empty[ArraySeq[Byte], V]
    private val consumed = Array.tabulate(topic.partitions)(topic.begin)

    private def advance(): Unit = synchronized {
      var p = 0
      while p < topic.partitions do
        var going = true
        while going do
          topic.read(p, consumed(p), 256) match
            case Topic.Read.TooEarly(b) => consumed(p) = b
            case Topic.Read.Records(rs) =>
              if rs.isEmpty then going = false
              else
                for r <- rs do
                  val k = ArraySeq.unsafeWrapArray(r.key)
                  fold(state.get(k), r) match
                    case Some(v) => state.put(k, v)
                    case None => state.remove(k)  // the tombstone
                  consumed(p) = r.offset + 1
        p += 1
    }

    def latest(k: K): Option[V] ! Async =
      async(synchronized(state.get(ArraySeq.unsafeWrapArray(keyOf(k)))))

    def lag: Long = synchronized {
      (0 until topic.partitions).map(p => math.max(0L, topic.end(p) - consumed(p))).sum
    }

    def refresh(): Unit ! Async = async(advance())
