package okay.cache

import okay.{!, Async, async}
import scala.collection.mutable

/**
 * Caching with NAMED invalidation (specs/cache.md): every cache
 * states at construction where its truth lives and how wrong it may
 * be — there is no default TTL, and the absence of that feature is
 * the feature. `Budget(n)` is regime 3, the declared staleness the
 * business signed off on; `Invalidated` is regime 2, write-through
 * owns correctness; regime 1 (the log-fed view) is not a Regime
 * value at all — a consumer is never invalid, only behind
 * (cache-view, over okay-persist).
 */
enum Regime:
  case Budget(ttlMillis: Long)
  case Invalidated

trait Cache[K, V]:
  def get(k: K): Option[V] ! Async
  def put(k: K, v: V): Unit ! Async
  def invalidate(k: K): Unit ! Async

  /** the only read most callers should use: on a miss ONE load per
   * key runs (single-flight, per process); concurrent callers await
   * that load's result instead of dogpiling the source */
  def getOrLoad(k: K)(load: K => V ! Async): V ! Async

  /** hits, misses, loads, evictions, size — plain values, the
   * persist Stats precedent: an endpoint, a log line and a test
   * assertion are the same thing */
  def stats: Cache.Stats

object Cache:

  final case class Stats(hits: Long, misses: Long, loads: Long,
                         evictions: Long, size: Int)

  /** bounded ALWAYS — an unbounded cache is a leak with an alibi;
   * and no construction without a named Regime */
  def memory[K, V](regime: Regime, maxEntries: Int): Cache[K, V] =
    memory(regime, maxEntries, () => System.currentTimeMillis())

  /** the clock is injectable so a budget's expiry is a fast test,
   * not a sleep */
  private[cache] def memory[K, V](regime: Regime, maxEntries: Int,
                                  clock: () => Long): Cache[K, V] =
    new Memory[K, V](regime, maxEntries, clock)

  // ── the memory engine ──────────────────────────────────────────

  /** one in-flight load: complete once, late subscribers answer
   * immediately — the single-flight cell */
  private final class Flight[V]:
    private var done: Option[Either[Throwable, V]] = None
    private var waiters = List.empty[Either[Throwable, V] => Unit]

    def complete(r: Either[Throwable, V]): Unit =
      val ws = synchronized {
        done = Some(r)
        val w = waiters
        waiters = Nil
        w
      }
      ws.reverse.foreach(_(r))

    def subscribe(k: Either[Throwable, V] => Unit): Unit =
      val now = synchronized {
        done match
          case some @ Some(_) => some
          case None => waiters ::= k; None
      }
      now.foreach(k)

  private final class Memory[K, V](regime: Regime, maxEntries: Int,
                                   clock: () => Long) extends Cache[K, V]:
    require(maxEntries > 0, "a cache is bounded; maxEntries must be positive")

    private final case class Entry(value: V, storedAt: Long)

    // LinkedHashMap keeps insertion order; a touched key is
    // re-inserted, so the head is always the least recently USED
    private val entries = mutable.LinkedHashMap.empty[K, Entry]
    private val flights = mutable.HashMap.empty[K, Flight[V]]
    private var hits = 0L
    private var misses = 0L
    private var loads = 0L
    private var evictions = 0L

    private def fresh(e: Entry): Boolean = regime match
      case Regime.Invalidated => true
      case Regime.Budget(ttl) => clock() - e.storedAt <= ttl

    /** the one synchronized read: a fresh hit is touched (recency),
     * an expired entry is REMOVED and counted a miss */
    private def lookup(k: K): Option[V] = synchronized {
      entries.get(k) match
        case Some(e) if fresh(e) =>
          entries.remove(k)
          entries.put(k, e)
          hits += 1
          Some(e.value)
        case Some(_) =>
          entries.remove(k)
          misses += 1
          None
        case None =>
          misses += 1
          None
    }

    private def store(k: K, v: V): Unit = synchronized {
      entries.remove(k)
      entries.put(k, Entry(v, clock()))
      while entries.size > maxEntries do
        entries.remove(entries.head._1)
        evictions += 1
    }

    def get(k: K): Option[V] ! Async = async(lookup(k))

    def put(k: K, v: V): Unit ! Async = async(store(k, v))

    def invalidate(k: K): Unit ! Async = async {
      synchronized { entries.remove(k) }
      ()
    }

    def getOrLoad(k: K)(load: K => V ! Async): V ! Async =
      // claim under the lock: a hit answers, the FIRST miss owns the
      // load, everyone else subscribes to its flight
      async[Either[V, (Flight[V], Boolean)]] {
        synchronized {
          lookup(k) match
            case Some(v) => Left(v)
            case None => flights.get(k) match
              case Some(f) => Right((f, false))
              case None =>
                val f = Flight[V]()
                flights.put(k, f)
                loads += 1
                Right((f, true))
        }
      }.flatMap {
        case Left(v) => okay.pure(v)
        case Right((flight, owns)) =>
          val loaded: Unit ! Async =
            if !owns then okay.pure(())
            else async {
              // the loader runs under its own drive, so a failure
              // ANYWHERE in it (a thrown step, a failed Await)
              // completes the flight instead of stranding waiters
              Async.runAsync(load(k)).onComplete { t =>
                val r = t.toEither
                synchronized {
                  flights.remove(k)
                  r.foreach(v => store(k, v))
                }
                flight.complete(r)
              }(using scala.concurrent.ExecutionContext.parasitic)
            }
          loaded
            .flatMap(_ => okay.await[Either[Throwable, V]](flight.subscribe))
            .map(_.fold(t => throw t, identity))
      }

    def stats: Cache.Stats = synchronized {
      Cache.Stats(hits, misses, loads, evictions, entries.size)
    }
