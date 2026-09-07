package okay.script

import okay.persist.{Ack, Policy, Store, Topic}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** The session store behind a `Site` -- id -> attributes with a
 * last-access time, swept of entries idle longer than `ttl`. Ids are
 * 128 random bits, URL-safe base64. Two engines: `memory` (the
 * default) and `persisted` over okay-persist, so a restart keeps the
 * carts. See specs/okay-script.md "Session" and "Persistent
 * sessions".
 */
trait Sessions:
  /** the session a request sees: bound to `existing` if that id is
   * live, else created on the first `set` -- `created`/`invalidated`
   * tell the container which cookie to send back */
  def handle(existing: Option[String], now: Long = System.currentTimeMillis()): Sessions.Bound

  /** live sessions */
  def size: Int

  /** stops whatever keeps this engine current (a `shared` engine's
   * tailer); the default engines have nothing to stop */
  def close(): Unit = ()

object Sessions:
  val defaultTtl: java.time.Duration = java.time.Duration.ofMinutes(30)

  def apply(ttl: java.time.Duration = defaultTtl): Sessions = memory(ttl)

  def memory(ttl: java.time.Duration = defaultTtl): Sessions = new Engine(new MemoryBackend, ttl)

  /** sessions written through to an okay-persist topic (keyed,
   * compacted -- one key per session, the whole state as the value,
   * an empty value as the tombstone) and rebuilt from it on open, so
   * they survive the process. Every write is `Ack.Durable`. */
  def persisted(store: Store, ttl: java.time.Duration = defaultTtl, topic: String = "__sessions"): Sessions =
    new Engine(new PersistedBackend(store.topic(topic, 1, Policy(compact = true)), tail = None), ttl)

  /** the persisted engine over ANY topic -- a `Replicated`
   * coordinator on the node that hosts it, a `RemoteStore` topic on
   * every other -- with the index kept current by TAILING the topic
   * every `poll`, so a session set on one node is read on another.
   * See specs/okay-script.md "Clustered sessions". */
  def shared(topic: Topic, ttl: java.time.Duration = defaultTtl,
             poll: java.time.Duration = java.time.Duration.ofMillis(200)): Sessions =
    new Engine(new PersistedBackend(topic, tail = Some(poll.toMillis)), ttl)

  /** a session's whole state -- what an engine stores and a log records */
  final case class State(lastAccess: Long, attrs: Map[String, String])

  /** the engine SPI: an in-memory index that a persisted engine also
   * writes through */
  private[script] trait Backend:
    def get(id: String): Option[State]
    def put(id: String, state: State): Unit
    /** last-access only, and ONLY if the session still exists: a
     * touch must never create -- or a stale cookie read on one node
     * racing an invalidate on another would resurrect the session
     * (found by the cluster test's invalidate step) */
    def touch(id: String, now: Long): Unit
    def delete(id: String): Unit
    def ids: Vector[String]
    def size: Int
    def close(): Unit = ()

  private final class MemoryBackend extends Backend:
    private val m = new ConcurrentHashMap[String, State]
    def get(id: String): Option[State] = Option(m.get(id))
    def put(id: String, state: State): Unit = m.put(id, state): Unit
    def touch(id: String, now: Long): Unit = m.computeIfPresent(id, (_, s) => s.copy(lastAccess = now)): Unit
    def delete(id: String): Unit = m.remove(id): Unit
    def ids: Vector[String] = m.keySet.asScala.toVector
    def size: Int = m.size

  private final class PersistedBackend(topic: Topic, tail: Option[Long]) extends Backend:
    private val index = new MemoryBackend
    /** where the tailer resumes, per partition */
    private val next = new Array[Long](topic.partitions)
    /** offsets THIS node appended: the tailer skips them, or a
     * re-applied older own write would regress the index for a
     * moment (put v1 at 5, put v2 at 6, tailer applies 5 before 6) */
    private val own = ConcurrentHashMap.newKeySet[Long]()
    @volatile private var closed = false
    rebuild()
    tail.foreach(ms => Thread.ofVirtual().name("okay-script-sessions-tail").start(() => tailLoop(ms)): Unit)

    private def apply(r: okay.persist.Record): Unit =
      val id = new String(r.key, "UTF-8")
      if r.value.isEmpty then index.delete(id)
      else if r.value.length == TouchBytes then index.touch(id, decodeTouch(r.value))
      else decode(r.value).foreach(index.put(id, _))

    /** reads partition `p` from `next(p)` to the end, applying;
     * `skipOwn` is false for the rebuild (nothing is ours yet) */
    private def catchUp(p: Int, skipOwn: Boolean): Unit =
      var going = true
      while going do
        topic.read(p, next(p), 512) match
          case Topic.Read.TooEarly(b) => next(p) = b
          case Topic.Read.Records(rs) =>
            if rs.isEmpty then going = false
            else
              for r <- rs do
                if !(skipOwn && own.remove(r.offset)) then apply(r)
              next(p) = rs.last.offset + 1

    private def rebuild(): Unit =
      for p <- 0 until topic.partitions do
        next(p) = topic.begin(p)
        catchUp(p, skipOwn = false)
      // expiry is the sweep's job, on the CALLER's clock (handle(now)):
      // the first request after a restart drops what expired while
      // the process was down, rather than this rebuild guessing with
      // the wall clock -- which a test with a synthetic clock caught

    private def tailLoop(ms: Long): Unit =
      while !closed do
        try
          for p <- 0 until topic.partitions do catchUp(p, skipOwn = true)
        catch case _: Throwable => () // a dead wire is retried next poll
        Thread.sleep(ms)

    private def record(offset: Long): Unit =
      if tail.isDefined then own.add(offset): Unit

    def get(id: String): Option[State] = index.get(id)
    def put(id: String, state: State): Unit =
      index.put(id, state)
      record(topic.append(id.getBytes("UTF-8"), encode(state), Ack.Durable))
    def touch(id: String, now: Long): Unit =
      index.touch(id, now)
      record(topic.append(id.getBytes("UTF-8"), encodeTouch(now), Ack.Durable))
    def delete(id: String): Unit =
      index.delete(id)
      record(topic.append(id.getBytes("UTF-8"), Array.empty[Byte], Ack.Durable))
    def ids: Vector[String] = index.ids
    def size: Int = index.size
    override def close(): Unit = closed = true

  /** a touch record is exactly the 8-byte last-access time; a full
   * state is at least 12 (long + int), a tombstone is 0 -- the length
   * IS the record type */
  private val TouchBytes = 8

  private def encodeTouch(now: Long): Array[Byte] =
    java.nio.ByteBuffer.allocate(TouchBytes).putLong(now).array()

  private def decodeTouch(b: Array[Byte]): Long =
    java.nio.ByteBuffer.wrap(b).getLong

  private def encode(s: State): Array[Byte] =
    val bytes = new ByteArrayOutputStream
    val out = new DataOutputStream(bytes)
    out.writeLong(s.lastAccess)
    out.writeInt(s.attrs.size)
    for (k, v) <- s.attrs do
      out.writeUTF(k)
      out.writeUTF(v)
    out.flush()
    bytes.toByteArray

  /** damage is `None`, never a throw -- a torn record is a lost
   * session, not a lost site */
  private def decode(b: Array[Byte]): Option[State] =
    try
      val in = new DataInputStream(new ByteArrayInputStream(b))
      val last = in.readLong()
      val n = in.readInt()
      val attrs = Map.newBuilder[String, String]
      for _ <- 0 until n do
        val k = in.readUTF()
        attrs += (k -> in.readUTF())
      Some(State(last, attrs.result()))
    catch case _: java.io.IOException => None

  /** the sweep and the id minting, over any backend */
  private final class Engine(backend: Backend, ttl: java.time.Duration) extends Sessions:
    private val random = new java.security.SecureRandom

    private def sweep(now: Long): Unit =
      backend.ids.foreach(id => backend.get(id).filter(s => now - s.lastAccess > ttl.toMillis).foreach(_ => backend.delete(id)))

    private def newId(): String =
      val b = new Array[Byte](16)
      random.nextBytes(b)
      java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(b)

    def handle(existing: Option[String], now: Long): Bound =
      sweep(now)
      new Handle(backend, () => newId(), existing, now)

    def size: Int = backend.size

    override def close(): Unit = backend.close()

  /**
   * One request's view of one session, as the CONTAINER sees it: the
   * page's `api.Session` plus the two facts that decide the cookie.
   *
   * A trait rather than the concrete `Handle` because a caller can
   * have a legitimate view of a session that is not this engine's —
   * `okay script build` installs one whose every method refuses,
   * since a static site has no request to have a session in
   * (script-cli).
   */
  trait Bound extends api.Session:
    /** a new id was minted during this request */
    def created: Boolean
    /** the client's cookie must be expired */
    def invalidated: Boolean

  /** one request's view of one session */
  final class Handle private[Sessions] (backend: Backend, mint: () => String, existing: Option[String], now: Long)
      extends Bound:
    private var bound: Option[(String, State)] =
      existing.flatMap(id => backend.get(id).map(s => id -> s.copy(lastAccess = now)))
    // touching is a write (the last-access time is state) but an
    // UPDATE-IF-PRESENT one, never a create -- see Backend.touch
    bound.foreach((id, _) => backend.touch(id, now))

    @volatile private var _created = false
    @volatile private var _invalidated = false

    def created: Boolean = _created

    def invalidated: Boolean = _invalidated

    def id: String = bound.map(_._1).getOrElse("")

    def get(key: String): Option[String] = bound.flatMap(_._2.attrs.get(key))

    def set(key: String, value: String): Unit = synchronized:
      if bound.isEmpty then
        bound = Some(mint() -> State(now, Map.empty))
        _created = true
        _invalidated = false
      bound.foreach { (id, s) =>
        val next = s.copy(attrs = s.attrs + (key -> value))
        bound = Some(id -> next)
        backend.put(id, next)
      }

    def remove(key: String): Unit = synchronized:
      bound.foreach { (id, s) =>
        val next = s.copy(attrs = s.attrs - key)
        bound = Some(id -> next)
        backend.put(id, next)
      }

    def attributes: Map[String, String] = bound.map(_._2.attrs).getOrElse(Map.empty)

    def invalidate(): Unit = synchronized:
      bound.foreach((id, _) => backend.delete(id))
      if existing.isDefined || _created then _invalidated = true
      bound = None
      _created = false
