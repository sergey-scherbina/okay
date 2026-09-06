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
  def handle(existing: Option[String], now: Long = System.currentTimeMillis()): Sessions.Handle

  /** live sessions */
  def size: Int

object Sessions:
  val defaultTtl: java.time.Duration = java.time.Duration.ofMinutes(30)

  def apply(ttl: java.time.Duration = defaultTtl): Sessions = memory(ttl)

  def memory(ttl: java.time.Duration = defaultTtl): Sessions = new Engine(new MemoryBackend, ttl)

  /** sessions written through to an okay-persist topic (keyed,
   * compacted -- one key per session, the whole state as the value,
   * an empty value as the tombstone) and rebuilt from it on open, so
   * they survive the process. Every write is `Ack.Durable`. */
  def persisted(store: Store, ttl: java.time.Duration = defaultTtl, topic: String = "__sessions"): Sessions =
    new Engine(new PersistedBackend(store.topic(topic, 1, Policy(compact = true))), ttl)

  /** a session's whole state -- what an engine stores and a log records */
  final case class State(lastAccess: Long, attrs: Map[String, String])

  /** the engine SPI: an in-memory index that a persisted engine also
   * writes through */
  private[script] trait Backend:
    def get(id: String): Option[State]
    def put(id: String, state: State): Unit
    def delete(id: String): Unit
    def ids: Vector[String]
    def size: Int

  private final class MemoryBackend extends Backend:
    private val m = new ConcurrentHashMap[String, State]
    def get(id: String): Option[State] = Option(m.get(id))
    def put(id: String, state: State): Unit = m.put(id, state): Unit
    def delete(id: String): Unit = m.remove(id): Unit
    def ids: Vector[String] = m.keySet.asScala.toVector
    def size: Int = m.size

  private final class PersistedBackend(topic: Topic) extends Backend:
    private val index = new MemoryBackend
    rebuild()

    private def rebuild(): Unit =
      for p <- 0 until topic.partitions do
        var from = topic.begin(p)
        var going = true
        while going do
          topic.read(p, from, 512) match
            case Topic.Read.TooEarly(b) => from = b
            case Topic.Read.Records(rs) =>
              if rs.isEmpty then going = false
              else
                for r <- rs do
                  val id = new String(r.key, "UTF-8")
                  if r.value.isEmpty then index.delete(id)
                  else decode(r.value).foreach(index.put(id, _))
                from = rs.last.offset + 1
      // expiry is the sweep's job, on the CALLER's clock (handle(now)):
      // the first request after a restart drops what expired while
      // the process was down, rather than this rebuild guessing with
      // the wall clock -- which a test with a synthetic clock caught

    def get(id: String): Option[State] = index.get(id)
    def put(id: String, state: State): Unit =
      index.put(id, state)
      topic.append(id.getBytes("UTF-8"), encode(state), Ack.Durable): Unit
    def delete(id: String): Unit =
      index.delete(id)
      topic.append(id.getBytes("UTF-8"), Array.empty[Byte], Ack.Durable): Unit
    def ids: Vector[String] = index.ids
    def size: Int = index.size

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

    def handle(existing: Option[String], now: Long): Handle =
      sweep(now)
      new Handle(backend, () => newId(), existing, now)

    def size: Int = backend.size

  /** one request's view of one session */
  final class Handle private[Sessions] (backend: Backend, mint: () => String, existing: Option[String], now: Long)
      extends api.Session:
    private var bound: Option[(String, State)] =
      existing.flatMap(id => backend.get(id).map(s => id -> s.copy(lastAccess = now)))
    // touching is a write: the last-access time is part of the state
    bound.foreach((id, s) => backend.put(id, s))

    @volatile private var _created = false
    @volatile private var _invalidated = false

    /** a new id was minted during this request */
    def created: Boolean = _created

    /** the client's cookie must be expired */
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
