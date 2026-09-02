package okay.cache

import okay.{!, Async, async}
import okay.given
import okay.codec.{Cbor, Schema}

/**
 * The Redis engine (specs/cache.md stage 2): the same trait over a
 * MINIMAL RESP client — GET/SET PX/DEL/PING and nothing more,
 * because four commands do not justify a client-library dependency.
 * This stack already speaks SSE, JSON-RPC, MCP, SigV4 and the pg
 * protocol on the wire; RESP is simpler than any of them.
 *
 * A Budget maps to `SET PX`: Redis enforces expiry SERVER-side, so
 * an expired entry is gone, not filtered. Values travel as CBOR (a
 * Redis value is a CBOR blob, readable by anything with the
 * Schema). TLS deployments (`rediss`) wrap the socket through
 * specs/tls.md before it is handed here — the RESP client adds
 * nothing of its own.
 */
object Redis {

  /** the client: one connection, one in-flight command at a time
   * (synchronized) — a virtual thread parks in the read */
  final class Resp(sock: java.net.Socket):
    private val out = java.io.BufferedOutputStream(sock.getOutputStream)
    private val in = java.io.BufferedInputStream(sock.getInputStream)

    def command(args: Array[Byte]*): Either[String, Option[Array[Byte]]] = synchronized {
      val sb = new java.io.ByteArrayOutputStream
      sb.write(s"*${args.length}\r\n".getBytes)
      for a <- args do
        sb.write(s"$$${a.length}\r\n".getBytes)
        sb.write(a); sb.write("\r\n".getBytes)
      out.write(sb.toByteArray); out.flush()
      reply()
    }

    private def line(): String =
      val b = new StringBuilder
      var c = in.read()
      while c != '\r' && c >= 0 do { b.append(c.toChar); c = in.read() }
      if c < 0 || in.read() != '\n' then
        throw IllegalStateException("the redis connection is DEAD mid-reply")
      b.toString

    /** the four reply shapes these commands can answer — total over
     * them, loud on a wire this client does not speak */
    private def reply(): Either[String, Option[Array[Byte]]] =
      in.read() match
        case '+' => line(): Unit; Right(Some(Array.empty)) // +OK and friends
        case ':' => line(): Unit; Right(Some(Array.empty)) // integer acks (DEL)
        case '-' => Left(line())                              // -ERR, as data
        case '$' =>
          val n = line().toInt
          if n < 0 then Right(None)                           // the nil bulk: a miss
          else
            val buf = new Array[Byte](n)
            var at = 0
            while at < n do
              val r = in.read(buf, at, n - at)
              if r < 0 then throw IllegalStateException("the redis connection is DEAD mid-bulk")
              at += r
            if in.read() != '\r' || in.read() != '\n' then
              throw IllegalStateException("a bulk without its terminator")
            Right(Some(buf))
        case other =>
          throw IllegalStateException(s"a RESP reply this client does not speak (leading byte $other)")

    def close(): Unit = sock.close()

  /** connect and PING — fail fast, before any entry is trusted to it */
  def connect(host: String = "127.0.0.1", port: Int = 6379): Resp =
    val s = java.net.Socket(host, port)
    s.setTcpNoDelay(true)
    val r = Resp(s)
    r.command("PING".getBytes) match
      case Right(_) => r
      case Left(e) => throw IllegalStateException(s"redis at $host:$port refused the PING: $e")

  /**
   * The engine. `keyOf` renders keys (a prefix belongs in it);
   * values ride CBOR by the Schema. Eviction and expiry are the
   * SERVER's; stats count what this process saw.
   */
  def cache[K, V](resp: Resp, regime: Regime, keyOf: K => String)
                 (using Schema[V]): Cache[K, V] = new Cache[K, V]:
    private val hits = java.util.concurrent.atomic.AtomicLong()
    private val misses = java.util.concurrent.atomic.AtomicLong()
    private val loads = java.util.concurrent.atomic.AtomicLong()
    private val flights = java.util.concurrent.ConcurrentHashMap[String, AnyRef]()

    private def raw(k: K): Array[Byte] = keyOf(k).getBytes("UTF-8")

    def get(k: K): Option[V] ! Async = async {
      resp.command("GET".getBytes, raw(k)) match
        case Right(Some(bytes)) if bytes.nonEmpty =>
          Cbor.read[V](bytes) match
            case Right(v) => hits.incrementAndGet(); Some(v)
            case Left(_) => misses.incrementAndGet(); None   // damage is a miss, not a throw
        case _ => misses.incrementAndGet(); None
    }

    def put(k: K, v: V): Unit ! Async = async {
      val body = Cbor.write(v)
      val ok = regime match
        case Regime.Budget(ttl) =>
          resp.command("SET".getBytes, raw(k), body,
            "PX".getBytes, ttl.toString.getBytes)
        case Regime.Invalidated =>
          resp.command("SET".getBytes, raw(k), body)
      ok match
        case Left(e) => throw IllegalStateException(s"redis SET refused: $e")
        case _ => ()
    }

    def invalidate(k: K): Unit ! Async = async {
      resp.command("DEL".getBytes, raw(k)) match
        case Left(e) => throw IllegalStateException(s"redis DEL refused: $e")
        case _ => ()
    }

    def getOrLoad(k: K)(load: K => V ! Async): V ! Async =
      get(k).flatMap {
        case Some(v) => okay.pure(v)
        case None => async {
          // single-flight is PER PROCESS (as in the memory engine):
          // the lock guards this node's dogpile, not the cluster's.
          // The whole miss path runs UNDER the lock — a virtual
          // thread parks here, the Loom trade as everywhere
          val key = keyOf(k)
          val lock = flights.computeIfAbsent(key, _ => new Object)
          try lock.synchronized {
            resp.command("GET".getBytes, raw(k)) match   // re-check inside
              case Right(Some(bytes)) if bytes.nonEmpty && Cbor.read[V](bytes).isRight =>
                hits.incrementAndGet()
                Cbor.read[V](bytes).toOption.get
              case _ =>
                loads.incrementAndGet()
                val v = okay.!.run(Async.run[V, Nothing](load(k)))
                okay.!.run(Async.run[Unit, Nothing](put(k, v)))
                v
          } finally flights.remove(key): Unit
        }
      }

    def stats: Cache.Stats =
      Cache.Stats(hits.get, misses.get, loads.get, evictions = 0, size = 0)
}
