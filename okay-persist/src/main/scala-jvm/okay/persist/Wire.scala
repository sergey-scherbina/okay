package okay.persist

import okay.{!, Async, async}
import okay.codec.{Cbor, Schema}
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream}
import java.net.{ServerSocket, Socket}

/**
 * The wire (specs/persist.md, "The wire"): the log reachable past
 * the process, as a DOCUMENTED surface — `[len:int32][CBOR]`
 * frames, message enums below as the one source of truth for both
 * ends (the okay-cluster precedent). The handshake's answer IS the
 * capability list: a client is offered exactly the topics its
 * token may see, and a name outside that set refuses by name.
 *
 * Auth is a FUNCTION (`token => Option[Set[topic]]`): okay-security
 * plugs its API-key verification in at construction; this module
 * carries no crypto of its own. TLS rides the one transport seam
 * when wire-tls lands — until then this surface is plaintext and
 * says so. The client speaks Async (an engine is not an access
 * path); the JVM leg is a blocking socket behind Async.Run, the
 * okay-pg pattern, and the Node leg arrives with a consumer.
 */
object Wire:

  // the protocol's one source of truth is SHARED (WireProtocol —
  // specs/net.md moved it so the Node client speaks the same
  // enums); the export keeps every existing Wire.* path compiling
  export WireProtocol.{Version, Req, Resp, WireRefused, Client}
  export WireProtocol.given

  private def ackOf(i: Int): Ack = WireProtocol.ackOf(i)
  private def ackCode(a: Ack): Int = WireProtocol.ackCode(a)

  private def writeFrame[A: Schema](out: DataOutputStream, a: A): Unit =
    val bs = Cbor.write(a)
    out.writeInt(bs.length)
    out.write(bs)
    out.flush()

  private def readFrame[A: Schema](in: DataInputStream): Either[String, A] =
    val len = in.readInt()
    if len < 0 || len > 64 * 1024 * 1024 then Left(s"frame length $len is not a frame")
    else
      val bs = new Array[Byte](len)
      in.readFully(bs)
      Cbor.read[A](bs)

  // ── the server ─────────────────────────────────────────────────

  /**
   * Serves one Store; `auth` decides per token which topics exist
   * for that client. Binds immediately (port 0 = ephemeral, the
   * bound port is `port`); one virtual thread per connection; damage
   * on a connection closes THAT connection, never the server.
   */
  final class Server(store: Store, auth: String => Option[Set[String]],
                     requested: Int = 0,
                     bind: java.net.InetAddress = java.net.InetAddress.getLoopbackAddress,
                     repl: String => Option[Replicated] = _ => None,
                     socket: Option[ServerSocket] = None):
    // TLS rides the ONE transport seam (specs/tls.md, persist-wire
    // lane): pass a `socket` built by `Tls.serverSocket` (an
    // SSLServerSocket) and every byte below is encrypted — the accept
    // loop and the frame code do not change, because encryption wraps
    // the TRANSPORT, not the protocol. okay-persist itself stays
    // dependency-free: the SSLServerSocket is built by the caller.
    // Loopback by DEFAULT when plaintext: a plaintext log does not
    // volunteer itself to the network — an operator opens it deliberately.
    private val listener = socket.getOrElse(ServerSocket(requested, 50, bind))
    @volatile private var closed = false

    val port: Int = listener.getLocalPort

    locally { val _ = Thread.ofVirtual().start(() => acceptLoop()) }

    private def acceptLoop(): Unit =
      while !closed do
        try
          val sock = listener.accept()
          Thread.ofVirtual().start(() => serve(sock))
          ()
        catch case _: Throwable => () // closed, or a doomed accept

    private def serve(sock: Socket): Unit =
      val in = DataInputStream(BufferedInputStream(sock.getInputStream))
      val out = DataOutputStream(BufferedOutputStream(sock.getOutputStream))
      try
        readFrame[Req](in) match
          case Right(Req.Hello(v, token)) if v == Version =>
            auth(token) match
              case None =>
                writeFrame[Resp](out, Resp.Refused("the token opens nothing here"))
              case Some(allowed) =>
                writeFrame[Resp](out, Resp.Granted(Version, allowed.toVector.sorted))
                var going = true
                while going do
                  readFrame[Req](in) match
                    case Left(_) => going = false // damage or EOF ends the connection
                    case Right(req) =>
                      writeFrame[Resp](out, answer(req, allowed))
          case Right(Req.Hello(v, _)) =>
            writeFrame[Resp](out,
              Resp.Refused(s"wire version $v is not spoken here (this node speaks $Version)"))
          case _ =>
            writeFrame[Resp](out, Resp.Refused("the first frame must be Hello"))
      catch case _: Throwable => ()
      finally sock.close()

    private def answer(req: Req, allowed: Set[String]): Resp =
      // a replicated name routes to its COORDINATOR (reads truncate to
      // the hwm, appends fence by epoch); every other name is a plain
      // engine topic — either way behind the capability check
      def topicOf(name: String): Either[Resp, Topic] =
        if !allowed.contains(name) then
          Left(Resp.Refused(s"topic $name is not on this client's capability list"))
        else Right(repl(name).getOrElse(store.topic(name)))
      def coordinatorOf(name: String): Either[Resp, Replicated] =
        if !allowed.contains(name) then
          Left(Resp.Refused(s"topic $name is not on this client's capability list"))
        else repl(name).toRight(Resp.Refused(s"topic $name is not a replicated topic"))
      try
        req match
          case Req.Hello(_, _) => Resp.Refused("Hello twice")
          case Req.Append(t, p, k, v, a) =>
            topicOf(t).fold(identity,
              tp => Resp.Appended(tp.append(p, k, v, ackOf(a))))
          case Req.Read(t, p, from, max) =>
            topicOf(t).fold(identity, tp => tp.read(p, from, max) match
              case Topic.Read.Records(rs) => Resp.Records(rs)
              case Topic.Read.TooEarly(b) => Resp.TooEarly(b))
          case Req.Begin(t, p) =>
            topicOf(t).fold(identity, tp => Resp.Offset(tp.begin(p)))
          case Req.End(t, p) =>
            topicOf(t).fold(identity, tp => Resp.Offset(tp.end(p)))
          case Req.Compact(t, p) =>
            topicOf(t).fold(identity, tp => { tp.compact(p); Resp.Done() })
          case Req.Produce(t, p, pid, seq, k, v, a) =>
            coordinatorOf(t).fold(identity,
              r => Resp.Appended(r.produce(p, pid, seq, k, v, ackOf(a))))
          case Req.Promote(t, p, replica) =>
            coordinatorOf(t).fold(identity, r => { r.promote(p, replica); Resp.Done() })
      catch case e: Throwable => Resp.Refused(s"the node threw: ${e.getMessage}")

    def close(): Unit =
      closed = true
      listener.close()

  // ── the client ─────────────────────────────────────────────────

  /** the Async remote surface; `topics` is what the handshake
   * GRANTED — the capability list is the offer */
  final class Remote private[Wire] (sock: Socket, in: DataInputStream,
                                    out: DataOutputStream,
                                    val topics: Vector[String]):

    private def call(req: Req): Resp =
      sock.synchronized {
        writeFrame[Req](out, req)
        readFrame[Resp](in).fold(e => throw WireRefused(s"a damaged answer: $e"), identity)
      }

    // ── the synchronous surface (package-private) ────────────────
    // one call is one round trip and already blocks under `call`;
    // the async methods below are `async{}` wrappers of these, and
    // RemoteStore drives a remote replica straight through them —
    // the coordinator's Topic is synchronous, so a remote replica is
    // reached on the coordinator's own thread, the okay-pg waist.

    private[persist] def appendSync(topic: String, partition: Int, key: Array[Byte],
                                    value: Array[Byte], ack: Ack): Long =
      call(Req.Append(topic, partition, key, value, ackCode(ack))) match
        case Resp.Appended(off) => off
        case Resp.Refused(r) => throw WireRefused(r)
        case other => throw WireRefused(s"unexpected answer $other")

    private[persist] def readSync(topic: String, partition: Int, from: Long, max: Int): Topic.Read =
      call(Req.Read(topic, partition, from, max)) match
        case Resp.Records(rs) => Topic.Read.Records(rs)
        case Resp.TooEarly(b) => Topic.Read.TooEarly(b)
        case Resp.Refused(r) => throw WireRefused(r)
        case other => throw WireRefused(s"unexpected answer $other")

    private[persist] def beginSync(topic: String, partition: Int): Long =
      offsetSync(Req.Begin(topic, partition))
    private[persist] def endSync(topic: String, partition: Int): Long =
      offsetSync(Req.End(topic, partition))

    private def offsetSync(req: Req): Long = call(req) match
      case Resp.Offset(v) => v
      case Resp.Refused(r) => throw WireRefused(r)
      case other => throw WireRefused(s"unexpected answer $other")

    private[persist] def compactSync(topic: String, partition: Int): Unit =
      doneSync(Req.Compact(topic, partition))

    private def doneSync(req: Req): Unit = call(req) match
      case Resp.Done() => ()
      case Resp.Refused(r) => throw WireRefused(r)
      case other => throw WireRefused(s"unexpected answer $other")

    // ── the async surface (an engine is not an access path) ──────

    def append(topic: String, partition: Int, key: Array[Byte],
               value: Array[Byte], ack: Ack = Ack.Durable): Long ! Async =
      async { appendSync(topic, partition, key, value, ack) }

    def read(topic: String, partition: Int, from: Long, max: Int): Topic.Read ! Async =
      async { readSync(topic, partition, from, max) }

    def begin(topic: String, partition: Int): Long ! Async = async { beginSync(topic, partition) }
    def end(topic: String, partition: Int): Long ! Async = async { endSync(topic, partition) }

    def compact(topic: String, partition: Int): Unit ! Async =
      async { compactSync(topic, partition) }

    /** the idempotent producer, driven remotely: a retry with the
     * same (producerId, seq) lands once and answers the original
     * offset — the server's topic must be a replicated coordinator */
    def produce(topic: String, partition: Int, producerId: String, seq: Long,
                key: Array[Byte], value: Array[Byte], ack: Ack = Ack.Replicated): Long ! Async =
      async {
        call(Req.Produce(topic, partition, producerId, seq, key, value, ackCode(ack))) match
          case Resp.Appended(off) => off
          case Resp.Refused(r) => throw WireRefused(r)
          case other => throw WireRefused(s"unexpected answer $other")
      }

    /** the operator's failover, driven remotely */
    def promote(topic: String, partition: Int, replica: Int): Unit ! Async =
      async { doneSync(Req.Promote(topic, partition, replica)) }

    def close(): Unit = sock.close()

  object Remote:
    /** the handshake: refused tokens and versions throw by name.
     * `wrap` is the TLS integration point (specs/tls.md): pass
     * `s => Tls.client(s, host, cfg).fold(throw …, identity)` to
     * encrypt the transport before any protocol byte flows — the
     * seam's own contract ("wrap an already-connected client socket
     * BEFORE any protocol bytes"). Default identity = plaintext. */
    def connect(host: String, port: Int, token: String,
                wrap: Socket => Socket = identity): Remote =
      val raw = Socket(host, port)
      raw.setTcpNoDelay(true)
      val sock = wrap(raw)   // e.g. the SSLSocket the TLS handshake produced
      val in = DataInputStream(BufferedInputStream(sock.getInputStream))
      val out = DataOutputStream(BufferedOutputStream(sock.getOutputStream))
      writeFrame[Req](out, Req.Hello(Version, token))
      readFrame[Resp](in) match
        case Right(Resp.Granted(_, topics)) => new Remote(sock, in, out, topics)
        case Right(Resp.Refused(r)) => sock.close(); throw WireRefused(r)
        case other => sock.close(); throw WireRefused(s"a broken handshake: $other")
