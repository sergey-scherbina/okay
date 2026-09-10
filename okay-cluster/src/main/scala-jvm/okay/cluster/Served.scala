package okay.cluster

import okay.codec.Codecs
import java.io.{DataInputStream, DataOutputStream}
import java.net.{ServerSocket, Socket}

/**
 * THE WORKER, ON A SOCKET (specs/dataflow.md, stage 4b).
 *
 * A four-byte big-endian length and then CBOR — no line framing, no
 * base64, because a partial is already bytes and re-encoding it as
 * text to fit a line-oriented protocol is a cost paid on every
 * accumulator that crosses.
 *
 * The connection is PERSISTENT and one request is in flight at a
 * time: a coordinator holds one per worker for a job's two rounds,
 * and every partition assigned to that worker takes its turn on it.
 * That is the smallest thing that works, and it is deliberate — a
 * multiplexed connection is a real design with real failure modes,
 * and nothing here has asked for one.
 */
object Served {

  /** answer requests until the socket closes */
  def handle(sock: Socket, serve: Cluster.Serve): Unit =
    val in = DataInputStream(sock.getInputStream)
    val out = DataOutputStream(sock.getOutputStream)
    try
      while true do
        val n = in.readInt()                       // EOFException ends the loop
        val bytes = new Array[Byte](n)
        in.readFully(bytes)
        val answer = Codecs.cbor(Req.given_Schema_Req).decode(bytes) match
          case Right(req) =>
            try serve(req) catch case t: Throwable => Resp.Failed(s"${t.getClass.getName}: ${t.getMessage}")
          case Left(why) => Resp.Failed(s"undecodable request: $why")
        val reply = Codecs.cbor(Resp.given_Schema_Resp).encode(answer)
        out.writeInt(reply.length)
        out.write(reply)
        out.flush()
    catch case _: java.io.EOFException => ()       // the coordinator hung up
    finally sock.close()

  /** accept connections and serve each on its own thread, until the
   * server socket is closed */
  def serve(server: ServerSocket, serve: Cluster.Serve): Unit =
    try
      while !server.isClosed do
        val sock = server.accept()
        val _ = Thread.ofVirtual().start(() => handle(sock, serve))
    catch case _: java.net.SocketException => ()   // closed while accepting

  /**
   * The coordinator's end. One socket, one request at a time —
   * `synchronized` because the driver fans partitions out over
   * fibres and several of them may share a worker.
   *
   * A worker that is gone THROWS, which is the protocol P7 already
   * had: `Cluster.distribute` treats a throwing worker as dead, and
   * stage 5 will do the same for a partition.
   */
  def connect(host: String, port: Int): Cluster.Serve =
    val sock = Socket(host, port)
    sock.setTcpNoDelay(true)
    val in = DataInputStream(sock.getInputStream)
    val out = DataOutputStream(sock.getOutputStream)
    req =>
      val bytes = Codecs.cbor(Req.given_Schema_Req).encode(req)
      val reply =
        sock.synchronized:
          out.writeInt(bytes.length)
          out.write(bytes)
          out.flush()
          val n = in.readInt()
          val buf = new Array[Byte](n)
          in.readFully(buf)
          buf
      Codecs.cbor(Resp.given_Schema_Resp).decode(reply) match
        case Right(r) => r
        case Left(why) => Resp.Failed(s"undecodable answer: $why")

  /**
   * THE SAME WORKER, ON A CONNECTION THAT HEALS
   * (dataflow-reconnect's second half).
   *
   * `connect` IS one socket: it dials once, and when that socket
   * breaks every later request on it breaks too. Tolerance in the
   * coordinator — burying a worker only after several consecutive
   * failures — is enough for a worker that HICCUPS, and cannot be
   * enough for this one, because the failure is permanent by
   * construction. A restarted worker process is reachable again and
   * its old socket never will be.
   *
   * So this dials LAZILY and drops the socket on any failure: the
   * next request dials again. What it does NOT do is retry inside
   * itself. The coordinator already has a policy for a failed
   * attempt — move the partition to a survivor, count the failure
   * against that worker, bury it if they keep coming — and a second
   * policy hidden in the transport would fight it. This makes the
   * connection able to heal; `Living` still decides when to give up.
   */
  def reconnecting(host: String, port: Int): Cluster.Serve =
    // a lock of its OWN. `connect` synchronizes on its socket, which
    // this cannot do because the socket is replaced; and a bare
    // `synchronized` inside the lambda would take the monitor of
    // `Served` itself and serialise every reconnecting worker in the
    // process against every other one.
    val lock = new Object
    var sock: Socket | Null = null
    var in: DataInputStream | Null = null
    var out: DataOutputStream | Null = null

    def dial(): Unit =
      val s = Socket(host, port)
      s.setTcpNoDelay(true)
      sock = s
      in = DataInputStream(s.getInputStream)
      out = DataOutputStream(s.getOutputStream)

    def drop(): Unit =
      val s = sock
      sock = null; in = null; out = null
      if s != null then try s.nn.close() catch case _: Throwable => ()

    req =>
      val bytes = Codecs.cbor(Req.given_Schema_Req).encode(req)
      val reply =
        lock.synchronized:
          if sock == null then dial()
          try
            out.nn.writeInt(bytes.length)
            out.nn.write(bytes)
            out.nn.flush()
            val n = in.nn.readInt()
            val buf = new Array[Byte](n)
            in.nn.readFully(buf)
            buf
          catch case t: Throwable =>
            // the socket is finished; the NEXT request dials again,
            // and this attempt is the coordinator's to place
            drop()
            throw t
      Codecs.cbor(Resp.given_Schema_Resp).decode(reply) match
        case Right(r) => r
        case Left(why) => Resp.Failed(s"undecodable answer: $why")
}

/**
 * A WORKER PROCESS.
 *
 * `java -cp <the build> okay.cluster.WorkerMain <port> <registrar...>`
 *
 * Each registrar is a class whose initialisation registers jobs —
 * which is how a build declares what it can run, and the reason a
 * worker needs no closure from anywhere. The port is printed once the
 * socket is bound, so a parent process can wait for that line instead
 * of guessing.
 */
object WorkerMain {
  def main(args: Array[String]): Unit =
    val port = if args.isEmpty then 0 else args(0).toInt
    for name <- args.drop(1) do Class.forName(name): Unit
    val server = ServerSocket(port)
    println(s"worker listening ${server.getLocalPort} knowing ${Jobs.names.mkString(",")}")
    System.out.flush()
    Served.serve(server, Cluster.local)
}
