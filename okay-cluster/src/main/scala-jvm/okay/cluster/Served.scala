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
