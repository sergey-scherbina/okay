package okay.persist

import okay.codec.Cbor
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream}
import java.net.{ServerSocket, Socket}

/**
 * Raft's peer-to-peer transport, stage 1a (specs/consensus.md,
 * persist-raft): real `ServerSocket`s, real threads, the SAME
 * `[len:int32][CBOR]` framing `Wire.scala` already uses for the
 * client-facing wire — reused here for NODE-TO-NODE `RaftMsg`
 * exchange. Proves the algorithm (`Raft.scala`, stage 0) survives
 * real network I/O and real wall-clock timing, not just an
 * in-process message bus driven by explicit test ticks.
 *
 * Deliberately NOT stage 1 whole: no `Store`/`Topic` wrapper yet
 * (`Election` cannot construct a topic over this), no persistent
 * `currentTerm`/`votedFor` (a real crash forgets them here — Raft's
 * own safety proof assumes stable storage for exactly those two
 * fields, stated not hidden).
 */
object RaftWire:

  private def writeFrame(out: DataOutputStream, m: RaftMsg): Unit =
    val bs = Cbor.write(m)
    out.writeInt(bs.length)
    out.write(bs)
    out.flush()

  private def readFrame(in: DataInputStream): Either[String, RaftMsg] =
    val len = in.readInt()
    if len < 0 || len > 16 * 1024 * 1024 then Left(s"frame length $len is not a frame")
    else
      val bs = new Array[Byte](len)
      in.readFully(bs)
      Cbor.read[RaftMsg](bs)

  /**
   * One real Raft node. `peers` names every OTHER node's address —
   * this node is never in its own peer set. A background tick
   * thread drives election timeouts (randomized per node, so a
   * cold cluster does not deadlock on a perfect tie every round)
   * and, once leading, periodic heartbeats/replication.
   *
   * Sends are ONE-SHOT connections (connect, write one frame,
   * close) — simple and correct first, not yet a pooled/persistent
   * connection per peer; Raft's own retry-by-heartbeat already
   * tolerates a dropped send, so a failed one-shot connect costs a
   * cycle, never correctness.
   */
  final class Node(id: String, port: Int, peers: Map[String, (String, Int)],
                   tickMs: Long = 50, electionTimeoutMs: Long = 300,
                   heartbeatMs: Long = 100,
                   onCommit: (Long, RaftEntry) => Unit = (_, _) => ()) {

    private val lock = new Object
    private var state = RaftState(id = id)
    private var lastHeartbeatSent = 0L
    private var nextElectionAt = System.currentTimeMillis() + jitter()

    private def jitter(): Long =
      electionTimeoutMs + scala.util.Random.nextInt(electionTimeoutMs.toInt)

    private val peerIds: Set[String] = peers.keySet

    private val listener = ServerSocket(port)
    @volatile private var closed = false

    Thread.ofVirtual().start(() => acceptLoop()): Unit
    Thread.ofVirtual().start(() => tickLoop()): Unit

    private def acceptLoop(): Unit =
      while !closed do
        try
          val sock = listener.accept()
          Thread.ofVirtual().start(() => handleConn(sock)): Unit
        catch case _: Throwable => ()   // closed, or a doomed accept

    private def handleConn(sock: Socket): Unit =
      try
        val in = DataInputStream(BufferedInputStream(sock.getInputStream))
        readFrame(in).foreach(onMessage)
      catch case _: Throwable => ()
      finally sock.close()

    /** the ONE state transition, network-driven: apply, notify newly
     * committed entries, send whatever it produced — network I/O
     * happens OUTSIDE the lock */
    private def onMessage(msg: RaftMsg): Unit =
      val (toSend, newlyCommitted) = lock.synchronized {
        val before = state.commitIndex
        val (ns, out) = Raft.handle(state, msg, peerIds)
        state = ns
        msg match
          case _: RaftMsg.AppendEntries => nextElectionAt = System.currentTimeMillis() + jitter()
          case _ => ()
        (out, if ns.commitIndex > before then (before until ns.commitIndex).toVector else Vector.empty[Long])
      }
      newlyCommitted.foreach(i => onCommit(i + 1, state.log(i.toInt)))
      toSend.foreach(send)

    private def send(o: RaftOut): Unit =
      peers.get(o.to).foreach { (host, p) =>
        try
          val sock = Socket()
          try
            sock.connect(new java.net.InetSocketAddress(host, p), 200)
            val out = DataOutputStream(BufferedOutputStream(sock.getOutputStream))
            writeFrame(out, o.msg)
          finally sock.close()
        catch case _: Throwable => ()   // best-effort: heartbeats/timeouts retry
      }

    private def tickLoop(): Unit =
      while !closed do
        Thread.sleep(tickMs)
        val toSend = lock.synchronized {
          val now = System.currentTimeMillis()
          state.role match
            case RaftRole.Leader =>
              if now - lastHeartbeatSent >= heartbeatMs then
                lastHeartbeatSent = now
                Raft.replicate(state, peerIds)
              else Vector.empty
            case _ =>
              if now >= nextElectionAt then
                val (ns, out) = Raft.startElection(state, peerIds)
                state = ns
                nextElectionAt = now + jitter()
                out
              else Vector.empty
        }
        toSend.foreach(send)

    def isLeader: Boolean = lock.synchronized(state.role == RaftRole.Leader)
    def leaderId: Option[String] = lock.synchronized(state.leaderId)
    def currentTerm: Long = lock.synchronized(state.currentTerm)
    def commitIndex: Long = lock.synchronized(state.commitIndex)
    def logSnapshot: Vector[RaftEntry] = lock.synchronized(state.log)

    /** the client seam: succeeds only on the CURRENT leader — no
     * forwarding to the real leader yet (stage 1b), so a caller
     * must retry elsewhere on `false`, the same way a follower's
     * demo-two-nodes route answers 503 naming the leader */
    def propose(data: Array[Byte]): Boolean =
      val toSend = lock.synchronized {
        if state.role != RaftRole.Leader then None
        else
          state = state.copy(log = state.log :+ RaftEntry(state.currentTerm, data))
          Some(Raft.replicate(state, peerIds))
      }
      toSend match
        case None => false
        case Some(out) => out.foreach(send); true

    def close(): Unit =
      closed = true
      listener.close()
  }
