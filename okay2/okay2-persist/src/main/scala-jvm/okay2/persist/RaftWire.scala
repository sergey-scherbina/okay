package okay2.persist

import okay2.codec.{Cbor, Codecs, Schema}
import okay2.platform.Threads
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream}
import java.net.{ServerSocket, Socket}

/**
 * Raft's peer-to-peer transport (okay-persist's RaftWire.scala;
 * specs/consensus.md, persist-raft): real `ServerSocket`s, real threads,
 * the SAME `[len:int32][CBOR]` framing the client wire uses, for
 * NODE-TO-NODE `RaftMsg` exchange. `currentTerm` and `votedFor` are kept
 * on a `Stable` across a crash, as Raft's proof assumes.
 */
object RaftWire {

  /** one member's address, as carried inside a configuration entry: the
   * log itself is the directory */
  final case class Address(id: String, host: String, port: Int)

  object Address {
    implicit lazy val schema: Schema[Address] = Schema.derived
  }

  /**
   * Stable storage for the two fields Raft's proof assumes survive a
   * crash: `currentTerm` and `votedFor`. Written before any message the
   * transition produced is sent, read once at start. `file` is one small
   * file, replaced atomically (write a sibling, rename over); `memory` is
   * for tests and for a node whose crash is its end.
   */
  trait Stable {
    def load(): (Long, Option[String])
    def save(term: Long, votedFor: Option[String]): Unit
  }

  object Stable {
    def memory(term: Long = 0L, votedFor: Option[String] = None): Stable = new Stable {
      private var held = (term, votedFor)
      def load(): (Long, Option[String]) = synchronized(held)
      def save(t: Long, v: Option[String]): Unit = synchronized { held = (t, v) }
    }

    def file(path: java.nio.file.Path): Stable = new Stable {
      import java.nio.file.{Files, StandardCopyOption}
      def load(): (Long, Option[String]) =
        if (!Files.exists(path)) (0L, None)
        else {
          val lines = new String(Files.readAllBytes(path), "UTF-8").split("\n", -1).toList
          val term = lines.headOption.flatMap(_.trim.toLongOption).getOrElse(0L)
          val vote = lines.lift(1).map(_.trim).filter(_.nonEmpty)
          (term, vote)
        }
      def save(t: Long, v: Option[String]): Unit = {
        val tmp = path.resolveSibling(path.getFileName.toString + ".tmp")
        Files.createDirectories(path.toAbsolutePath.getParent)
        Files.write(tmp, s"$t\n${v.getOrElse("")}\n".getBytes("UTF-8"))
        val _ = Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      }
    }
  }

  private def writeFrame(out: DataOutputStream, m: RaftMsg): Unit = {
    val bs = Codecs.writeCbor(m)
    out.writeInt(bs.length)
    out.write(bs)
    out.flush()
  }

  private def readFrame(in: DataInputStream): Either[String, RaftMsg] = {
    val len = in.readInt()
    if (len < 0 || len > 16 * 1024 * 1024) Left(s"frame length $len is not a frame")
    else {
      val bs = new Array[Byte](len)
      in.readFully(bs)
      Codecs.readCbor[RaftMsg](bs)
    }
  }

  /**
   * One real Raft node. `peers` names every OTHER node's address. A
   * background tick thread drives election timeouts (randomized per
   * node) and, once leading, periodic heartbeats/replication. Sends are
   * ONE-SHOT connections (connect, write one frame, close); Raft's own
   * retry-by-heartbeat tolerates a dropped send.
   *
   * `onCommit` receives each newly committed entry in log order;
   * `onRefused` a forwarded proposal's refusal, with the leader the
   * refusing node named; `onRestore` a snapshot installed from the
   * leader (its index and the engine's bytes), before any later commit.
   */
  final class Node(id: String, port: Int, peers: Map[String, (String, Int)],
                   tickMs: Long = 50, electionTimeoutMs: Long = 300,
                   heartbeatMs: Long = 100,
                   onCommit: (Long, RaftEntry) => Unit = (_, _) => (),
                   stable: Stable = Stable.memory(),
                   onRefused: (Long, Option[String]) => Unit = (_, _) => (),
                   onRestore: (Long, Array[Byte]) => Unit = (_, _) => ()) {

    private val lock = new Object
    // the two fields Raft's safety proof assumes on stable storage, read
    // back before the first message: a node that forgot its vote could
    // grant it twice in one term
    private var state = {
      val (term, vote) = stable.load()
      RaftState(id = id, currentTerm = term, votedFor = vote)
    }

    /** persist term/vote BEFORE anything the transition produced is sent */
    private def persisted(before: RaftState, after: RaftState): Unit =
      if (after.currentTerm != before.currentTerm || after.votedFor != before.votedFor)
        stable.save(after.currentTerm, after.votedFor)

    private var lastHeartbeatSent = 0L
    private var nextElectionAt = System.currentTimeMillis() + jitter()
    /** when a leader last spoke to this node: a pre-vote is refused while
     * that is within an election timeout (thesis §4.2.3) */
    private var lastHeard = 0L

    private def jitter(): Long = electionTimeoutMs + scala.util.Random.nextInt(electionTimeoutMs.toInt)

    /** the bootstrap configuration; a configuration entry overrides it */
    private val peerIds: Set[String] = peers.keySet
    /** where to reach each node — grows as configuration entries arrive,
     * never shrinks: a removed node may still be owed a last heartbeat */
    @volatile private var addresses: Map[String, (String, Int)] = peers

    /** the log's latest configuration entry changed: learn the addresses
     * it carries. Called under the lock */
    private def learned(before: RaftState, after: RaftState): Unit =
      // a configuration inside a snapshot carries no addresses
      if (after.configIndex != before.configIndex && after.configIndex > after.snapshotIndex)
        Cbor.read[Vector[Address]](after.log((after.configIndex - after.snapshotIndex - 1).toInt).data) match {
          case Right(as) => addresses = addresses ++ as.filter(_.id != id).map(a => a.id -> ((a.host, a.port)))
          case Left(_) => ()   // a configuration without addresses: reachable only as before
        }

    private val listener = new ServerSocket(port)
    @volatile private var closed = false

    Threads.spawn("okay2-persist-raft-accept")(() => acceptLoop())
    Threads.spawn("okay2-persist-raft-tick")(() => tickLoop())

    private def acceptLoop(): Unit =
      while (!closed) {
        try {
          val sock = listener.accept()
          Threads.spawn("okay2-persist-raft-conn")(() => handleConn(sock))
        } catch { case _: Throwable => () }   // closed, or a doomed accept
      }

    private def handleConn(sock: Socket): Unit =
      try {
        val in = new DataInputStream(new BufferedInputStream(sock.getInputStream))
        readFrame(in).foreach(onMessage)
      } catch { case _: Throwable => () }
      finally sock.close()

    /** the ONE state transition, network-driven. The engine's callbacks
     * run INSIDE the lock, in log order (two connections committing
     * adjacent ranges would otherwise race them); network I/O happens
     * OUTSIDE the lock */
    private def onMessage(msg: RaftMsg): Unit = {
      val toSend = lock.synchronized {
        val before = state.commitIndex
        val now = System.currentTimeMillis()
        val (ns, out) = Raft.handle(state, msg, peerIds, leaderFresh = now - lastHeard < electionTimeoutMs)
        persisted(state, ns)
        learned(state, ns)
        val installed = ns.restored != state.restored
        state = ns
        msg match {
          case _: RaftMsg.AppendEntries | _: RaftMsg.InstallSnapshot =>
            lastHeard = now
            nextElectionAt = now + jitter()
          case _ => ()
        }
        // after a snapshot the engine restarts at the snapshot's edge
        if (installed) onRestore(ns.snapshotIndex, ns.snapshotData)
        var i = if (installed) ns.snapshotIndex else before
        while (i < ns.commitIndex) {
          onCommit(i + 1, ns.log((i - ns.snapshotIndex).toInt))
          i += 1
        }
        out
      }
      msg match {
        case RaftMsg.Proposed(_, _, n, false, leader) => onRefused(n, Option(leader).filter(_.nonEmpty))
        case _ => ()
      }
      toSend.foreach(send)
    }

    private def send(o: RaftOut): Unit =
      addresses.get(o.to).foreach { case (host, p) =>
        try {
          val sock = new Socket()
          try {
            sock.connect(new java.net.InetSocketAddress(host, p), 200)
            writeFrame(new DataOutputStream(new BufferedOutputStream(sock.getOutputStream)), o.msg)
          } finally sock.close()
        } catch { case _: Throwable => () }   // best-effort: heartbeats/timeouts retry
      }

    private def tickLoop(): Unit =
      while (!closed) {
        Thread.sleep(tickMs)
        val toSend = lock.synchronized {
          val now = System.currentTimeMillis()
          if (state.role == RaftRole.Leader) {
            if (now - lastHeartbeatSent >= heartbeatMs) {
              lastHeartbeatSent = now
              Raft.replicate(state, peerIds)
            } else Vector.empty
          } else if (now >= nextElectionAt) {
            val (ns, out) = Raft.startElection(state, peerIds)
            persisted(state, ns)
            state = ns
            nextElectionAt = now + jitter()
            out
          } else Vector.empty
        }
        toSend.foreach(send)
      }

    def votedFor: Option[String] = lock.synchronized(state.votedFor)
    def isLeader: Boolean = lock.synchronized(state.role == RaftRole.Leader)
    def leaderId: Option[String] = lock.synchronized(state.leaderId)
    def currentTerm: Long = lock.synchronized(state.currentTerm)
    def commitIndex: Long = lock.synchronized(state.commitIndex)
    def logSnapshot: Vector[RaftEntry] = lock.synchronized(state.log)
    /** the cluster as this node currently counts it */
    def members: Set[String] = lock.synchronized(Raft.members(state, peerIds))
    def snapshotIndex: Long = lock.synchronized(state.snapshotIndex)

    /** log compaction (stage 2b): the engine has written its state machine
     * as of the APPLIED index `upTo` into `snapshot`; false when `upTo` is
     * not past the current snapshot or not committed */
    def compact(upTo: Long, snapshot: Array[Byte]): Boolean = lock.synchronized {
      val ns = Raft.compact(state, upTo, snapshot)
      val did = ns.snapshotIndex != state.snapshotIndex
      state = ns
      did
    }

    /** run `f` with this node's transitions paused, so an engine can read
     * its own state machine at a definite index and `compact` to it.
     * Reentrant. Keep it short */
    def quiesced[A](f: => A): A = lock.synchronized(f)

    /** a membership change (stage 2a): `cluster` is the WHOLE new cluster
     * with an address for each node; accepted only on the leader with no
     * earlier change uncommitted */
    def reconfigure(cluster: Map[String, (String, Int)]): Boolean = {
      val directory = Cbor.write(cluster.toVector.sortBy(_._1).map { case (i, a) => Address(i, a._1, a._2) })
      val toSend = lock.synchronized {
        Raft.reconfigure(state, peerIds, cluster.keySet, directory).map { case (ns, out) =>
          learned(state, ns); state = ns; out
        }
      }
      toSend match {
        case None => false
        case Some(out) => out.foreach(send); true
      }
    }

    /** the client seam on the current leader, or carried to it */
    def propose(data: Array[Byte]): Boolean = propose(0L, data)

    /** propose with the caller's own sequence: on the leader, appended and
     * replicated here; on a follower that KNOWS its leader, carried there
     * as a `Propose` — true means "on its way"; false means no leader is
     * known */
    def propose(n: Long, data: Array[Byte]): Boolean = {
      val toSend = lock.synchronized {
        if (state.role == RaftRole.Leader) {
          state = Raft.append(state, RaftEntry(state.currentTerm, data))
          Some(Raft.replicate(state, peerIds))
        } else state.leaderId.filter(_ != state.id).map(l =>
          Vector(RaftOut(l, RaftMsg.Propose(state.currentTerm, state.id, n, data))))
      }
      toSend match {
        case None => false
        case Some(out) => out.foreach(send); true
      }
    }

    def close(): Unit = {
      closed = true
      listener.close()
    }
  }
}
