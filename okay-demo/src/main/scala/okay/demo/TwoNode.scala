package okay.demo

import okay.{Async, !, pure}
import okay.http.{Http, Method, Request, Response}
import okay.llm.Transport
import okay.conf.Secrets
import okay.persist.{Election, FileStore, Policy}
import okay.codec.Json
import okay.codec.Json.*
import java.nio.file.Path
import java.nio.charset.StandardCharsets.UTF_8

/**
 * Two real ChatDemo processes over one shared log (specs/demo-chat.
 * md, demo-two-nodes). Failover itself is proven at okay-persist's
 * layer (specs/consensus.md) — this is that machinery made
 * consumer-visible, not new election work.
 *
 * A real constraint stated, not hidden: FileStore.open scans its
 * directory ONCE; a process that never writes a topic never sees
 * another process's later records on it. So this is a POLL, not a
 * live tail — every tick reopens the shared directory fresh, and
 * the tick interval is the staleness bound.
 */
final class TwoNode(root: Path, val node: String,
                    tickMs: Long = 500, leaseMillis: Long = 5000)
                   (using Transport, Secrets) {


  @volatile private var leaderOf: Option[String] = None

  /** one tick: reopen the shared directory fresh, decide leadership
   * from ITS control topic, keep the seat's lease alive when held,
   * and — only when NOT leading — re-derive the market from the
   * freshly-reopened log (the SAME function POST /admin/replay
   * calls, reused rather than reinvented). No store handle survives
   * past this method. */
  private def tick(): Unit =
    val fresh = ChatDemo.storeOf(root.toString)
    try
      val election = Election(fresh.topic("__control", 1, Policy.default), node, leaseMillis)
      if election.vacant(0) then election.tryTakeover(0): Unit
      val decided = election.leader(0).map(_._2)
      leaderOf = decided
      if decided.contains(node) then election.heartbeat()
      else
        // a FOLLOWER carries no authority of its own: it drops its
        // projection and derives it again from the leader's log, which
        // is the whole claim of a log-first design under two nodes
        ChatDemo.board.replay(): Unit
    finally fresh match
      case f: FileStore => f.close()
      case _ => ()   // MemoryStore (a :memory: log): nothing to release

  def isLeader: Boolean = leaderOf.contains(node)
  def leaderNode: Option[String] = leaderOf

  // the first tick runs SYNCHRONOUSLY — a follower boots already
  // caught up, not empty until the first timer fires. A cold start
  // where BOTH nodes provision a brand-new shared directory at once
  // can race on a topic's FIRST segment file (FileStore has no
  // cross-process locking, specs/persist.md) — this must not crash
  // main; the next tick, ~tickMs later, finds the segment the OTHER
  // node just created and proceeds normally
  try tick() catch case e: Throwable => e.printStackTrace()

  private val thread = new Thread(() =>
    while true do
      Thread.sleep(tickMs)
      try tick()
      catch case e: Throwable => e.printStackTrace()
  , s"two-node-$node")
  thread.setDaemon(true)
  thread.start()
}

object TwoNode {

  /** the write gate: a POST from a non-leader is refused 503 naming
   * the current leader — the smallest honest cut, the WHOLE route
   * table wrapped once rather than each write case picked out by
   * hand. Every GET passes through untouched, so /market,
   * /market.json, and the /events streams keep serving from
   * whichever process is up, leader or not. Also answers GET
   * /whoami — the only way outside the process to observe which
   * node holds the seat. */
  def leaderGated(twoNode: TwoNode)
                 (inner: PartialFunction[Request, Response ! Async])
  : PartialFunction[Request, Response ! Async] = new PartialFunction[Request, Response ! Async] {
    private val whoami: PartialFunction[Request, Response ! Async] = {
      case r if r.method == Method.Get && r.url == "/whoami" =>
        pure(Response(200, Seq("content-type" -> "application/json"),
          Http.one(Json.print(JObj(Vector(
            "node" -> JStr(twoNode.node),
            "leader" -> twoNode.leaderNode.fold[Json](JNull)(JStr(_)),
            "isLeader" -> JBool(twoNode.isLeader)))).getBytes(UTF_8))))
    }
    private val gated = whoami.orElse(inner)
    def isDefinedAt(r: Request): Boolean = gated.isDefinedAt(r)
    def apply(r: Request): Response ! Async =
      if r.method == Method.Post && !twoNode.isLeader then
        pure(Response(503, Seq("content-type" -> "text/plain; charset=utf-8"),
          Http.one(s"not the leader — current: ${twoNode.leaderNode.getOrElse("none")}"
            .getBytes(UTF_8))))
      else gated(r)
  }
}
