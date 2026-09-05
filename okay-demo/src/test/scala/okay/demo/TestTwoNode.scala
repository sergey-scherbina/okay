package okay.demo

import okay.codec.Json
import okay.codec.Json.*
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.URI
import java.nio.file.Files

/**
 * specs/demo-chat.md, "Two nodes over one shared log" (demo-two-
 * nodes) — TWO REAL PROCESSES, not two threads in one JVM (Election
 * itself is already proven in-process, specs/consensus.md; this
 * proves the demo's OWN wiring: real OS processes, a real shared
 * FileStore directory, a real kill).
 */
class TestTwoNode extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  val client = HttpClient.newHttpClient()
  private val javaBin =
    java.nio.file.Path.of(sys.props("java.home"), "bin", "java").toString
  private val cp = sys.props("java.class.path")

  private def spawn(port: Int, node: String, logDir: String): Process =
    val pb = new ProcessBuilder(javaBin, "-cp", cp, "okay.demo.ChatDemo")
    val env = pb.environment()
    env.put("OKAY_CHAT_PORT", port.toString)
    env.put("OKAY_CHAT_NODE", node)
    env.put("OKAY_CHAT_LOG", logDir)
    // the LOG is shared (that is the whole point); the STORE is
    // per-node, a local projection kept in sync by replay — sharing
    // a sqlite file here was never the design and races the two
    // processes against each other on schema creation
    // the board IS its log now: the projection and the record are one
    // object, so both nodes must be pointed at the SHARED log rather
    // than at a private in-memory store beside it
    env.put("OKAY_CHAT_DB", logDir)
    env.put("OKAY_CHAT_TICK_MS", "200")
    env.put("OKAY_CHAT_LEASE_MS", "1000")
    pb.redirectErrorStream(true)
    pb.redirectOutput(ProcessBuilder.Redirect.DISCARD)
    pb.start()

  private def whoami(port: Int): Option[(String, Boolean)] =
    try
      val res = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/whoami"))
          .timeout(java.time.Duration.ofMillis(500)).GET().build(),
        HttpResponse.BodyHandlers.ofString())
      if res.statusCode() != 200 then None
      else Json.parse(res.body()) match
        case JObj(fs) =>
          for
            leader <- fs.collectFirst { case ("leader", JStr(l)) => l }
            isLeader <- fs.collectFirst { case ("isLeader", JBool(b)) => b }
          yield (leader, isLeader)
        case _ => None
    catch case _: Throwable => None

  private def waitUntil(timeoutMs: Long)(cond: => Boolean): Boolean =
    val deadline = System.currentTimeMillis() + timeoutMs
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(50)
    cond

  private def post(port: Int, body: String): (Int, String) =
    val res = client.send(
      HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/chat"))
        .header("content-type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(body)).build(),
      HttpResponse.BodyHandlers.ofString())
    (res.statusCode(), res.body())

  private def boardJson(port: Int): String =
    client.send(HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/board.json")).GET().build(),
      HttpResponse.BodyHandlers.ofString()).body()

  test("two real processes over one shared log: one leader, the follower serves reads, a kill fails over") {
    val logDir = Files.createTempDirectory("okay-two-node").toString
    // the log does NOT exist yet, deliberately: two processes opening
    // one empty directory together is what found the FileStore race
    // (filestore-first-segment-race), and this is the test that keeps
    // it found. Pre-creating the log here would hide it again
    val portA = 18091
    val portB = 18092
    val procA = spawn(portA, "a", logDir)
    val procB = spawn(portB, "b", logDir)
    try
      assert(waitUntil(15000)(whoami(portA).isDefined && whoami(portB).isDefined),
        "both processes must answer /whoami")

      // exactly one leader, and both nodes agree on who it is
      val (leaderFromA, _) = whoami(portA).get
      val (leaderFromB, _) = whoami(portB).get
      assertEquals(leaderFromA, leaderFromB, "both nodes must agree on the leader")
      val leaderPort = if leaderFromA == "a" then portA else portB
      val followerPort = if leaderFromA == "a" then portB else portA

      // the follower refuses a write, naming the leader; its reads keep answering
      val (followerStatus, followerBody) =
        post(followerPort, """{"messages":[{"role":"user","content":"/board добавь класть плитку"}]}""")
      assertEquals(followerStatus, 503, followerBody)
      assert(followerBody.contains(leaderFromA), followerBody)
      assertEquals(client.send(HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$followerPort/board.json")).GET().build(),
        HttpResponse.BodyHandlers.ofString()).statusCode(), 200, "the follower must still serve reads")

      // the leader accepts the write
      val (leaderStatus, leaderBody) =
        post(leaderPort, """{"messages":[{"role":"user","content":"/board добавь класть плитку"}]}""")
      assertEquals(leaderStatus, 200, leaderBody)

      // within one tick, the FOLLOWER's market reflects the leader's write
      assert(waitUntil(5000)(boardJson(followerPort).contains("плитку")),
        s"the follower never caught up: ${boardJson(followerPort)}")

      // kill the leader; the follower must take over
      val killedNode = leaderFromA
      if killedNode == "a" then procA.destroyForcibly(): Unit else procB.destroyForcibly(): Unit
      assert(waitUntil(10000)(whoami(followerPort).exists(_._2)),
        s"the surviving node never took the seat: ${whoami(followerPort)}")

      // the board it already held from polling is immediately servable,
      // and it now accepts writes too — the showcase's own claim
      assert(boardJson(followerPort).contains("плитку"), "the board did not survive the kill")
      val (newLeaderStatus, newLeaderBody) =
        post(followerPort, """{"messages":[{"role":"user","content":"/board добавь покрасить стену"}]}""")
      assertEquals(newLeaderStatus, 200, newLeaderBody)
    finally
      procA.destroyForcibly(): Unit
      procB.destroyForcibly(): Unit
  }
}
