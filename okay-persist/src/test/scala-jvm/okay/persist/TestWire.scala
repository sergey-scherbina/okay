package okay.persist

import okay.{!, Async}
import okay.given
import munit.FunSuite

/**
 * The wire over loopback (specs/persist.md, "The wire"): the
 * documented surface exercised end to end — handshake with the
 * capability list as the offer, the four calls 1:1 with the Topic
 * SPI, TooEarly passing through unchanged, refusals by name.
 */
class TestWire extends FunSuite:

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  /** two tokens, two capability sets — the auth function is the plug */
  def server(store: Store): Wire.Server =
    Wire.Server(store, {
      case "reader-of-events" => Some(Set("events"))
      case "the-admin" => Some(Set("events", "audit"))
      case _ => None
    })

  test("the handshake offers exactly what the token may see") {
    val srv = server(MemoryStore())
    try
      val events = Wire.Remote.connect("127.0.0.1", srv.port, "reader-of-events")
      assertEquals(events.topics, Vector("events"))
      events.close()
      val admin = Wire.Remote.connect("127.0.0.1", srv.port, "the-admin")
      assertEquals(admin.topics, Vector("audit", "events"))
      admin.close()
    finally srv.close()
  }

  test("a token the node does not accept refuses by name, not by silence") {
    val srv = server(MemoryStore())
    try
      val e = intercept[Wire.WireRefused](
        Wire.Remote.connect("127.0.0.1", srv.port, "stranger"))
      assert(e.reason.contains("token"), e.reason)
    finally srv.close()
  }

  test("append and read round-trip bytes exactly; offsets and timestamps survive") {
    val store = MemoryStore()
    val srv = server(store)
    try
      val c = Wire.Remote.connect("127.0.0.1", srv.port, "the-admin")
      try
        val o0 = run(c.append("events", 0, bytes("k0"), bytes("v0")))
        val o1 = run(c.append("events", 0, Array.empty, Array[Byte](0, 1, -1)))
        assertEquals((o0, o1), (0L, 1L))
        assertEquals(run(c.end("events", 0)), 2L)
        assertEquals(run(c.begin("events", 0)), 0L)

        run(c.read("events", 0, 0L, 10)) match
          case Topic.Read.Records(rs) =>
            assertEquals(rs.map(_.offset), Vector(0L, 1L))
            assertEquals(str(rs(0).key), "k0")
            assertEquals(str(rs(0).value), "v0")
            assertEquals(rs(1).value.toList, List[Byte](0, 1, -1))
            assert(rs(0).timestamp > 0L)
          case other => fail(s"unexpected $other")

        // the server's store saw the same log — one truth
        assertEquals(store.topic("events").end(0), 2L)
      finally c.close()
    finally srv.close()
  }

  test("TooEarly crosses the wire unchanged: dropped history stays an answer") {
    val store = MemoryStore()
    val t = store.topic("events", 1, Policy(retainBytes = 340))
    (0 until 50).foreach(i => t.append(0, Array.empty, bytes(s"payload-$i"), Ack.Received))
    val b = t.begin(0)
    assert(b > 0L)
    val srv = server(store)
    try
      val c = Wire.Remote.connect("127.0.0.1", srv.port, "reader-of-events")
      try
        run(c.read("events", 0, 0L, 10)) match
          case Topic.Read.TooEarly(at) => assertEquals(at, b)
          case other => fail(s"expected TooEarly, got $other")
      finally c.close()
    finally srv.close()
  }

  test("a topic outside the capability list refuses by name; the granted one still works") {
    val srv = server(MemoryStore())
    try
      val c = Wire.Remote.connect("127.0.0.1", srv.port, "reader-of-events")
      try
        val e = intercept[Wire.WireRefused](
          run(c.append("audit", 0, Array.empty, bytes("sneak"))))
        assert(e.reason.contains("audit"), e.reason)
        // the connection survives the refusal
        assertEquals(run(c.append("events", 0, Array.empty, bytes("fine"))), 0L)
      finally c.close()
    finally srv.close()
  }

  test("a version the node does not speak refuses in the handshake") {
    val srv = server(MemoryStore())
    try
      // forge a Hello from the future by hand
      val sock = java.net.Socket("127.0.0.1", srv.port)
      val out = java.io.DataOutputStream(sock.getOutputStream)
      val in = java.io.DataInputStream(sock.getInputStream)
      val hello = okay.codec.Cbor.write[Wire.Req](Wire.Req.Hello(99, "the-admin"))
      out.writeInt(hello.length); out.write(hello); out.flush()
      val len = in.readInt(); val bs = new Array[Byte](len); in.readFully(bs)
      okay.codec.Cbor.read[Wire.Resp](bs) match
        case Right(Wire.Resp.Refused(r)) => assert(r.contains("version"), r)
        case other => fail(s"expected Refused, got $other")
      sock.close()
    finally srv.close()
  }

  test("the tail shape works remotely: end, then read from there sees the later append") {
    val store = MemoryStore()
    val srv = server(store)
    try
      val c = Wire.Remote.connect("127.0.0.1", srv.port, "the-admin")
      try
        run(c.append("events", 0, Array.empty, bytes("v0")))
        val caughtUp = run(c.end("events", 0))
        // another writer appends behind our back
        store.topic("events").append(0, Array.empty, bytes("late"), Ack.Durable)
        run(c.read("events", 0, caughtUp, 10)) match
          case Topic.Read.Records(rs) => assertEquals(rs.map(r => str(r.value)), Vector("late"))
          case other => fail(s"unexpected $other")
      finally c.close()
    finally srv.close()
  }
