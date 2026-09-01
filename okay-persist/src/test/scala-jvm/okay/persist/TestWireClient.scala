package okay.persist

import okay.{!, Async}
import okay.given
import munit.FunSuite

/**
 * The SHARED client over the Net seam against the jvm server
 * (specs/net.md): the same code that talks to a Node-scripted
 * server in the JS suite talks to Wire.Server here — one client,
 * every platform, one protocol source.
 */
class TestWireClient extends FunSuite:

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")
  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def server(store: Store): Wire.Server =
    Wire.Server(store, {
      case "reader" => Some(Set("events"))
      case "admin" => Some(Set("events", "audit"))
      case _ => None
    })

  test("the shared client passes the wire battery against the jvm server") {
    val store = MemoryStore()
    val srv = server(store)
    try
      val c = run(WireProtocol.Client.connect("127.0.0.1", srv.port, "admin"))
      try
        assertEquals(c.topics, Vector("audit", "events"))
        assertEquals(run(c.append("events", 0, bytes("k"), bytes("v0"))), 0L)
        assertEquals(run(c.end("events", 0)), 1L)
        run(c.read("events", 0, 0L, 10)) match
          case Topic.Read.Records(rs) =>
            assertEquals(rs.map(r => str(r.value)), Vector("v0"))
          case other => fail(s"unexpected $other")
        // a refusal by name, and the connection survives it
        val e = intercept[WireProtocol.WireRefused](
          run(c.append("secrets", 0, Array.empty, bytes("no"))))
        assert(e.reason.contains("secrets"), e.reason)
        assertEquals(run(c.append("events", 0, Array.empty, bytes("v1"))), 1L)
      finally c.close()
    finally srv.close()
  }

  test("a refused token throws by name at connect") {
    val srv = server(MemoryStore())
    try
      val e = intercept[WireProtocol.WireRefused](
        run(WireProtocol.Client.connect("127.0.0.1", srv.port, "stranger")))
      assert(e.reason.contains("token"), e.reason)
    finally srv.close()
  }
