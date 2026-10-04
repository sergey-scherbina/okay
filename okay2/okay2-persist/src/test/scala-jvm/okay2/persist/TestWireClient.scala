package okay2.persist

import okay2.platform._

/**
 * The SHARED client over the Net seam against the JVM server
 * (okay-persist's TestWireClient; specs/net.md): the same code that
 * talks to a Node-scripted server in the JS suite talks to Wire.Server
 * here. `Live`: it binds a real port.
 */
class TestWireClient extends WireRun {

  def server(store: Store): Wire.Server =
    new Wire.Server(store, {
      case "reader" => Some(Set("events"))
      case "admin" => Some(Set("events", "audit"))
      case _ => None
    })

  test("the shared client passes the wire battery against the jvm server") {
    val store = new MemoryStore
    val srv = server(store)
    try {
      val c = run(WireProtocol.Client.connect("127.0.0.1", srv.port, "admin"))
      try {
        assertEquals(c.topics, Vector("audit", "events"))
        assertEquals(run(c.append("events", 0, bytes("k"), bytes("v0"))), 0L)
        assertEquals(run(c.end("events", 0)), 1L)
        run(c.read("events", 0, 0L, 10)) match {
          case Topic.Read.Records(rs) => assertEquals(rs.map(r => str(r.value)), Vector("v0"))
          case other => fail(s"unexpected $other")
        }
        // a refusal by name, and the connection survives it
        val e = intercept[WireProtocol.WireRefused](run(c.append("secrets", 0, Array.empty[Byte], bytes("no"))))
        assert(e.reason.contains("secrets"), e.reason)
        assertEquals(run(c.append("events", 0, Array.empty[Byte], bytes("v1"))), 1L)
      } finally c.close()
    } finally srv.close()
  }

  test("a refused token throws by name at connect") {
    val srv = server(new MemoryStore)
    try {
      val e = intercept[WireProtocol.WireRefused](run(WireProtocol.Client.connect("127.0.0.1", srv.port, "stranger")))
      assert(e.reason.contains("token"), e.reason)
    } finally srv.close()
  }
}
