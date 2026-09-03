package okay.obs

import okay.{!, Async, Pure, Resource}
import okay.given
import okay.codec.{Cbor, Schema}
import okay.http.{Http, Request, Response, Server, Transports}
import okay.jdbc.JdbcSql
import okay.persist.{MemoryStore, Policy, Topic}
import okay.sql.{Sql, Typed}

/**
 * The crossing the spec exists for: a request enters okay-http
 * carrying a traceparent, reaches through the Sql seam into H2, and
 * every span shares the inbound traceId with correct parentage — the
 * request can be FOLLOWED across the seams.
 */
class TestCrossing extends munit.FunSuite {
  // nio-port-scope (2026-09-03): this suite BINDS a real port, so its
  // result depends on what else on the machine is binding them — the
  // class of failure netty-ws-matrix-flake and nio-port-scope-flake
  // both were. Out of the default gate; `sbt integrationTest` runs it.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))


  final case class Row(n: Int)
  given Schema[Row] = Schema.derived

  test("http -> sql: one traceId, correct parentage, on the trace topic") {
    val topic = MemoryStore().topic("__trace", 1, Policy())
    // under the full matrix DriverManager sees another module's
    // loader first; naming the driver removes the race
    Class.forName("org.h2.Driver")
    val conn = java.sql.DriverManager.getConnection("jdbc:h2:mem:obs;DB_CLOSE_DELAY=-1")
    val st = conn.createStatement()
    st.execute("create table t(n int)"); st.execute("insert into t values (7)"); st.close()

    val route: PartialFunction[Request, Response ! Async] = {
      case r if r.url.startsWith("/q") =>
        okay.async {
          // one tracer per request; the header is the inbound edge
          val tracer = Tracer(topic)
          def h(name: String) = r.headers.collectFirst {
            case (k, v) if k.equalsIgnoreCase(name) => v }
          val n = tracer.root("GET /q", h("traceparent"), h("tracestate")) {
            tracer.span("sql update", Attr("db.system", "h2")) {
              val db: Sql = JdbcSql(conn)
              okay.!.run(Async.run[Long, Nothing](Typed.update[Row](db, "update t set n = ?")(Row(7))))
            }
          }
          Response(200, Nil, Http.one(n.toString.getBytes("UTF-8")))
        }
    }

    val inbound = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
    val http = Transports.http()
    Resource.run[Unit, Pure](Server.serve(0)(route).map { s =>
      val resp = Async.run[Response, Pure](http.send(
        Request.get(s"http://127.0.0.1:${Server.port(s)}/q", Seq("traceparent" -> inbound)))).runWith
      assertEquals(resp.status, 200)
    }).runWith

    val spans = topic.read(0, 0, 100) match
      case Topic.Read.Records(rs) => rs.flatMap(r => Cbor.read[Span](r.value).toOption)
      case _ => Vector.empty
    val sql = spans.find(_.name == "sql update").get
    val httpSpan = spans.find(_.name == "GET /q").get
    assertEquals(httpSpan.traceId, "4bf92f3577b34da6a3ce929d0e0e4736")
    assertEquals(httpSpan.parentId, Some("00f067aa0ba902b7"))
    assertEquals(sql.traceId, httpSpan.traceId)
    assertEquals(sql.parentId, Some(httpSpan.spanId))
    assertEquals(sql.attrs, Vector(Attr("db.system", "h2")))
    conn.close()
  }
}
