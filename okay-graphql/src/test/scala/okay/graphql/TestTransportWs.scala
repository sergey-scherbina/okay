package okay.graphql

import okay.{Async, AsyncCont, StreamCont}
import okay.codec.Json
import okay.one.row.*
import scala.collection.mutable

class TestTransportWs extends okay.testkit.Munit.Diagnosed:
  /** a stream of ticks a test pushes by hand: the subscription stays open until the test ends it */
  final class Ticks:
    private var k: Option[Option[Int] => Unit] = None
    private var n = 0
    def src: StreamCont.Src[Int] = StreamCont.unfold(0)(_ =>
      AsyncCont.await[Option[(Seq[Int], Int)]](cb => k = Some(o => cb(o.map(i => (Seq(i), i))))).at[Async +: Pure])
    def tick(): Unit = { n += 1; k.foreach(_(Some(n))) }
    def end(): Unit = k.foreach(_(None))

  private def connection(ticks: Ticks) =
    val sent = mutable.ArrayBuffer.empty[String]
    var closed: Option[TransportWs.Close] = None
    val schema = Schema(ObjectType[Unit]("Query")(List(Field("hello", Scalar.string)(_ => "world"))),
      subscription = Some(SubscriptionType("Subscription")(List(Subscription("tick", Scalar.int)(_ => ticks.src)))))
    val c = TransportWs.Connection(schema, s => { note(s"> $s"); sent += s }, c => closed = Some(c),
      accept = p => p != Json.parse("""{"token":"bad"}"""))
    (c, sent, () => closed)

  test("init, ack, a query answered as next and complete, ping/pong") {
    val (c, sent, _) = connection(Ticks())
    c.receive("""{"type":"connection_init"}""")
    c.receive("""{"type":"ping"}""")
    c.receive("""{"id":"1","type":"subscribe","payload":{"query":"{ hello }"}}""")
    assertEquals(sent.toList, List("""{"type":"connection_ack"}""", """{"type":"pong"}""",
      """{"id":"1","type":"next","payload":{"data":{"hello":"world"}}}""", """{"type":"complete","id":"1"}"""))
  }

  test("a subscription: an event per tick, completed by the server's end or by the client's complete") {
    val ticks = Ticks()
    val (c, sent, _) = connection(ticks)
    c.receive("""{"type":"connection_init"}""")
    c.receive("""{"id":"s","type":"subscribe","payload":{"query":"subscription { tick }"}}""")
    ticks.tick(); ticks.tick()
    assertEquals(c.running, Set("s"))
    ticks.end()
    assertEquals(sent.toList.drop(1), List("""{"id":"s","type":"next","payload":{"data":{"tick":1}}}""",
      """{"id":"s","type":"next","payload":{"data":{"tick":2}}}""", """{"type":"complete","id":"s"}"""))
    val again = Ticks()
    val (c2, sent2, _) = connection(again)
    c2.receive("""{"type":"connection_init"}""")
    c2.receive("""{"id":"t","type":"subscribe","payload":{"query":"subscription { tick }"}}""")
    again.tick()
    c2.receive("""{"id":"t","type":"complete"}""")
    again.tick()
    assertEquals(c2.running, Set.empty[String])
    assertEquals(sent2.toList.drop(1), List("""{"id":"t","type":"next","payload":{"data":{"tick":1}}}"""))
  }

  test("refusals: an invalid document is an error message; the protocol's closes") {
    val (c, sent, closed) = connection(Ticks())
    c.receive("""{"id":"x","type":"subscribe","payload":{"query":"{ hello }"}}""")
    assertEquals(closed(), Some(TransportWs.Close(4401, "Unauthorized")))
    val (c2, sent2, closed2) = connection(Ticks())
    c2.receive("""{"type":"connection_init"}""")
    c2.receive("""{"id":"e","type":"subscribe","payload":{"query":"{ nope }"}}""")
    assertEquals(sent2.last, """{"id":"e","type":"error","payload":[{"message":"Query has no field nope","locations":[{"line":1,"column":3}]}]}""")
    c2.receive("""{"type":"connection_init"}""")
    assertEquals(closed2(), Some(TransportWs.Close(4429, "Too many initialisation requests")))
    val (c3, _, closed3) = connection(Ticks())
    c3.receive("""{"type":"connection_init","payload":{"token":"bad"}}""")
    assertEquals(closed3(), Some(TransportWs.Close(4403, "Forbidden")))
    val (c4, _, closed4) = connection(Ticks())
    c4.receive("""not json""")
    assertEquals(closed4().map(_.code), Some(4400))
    assertEquals(sent.toList, Nil)
  }

  test("a subscriber id twice is a close 4409") {
    val (c, _, closed) = connection(Ticks())
    c.receive("""{"type":"connection_init"}""")
    c.receive("""{"id":"s","type":"subscribe","payload":{"query":"subscription { tick }"}}""")
    c.receive("""{"id":"s","type":"subscribe","payload":{"query":"subscription { tick }"}}""")
    assertEquals(closed(), Some(TransportWs.Close(4409, "Subscriber for s already exists")))
  }
