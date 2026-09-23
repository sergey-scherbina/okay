package scala2probe

import scala.collection.mutable.ArrayBuffer
import okay.actor.{Reply, Supervise}
import okay.http.Request
import okay.obs.{Log, Tracer}
import okay.ops.Red
import okay.outbox.{Dialect, Inbox, Outbox}
import okay.persist.{MemoryStore, Topic}
import okay.scala2._

object ServicesModel {
  sealed trait Msg
  final case class Add(n: Int) extends Msg
  final case class Get(reply: Reply[Int]) extends Msg
  case object Boom extends Msg

  val counter: (Int, Msg) => Eff[Async, Int] = {
    case (n, Add(k)) => Eff.pure(n + k)
    case (n, Get(reply)) => Async.delay { reply(n); n }
    case (_, Boom) => Async.delay(throw new IllegalStateException("boom"))
  }
}

/** okay-actor, okay-outbox, okay-obs, okay-ops from Scala 2.13 (specs/scala2-facade.md, stage 15.8) */
class TestServicesFromScala2 extends munit.FunSuite {
  import ServicesModel._

  test("an actor: tell changes its state, ask reads it, stop ends it") {
    val prog = for {
      actor <- Actors.spawn(0)(counter)
      _ <- Actors.tell(actor, Add(2))
      _ <- Actors.tell(actor, Add(3))
      total <- Actors.ask[Msg, Int](actor, 1000)(Get(_))
      _ <- Actors.stop(actor)
      after <- Actors.tell(actor, Add(1))
    } yield (total, after)
    assertEquals(Eff.runAsync(prog), (Some(5), false))
  }

  test("a supervised actor restarts from a fresh state when a message throws") {
    val prog = for {
      actor <- Actors.spawn[Int, Msg](0, Supervise.Restart(() => 100), 16)(counter)
      _ <- Actors.tell(actor, Add(7))
      _ <- Actors.tell(actor, Boom)
      _ <- Actors.tell(actor, Add(1))
      total <- Actors.ask[Msg, Int](actor, 1000)(Get(_))
      _ <- Actors.stop(actor)
    } yield total
    assertEquals(Eff.runAsync(prog), Some(101))
  }

  test("an outbox: the message is written with the change, then relayed once; an inbox runs a message once") {
    val db = Db.jdbc(new org.h2.Driver().connect("jdbc:h2:mem:s2outbox;DB_CLOSE_DELAY=-1", new java.util.Properties()))
    val outbox = new Outbox()
    val inbox = new Inbox()
    val store = new MemoryStore
    var handled = 0
    val prog = for {
      _ <- db.update(outbox.ddl(Dialect.H2))
      _ <- db.update(inbox.ddl(Dialect.H2))
      _ <- Outboxes.enqueue(outbox, db, "orders", "order-1".getBytes)
      waiting <- Outboxes.pending(outbox, db)
      relayed <- Outboxes.relayOnce(outbox, db, store)
      again <- Outboxes.relayOnce(outbox, db, store)
      left <- Outboxes.pending(outbox, db)
      first <- Outboxes.once(inbox, db, "msg-1")(Async.delay { handled += 1; "done" })
      second <- Outboxes.once(inbox, db, "msg-1")(Async.delay { handled += 1; "done" })
    } yield (waiting, relayed, again, left, first, second)
    assertEquals(Eff.runAsync(prog), (1L, 1, 0, 0L, Some("done"), None))
    assertEquals(handled, 1)
    Persist.topic(store, "orders").read(0, 0L, 10) match {
      case Topic.Read.Records(rs) => assertEquals(rs.map(r => new String(r.value)), Vector("order-1"))
      case other => fail(other.toString)
    }
  }

  test("log lines go to a sink as they are said, below the minimum dropped, stamped") {
    val lines = ArrayBuffer.empty[Log.Line]
    val work: Eff[Writer[Log.Line], Int] = for {
      _ <- Logs.debug("noise")
      _ <- Logs.info("started", "job" -> "42")
      _ <- Logs.failure("failed", new IllegalStateException("disk"))
    } yield 7
    val answer = Eff.run(Logs.to[Any, Int](l => lines += l, Log.Level.Info, () => 1000L)(work))
    assertEquals(answer, 7)
    assertEquals(lines.map(l => (l.level, l.message, l.at)).toList, List((Log.Level.Info, "started", 1000L), (Log.Level.Error, "failed", 1000L)))
    assert(lines(1).fields.exists(a => a.key == "error.message" && a.value == "disk"), lines(1).fields)
  }

  test("a span around a program is written to the tracer's topic") {
    val store = new MemoryStore
    val spans = Persist.topic(store, "spans")
    val tracer = new Tracer(spans)
    assertEquals(Eff.runAsync(Tracing.span(tracer, "work", "k" -> "v")(Async.delay(21 * 2))), 42)
    spans.read(0, 0L, 10) match {
      case Topic.Read.Records(rs) => assertEquals(rs.size, 1)
      case other => fail(other.toString)
    }
  }

  test("ops routes answer health in the http facade's terms; a RED meter counts what it wraps") {
    val store = new MemoryStore
    val health = Eff.runAsync(Operations.routes(store)(Request.get("/healthz")))
    assertEquals((health.status, health.text), (200, "live=true"))
    val red = new Red("api")
    val app = Operations.measured(red, _ => "hello") { case _ => Eff.pure(Response.text("hi")) }
    assertEquals(Eff.runAsync(app(Request.get("/hello"))).status, 200)
    assertEquals(red.stats.series.map(_.route), Vector("hello"))
  }
}
