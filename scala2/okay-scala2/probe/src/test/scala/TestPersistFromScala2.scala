package scala2probe

import okay.codec.Schema
import okay.persist.{Ack, FileStore, MemoryStore, Offsets, Snapshots, Topic, Typed}
import okay.scala2._

object PersistModel {
  final case class Deposit(account: String, amount: Long)
  object Deposit {
    implicit val schema: Schema[Deposit] =
      Schemas.product2("Deposit", "account", "amount")(Deposit.apply)(d => (d.account, d.amount))
  }
}

/** okay-persist from Scala 2.13 (specs/scala2-facade.md, stage 15.2) */
class TestPersistFromScala2 extends munit.FunSuite {
  import PersistModel._

  test("the engine is used directly: append, read, offsets, snapshots") {
    val store = new MemoryStore
    val t = Persist.topic(store, "events")
    assertEquals(t.append("k".getBytes, "a".getBytes, Ack.Durable), 0L)
    assertEquals(t.append("k".getBytes, "b".getBytes, Ack.Durable), 1L)
    t.read(0, 0L, 10) match {
      case Topic.Read.Records(rs) => assertEquals(rs.map(r => new String(r.value)), Vector("a", "b"))
      case other => fail(other.toString)
    }
    val offsets = Offsets(store, "__offsets")
    offsets.commit("reader", "events", 0, 2L, Ack.Durable)
    assertEquals(offsets.committed("reader", "events", 0), Some(2L))
    val snaps = Snapshots(store, "__snapshots", 1)
    snaps.put("s".getBytes, "state".getBytes, Ack.Durable)
    assertEquals(snaps.latest("s".getBytes).map(r => new String(r.value)), Some("state"))
  }

  test("the typed view decodes values by their Schema") {
    val typed = Persist.typed[Deposit](Persist.topic(new MemoryStore, "deposits"))
    typed.append("acct-1".getBytes, Deposit("acct-1", 100), Ack.Durable)
    typed.read(0, 0L, 10) match {
      case Typed.Read.Records(rs) => assertEquals(rs.collect { case Typed.Decoded.Ok(_, _, _, d) => d }, Vector(Deposit("acct-1", 100)))
      case other => fail(other.toString)
    }
  }

  test("a FileStore keeps its records across a reopen") {
    val dir = java.nio.file.Files.createTempDirectory("okay-s2-persist")
    val first = FileStore.open(dir)
    Persist.topic(first, "log").append("k".getBytes, "kept".getBytes, Ack.Durable)
    first.close()
    val again = FileStore.open(dir)
    try assertEquals(Eff.runAsync(Persist.stream(Persist.topic(again, "log"), 0, 0L).map(r => new String(r.value)).runCollect), Vector("kept"))
    finally again.close()
  }

  test("tail sees records appended after it started, and take ends it") {
    val t = Persist.topic(new MemoryStore, "live")
    t.append("k".getBytes, "0".getBytes, Ack.Durable)
    val prog = for {
      writer <- Async.fork(Async.sleep(30).flatMap(_ => Async.delay { t.append("k".getBytes, "1".getBytes, Ack.Durable); t.append("k".getBytes, "2".getBytes, Ack.Durable); () }))
      seen <- Persist.tail(t, 0, 0L, pollMillis = 5).map(r => new String(r.value)).take(3).runCollect
      _ <- writer.join
    } yield seen
    assertEquals(Eff.runAsync(prog), Vector("0", "1", "2"))
  }
}
