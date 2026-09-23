package scala2probe

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.ArraySeq
import okay.blob.Fs
import okay.cache.{Cache, Invalidations, Regime}
import okay.codec.Schema
import okay.docs.{Cond, PutResult}
import okay.persist.{Ack, FileStore, MemoryStore}
import okay.scala2._

object StoresModel {
  final case class Person(name: String, city: String)
  object Person {
    implicit val schema: Schema[Person] =
      Schemas.product2("Person", "name", "city")(Person.apply)(p => (p.name, p.city))
  }
}

/** okay-cache, okay-blob, okay-docs from Scala 2.13 (specs/scala2-facade.md, stage 15.4) */
class TestStoresFromScala2 extends munit.FunSuite {
  import StoresModel._

  test("a cache loads a missing key once, however many fibers ask") {
    val cache = Cache.memory[String, Int](Regime.Invalidated, 100)
    val loads = new AtomicInteger
    val slowLoad = (k: String) => Async.sleep(20).map { _ => loads.incrementAndGet(); k.length }
    val prog = for {
      a <- Async.fork(Caches.getOrLoad(cache, "hello")(slowLoad))
      b <- Async.fork(Caches.getOrLoad(cache, "hello")(slowLoad))
      x <- a.join
      y <- b.join
      cached <- Caches.get(cache, "hello")
    } yield (x, y, cached)
    assertEquals(Eff.runAsync(prog), (5, 5, Some(5)))
    assertEquals(loads.get, 1)
  }

  test("writeThrough commits, then invalidates; drain invalidates what another node published") {
    val cache = Cache.memory[String, String](Regime.Invalidated, 100)
    val prog = for {
      _ <- Caches.put(cache, "user:1", "old")
      _ <- Caches.writeThrough(cache, "user:1")(Async.delay("written"))
      afterWrite <- Caches.get(cache, "user:1")
    } yield afterWrite
    assertEquals(Eff.runAsync(prog), None)

    val topic = Persist.topic(new MemoryStore, "invalidations")
    Eff.runAsync(Caches.put(cache, "user:2", "stale"))
    Invalidations.append(topic, "user:2")
    val next = Eff.runAsync(Caches.drain(topic, cache, (k: String) => k, 0L))
    assertEquals((next, Eff.runAsync(Caches.get(cache, "user:2"))), (1L, None))
  }

  test("a blob store: bytes in and out, an absent key named, a listing by prefix") {
    val blob = Fs(Files.createTempDirectory("okay-s2-blob"))
    val prog = for {
      _ <- Blobs.putBytes(blob, "reports/a.txt", "alpha".getBytes)
      _ <- Blobs.put(blob, "reports/b.txt", Source(ArraySeq.unsafeWrapArray("be".getBytes), ArraySeq.unsafeWrapArray("ta".getBytes)))
      _ <- Blobs.putBytes(blob, "other/c.txt", "gamma".getBytes)
      a <- Blobs.getBytes(blob, "reports/a.txt")
      b <- Blobs.getBytes(blob, "reports/b.txt")
      missing <- Blobs.getBytes(blob, "reports/none.txt")
      keys <- Blobs.list(blob, "reports/").map(_.key).runCollect
    } yield (a.map(new String(_)), b.map(new String(_)), missing.isLeft, keys)
    assertEquals(Eff.runAsync(prog), (Right("alpha"), Right("beta"), true, Vector("reports/a.txt", "reports/b.txt")))
  }

  test("a blob streams out in chunks, head sees its size, delete is idempotent") {
    val blob = Fs(Files.createTempDirectory("okay-s2-blob"))
    val prog = for {
      _ <- Blobs.putBytes(blob, "k", Array.fill[Byte](10)(7))
      total <- Blobs.stream(blob, "k").runFold(0)(_ + _.length)
      meta <- Blobs.head(blob, "k")
      _ <- Blobs.delete(blob, "k")
      _ <- Blobs.delete(blob, "k")
      gone <- Blobs.head(blob, "k")
    } yield (total, meta.map(_.size), gone)
    assertEquals(Eff.runAsync(prog), (10, Some(10L), None))
  }

  test("a file goes in streamed; a log's segments are backed up to a blob and restored") {
    val blob = Fs(Files.createTempDirectory("okay-s2-blob"))
    val file = Files.createTempFile("okay-s2", ".txt")
    Files.write(file, "from a file".getBytes)
    val dir = Files.createTempDirectory("okay-s2-log")
    val log = FileStore.open(dir)
    Persist.topic(log, "events").append("k".getBytes, "kept".getBytes, Ack.Durable)
    log.close()
    val restoredDir = Files.createTempDirectory("okay-s2-restored")
    val prog = for {
      _ <- Blobs.putFile(blob, "in/file.txt", file)
      back <- Blobs.getBytes(blob, "in/file.txt")
      copied <- Blobs.backup(dir, blob)
      restored <- Blobs.restore(blob, restoredDir)
    } yield (back.map(new String(_)), copied.nonEmpty, restored.size == copied.size)
    assertEquals(Eff.runAsync(prog), (Right("from a file"), true, true))
    val again = FileStore.open(restoredDir)
    try assertEquals(Eff.runAsync(Persist.stream(Persist.topic(again, "events"), 0, 0L).map(r => new String(r.value)).runCollect), Vector("kept"))
    finally again.close()
  }

  test("documents: conditional writes, versions, and a query by an indexed field") {
    val people = Documents.onTopic[Person](Persist.topic(new MemoryStore, "people"), Map("city" -> ((p: Person) => p.city)))
    val prog = for {
      first <- Documents.put(people, "ada", Person("Ada", "London"), Cond.IfAbsent)
      again <- Documents.put(people, "ada", Person("Ada", "Paris"), Cond.IfAbsent)
      _ <- Documents.put(people, "alan", Person("Alan", "London"))
      found <- Documents.get(people, "ada")
      londoners <- Documents.query(people, "city", "London").map(_._1).runCollect
    } yield (first, again, found.map(_.value), londoners.sorted)
    val (first, again, found, londoners) = Eff.runAsync(prog)
    assert(first.isInstanceOf[PutResult.Applied], first)
    assert(again.isInstanceOf[PutResult.Stale], again)
    assertEquals((found, londoners), (Some(Person("Ada", "London")), Vector("ada", "alan")))
  }
}
