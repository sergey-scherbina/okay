package okay.script

import okay.*
import okay.given
import okay.http.{Http, Request, Response as HttpResponse}
import okay.persist.{FileStore, MemoryStore}

import java.nio.file.{Files, Path}

/** okay-script-persistent-sessions: `Sessions.persisted` over
 * okay-persist -- a restart keeps the carts. See specs/okay-script.md
 * "Persistent sessions".
 */
class TestSessionsPersisted extends munit.FunSuite:

  private def deleteAll(p: Path): Unit =
    Files.walk(p).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(q => Files.deleteIfExists(q): Unit)

  test("over MemoryStore: set, reopen, the attributes are back; invalidate, reopen, gone; expired is not resurrected") {
    val store = MemoryStore()
    val a = Sessions.persisted(store, ttl = java.time.Duration.ofMillis(1000))
    val h = a.handle(None, now = 10_000)
    h.set("cart", "ok-1,ok-2")
    h.set("name", "Ann")
    val id = h.id
    assert(id.nonEmpty && h.created)

    val b = Sessions.persisted(store, ttl = java.time.Duration.ofMillis(1000))
    assertEquals(b.size, 1)
    val h2 = b.handle(Some(id), now = 10_500)
    assertEquals(h2.get("cart"), Some("ok-1,ok-2"))
    assertEquals(h2.attributes, Map("cart" -> "ok-1,ok-2", "name" -> "Ann"))
    assert(!h2.created)
    h2.remove("name")

    val c = Sessions.persisted(store, ttl = java.time.Duration.ofMillis(1000))
    val h3 = c.handle(Some(id), now = 10_900)
    assertEquals(h3.attributes, Map("cart" -> "ok-1,ok-2"))
    h3.invalidate()
    assert(h3.invalidated)

    val d = Sessions.persisted(store, ttl = java.time.Duration.ofMillis(1000))
    assertEquals(d.size, 0)
    assertEquals(d.handle(Some(id), now = 11_000).get("cart"), None)

    // idle past ttl while "down": the record is on the log, the reopen drops it
    val e = Sessions.persisted(store, ttl = java.time.Duration.ofMillis(1000))
    val h4 = e.handle(None, now = System.currentTimeMillis() - 5000)
    h4.set("k", "v")
    val f = Sessions.persisted(store, ttl = java.time.Duration.ofMillis(1000))
    assertEquals(f.size, 1) // rebuilt as-is ...
    f.handle(None, now = System.currentTimeMillis()): Unit
    assertEquals(f.size, 0) // ... and dropped by the first sweep
  }

  test("a touch is a write: a session quietly in use is still live after a reopen") {
    val store = MemoryStore()
    val a = Sessions.persisted(store, ttl = java.time.Duration.ofMillis(1000))
    val h = a.handle(None, now = 1_000)
    h.set("k", "v")
    val id = h.id
    a.handle(Some(id), now = 1_800): Unit // read only, at 1.8s
    val b = Sessions.persisted(store, ttl = java.time.Duration.ofMillis(1000))
    assertEquals(b.handle(Some(id), now = 2_500).get("k"), Some("v"))
  }

  test("over FileStore on disk, through two Sites on the same directory: a cart set via one is read via the other") {
    val dir = Files.createTempDirectory("okay-script-sessions-")
    val pages = Files.createTempDirectory("okay-script-sessions-pages-")
    val api = "```scala\nimport okay.script.api.*\n```\n"
    Files.writeString(pages.resolve("set.md"), api + "```scala\nSession.current.set(\"cart\", \"ok-3\")\n```\nset\n"): Unit
    Files.writeString(pages.resolve("get.md"), api + "cart=${Session.current.get(\"cart\").getOrElse(\"none\")}\n"): Unit
    def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
    try
      val store1 = FileStore.open(dir)
      val site1 = Site(pages, sessions = Sessions.persisted(store1))
      val cookie =
        try
          val r = site1.handle(Request.get("/set"))
          r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("set-cookie") && v.startsWith(Site.SessionCookie + "=") => v.takeWhile(_ != ';') }
            .getOrElse(fail("no cookie"))
        finally
          site1.close()
          store1.close()

      val store2 = FileStore.open(dir)
      val site2 = Site(pages, sessions = Sessions.persisted(store2))
      try
        assertEquals(site2.sessions.size, 1)
        val t = text(site2.handle(Request.get("/get", Seq("Cookie" -> cookie))))
        assert(t.contains("cart=ok-3"), t)
      finally
        site2.close()
        store2.close()
    finally
      deleteAll(dir)
      deleteAll(pages)
  }
