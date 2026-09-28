package okay.dlm

import munit.FunSuite
import java.nio.file.{Files, Path}

/** the shelf and its policy — one test per behavior line of
 * specs/dlm-learning.md §9 that the library holds */
class TestShelf extends FunSuite:
  import Ledger.Entry

  val embed = okay.rag.Vectors.hashing(64)
  def table(phrases: String*): Exemplars =
    Exemplars.compile(phrases.map(p => "need" -> p), embed, "hashing-64")
  val one = table("нужен сантехник", "ищу электрика")
  val two = table("нужен сантехник", "ищу электрика", "нужна швея")
  val three = table("нужен плотник")

  val tmp = FunFixture[Path](_ => Files.createTempDirectory("shelf"), d =>
    Files.walk(d).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p): Unit))

  tmp.test("a table put on the shelf and read back hashes the same, F16 included; putting it twice keeps one") { dir =>
    for shelf <- Vector(Shelf.directory(dir), Shelf.memory()) do
      val k = shelf.put("intents", one, 10L)
      // the name is the hash of the table AS SERVED — rounded to F16 —
      // and not of the full-precision numbers the build held
      assertEquals(k.hash, one.stored(f16 = true).hash)
      assertNotEquals(k.hash, one.hash)
      assertEquals(shelf.get("intents", k.hash).map(_.hash), Right(k.hash))
      assertEquals(shelf.put("intents", one, 20L), k)
      assertEquals(shelf.kept("intents").map(_.hash), Vector(k.hash))
      assertEquals((k.encoder, k.at), ("hashing-64", 10L))
      // rounding twice rounds nothing
      assertEquals(one.stored().stored().hash, one.stored().hash)
  }

  tmp.test("a shelf refuses a table of another encoder by name, like a checkpoint — and one whose content changed") { dir =>
    val shelf = Shelf.directory(dir)
    val k = shelf.put("intents", one, 1L)
    assert(shelf.get("intents", k.hash, Some(("e5-small", 64))).left.exists(_.contains("refused")))
    assert(shelf.get("intents", "0" * 32).left.exists(_.contains("not on the shelf")))
    assert(shelf.get("../etc", k.hash).isLeft)
    // a file on the shelf is named by what it holds; replaced under
    // the same name, it is refused rather than served
    Files.move(dir.resolve(s"intents.${shelf.put("intents", three, 2L).hash}.safetensors"),
      dir.resolve(s"intents.${k.hash}.safetensors"), java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    assert(shelf.get("intents", k.hash).left.exists(_.contains("hashes to")), shelf.get("intents", k.hash).toString)
  }

  test("`all` drops nothing; `latest` keeps the newest; `within` keeps the young; `any` keeps what either keeps; `of` reads each spelling") {
    val kept = Vector(Kept("a", "1" * 32, "e", 100L, 1), Kept("a", "2" * 32, "e", 200L, 1), Kept("a", "3" * 32, "e", 300L, 1))
    assertEquals(Retention.all.expired(kept, 1000L), Vector.empty)
    assertEquals(summon[Retention].name, "all")
    assertEquals(Retention.latest(2).expired(kept, 1000L).map(_.at), Vector(100L))
    assertEquals(Retention.within(750L).expired(kept, 1000L).map(_.at), Vector(100L, 200L))
    assertEquals(Retention.any(Retention.latest(1), Retention.within(850L)).expired(kept, 1000L).map(_.at), Vector(100L))
    assertEquals(Retention.of("last:2").map(_.name), Right("last:2"))
    assertEquals(Retention.of("days:30").map(_.name), Right("days:30"))
    assertEquals(Retention.of("last:5, days:30").map(_.name), Right("last:5,days:30"))
    assertEquals(Retention.of("all").map(_.expired(kept, 0L)), Right(Vector.empty))
    assert(Retention.of("last:0").left.exists(_.contains("last:N")))
    assert(Retention.of("forever").left.exists(_.contains("«forever»")))
    assert(Retention.of("").isLeft)
  }

  tmp.test("`prune` never drops the newest table of an artifact nor a serving one; each drop names the policy") { dir =>
    val shelf = Shelf.directory(dir)
    val a = shelf.put("intents", one, 100L)
    val b = shelf.put("intents", two, 200L)
    val c = shelf.put("intents", three, 300L)
    val acts = shelf.put("acts", one, 100L)
    // a policy that would drop everything still leaves the newest of
    // each artifact and the table serving
    val everything = new Retention:
      val name = "everything"
      def expired(kept: Vector[Kept], now: Long) = kept
    val dropped = Shelf.prune(shelf, everything, serving = Set("intents" -> a.hash), now = 999L, by = "ops")
    assertEquals(dropped, Vector(Entry.Pruned("intents", b.hash, "everything", 999L, "ops")))
    assertEquals(shelf.kept("intents").map(_.hash).toSet, Set(a.hash, c.hash))
    assertEquals(shelf.kept("acts").map(_.hash), Vector(acts.hash))
    // ours keeps everything
    assertEquals(Shelf.prune(shelf, Retention.ours, Set.empty, 1000L, "ops"), Vector.empty)
    assertEquals(shelf.all.length, 3)
  }

  tmp.test("a ledger written to a file reads back entry for entry, `Pruned` included") { dir =>
    val f = Ledger.File(dir.resolve("ledger.jsonl"))
    val entries = Vector(
      Entry.Rebuilt("intents", "hashing-64", None, "a" * 32, "corpus/intents.json@f00", 1L, "ci"),
      Entry.Rebuilt("intents", "hashing-64", Some("a" * 32), "b" * 32, "shelf:" + "a" * 32, 2L, "ops"),
      Entry.Pruned("intents", "a" * 32, "last:1", 3L, "ops"),
      Entry.Learned("ann", "мои штаны", "listings", 7L, 4L, "ann"),
      Entry.Refused("bob", "teach", "no such class: orders", 5L, "bob"))
    entries.foreach(f.append)
    assertEquals(f.entries, entries)
    // a line this reader does not know is skipped, not a failure
    assertEquals(Ledger.parse(Ledger.line(entries(2)) + "\n{\"entry\":\"future\"}\n"), Vector(entries(2)))
  }
