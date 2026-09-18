package okay.script

import okay.codec.Schema
import okay.script.api.Content

import java.nio.file.{Files, Path}

/** specs/site-framework.md stage 2: the words a site shows, as data a
 * person edits — the file, the baked default under it, and a write
 * that a visitor reading at the same moment cannot catch half-done.
 */
class TestContent extends munit.FunSuite:

  final case class Service(key: String, name: String, priceCents: Long) derives Schema
  final case class Shop(title: String, services: Vector[Service]) derives Schema

  private val shipped = Shop("Szykownia", Vector(Service("hem", "Skrócenie spodni", 3500)))

  private def withRoot[A](f: Path => A): A =
    val root = Files.createTempDirectory("okay-script-content-")
    Content.setRoot(Some(root))
    Content.clearProblems()
    try f(root)
    finally
      Content.setRoot(None)
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  test("with no file the baked default answers, and the default is not even built when a file does") {
    withRoot { _ =>
      assertEquals(Content.read[Shop]("content/shop.json", shipped), shipped)
      var built = 0
      def expensive: Shop = { built += 1; shipped }
      assertEquals(Content.read[Shop]("content/shop.json", expensive).title, "Szykownia")
      assertEquals(built, 1)
      assert(Content.write("content/shop.json", shipped.copy(title = "Edited")))
      assertEquals(Content.read[Shop]("content/shop.json", expensive).title, "Edited")
      assertEquals(built, 1, "the default was built although the file answered")
    }
  }

  test("a write round-trips through the Schema, and clear puts the shipped default back") {
    withRoot { root =>
      val edited = Shop("Ательє", Vector(
        Service("hem", "Вкорочення штанів", 4000),
        Service("zip", "Заміна блискавки", 6000)))
      assert(Content.write("content/shop.json", edited))
      assert(Files.isRegularFile(root.resolve("content/shop.json")), "the directories were not created")
      assertEquals(Content.read[Shop]("content/shop.json", shipped), edited)
      assert(Content.clear("content/shop.json"))
      assertEquals(Content.read[Shop]("content/shop.json", shipped), shipped)
    }
  }

  test("a DAMAGED file is the default, not a 500 -- and the page can say so") {
    withRoot { root =>
      Files.createDirectories(root.resolve("content"))
      Files.writeString(root.resolve("content/shop.json"), """{"title": "half a""") : Unit
      assertEquals(Content.read[Shop]("content/shop.json", shipped), shipped)
      assert(Content.problem.exists(_.contains("content/shop.json")), Content.problem.toString)
      // an EMPTY file is the same story and says nothing alarming
      Content.clearProblems()
      Files.writeString(root.resolve("content/shop.json"), ""): Unit
      assertEquals(Content.read[Shop]("content/shop.json", shipped), shipped)
    }
  }

  test("content cannot leave the site root, reading or writing") {
    withRoot { root =>
      val outside = root.getParent.resolve("escaped.json")
      assert(!Content.write("../escaped.json", shipped), "a write escaped the root")
      assert(!Files.exists(outside), "a file was written outside the site")
      assertEquals(Content.read[Shop]("../escaped.json", shipped), shipped)
      assert(!Content.exists("../../etc/passwd"))
    }
  }

  test("outside a Site there is no root, and every read is its default") {
    Content.setRoot(None)
    assertEquals(Content.read[Shop]("content/shop.json", shipped), shipped)
    assert(!Content.write("content/shop.json", shipped))
    assert(!Content.exists("content/shop.json"))
  }
