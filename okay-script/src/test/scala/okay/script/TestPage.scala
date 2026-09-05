package okay.script

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

/** okay-script-page: a render-mode .md file compiled ONCE, cached by
 * mtime, re-invoked (not re-compiled) on every render() while the
 * file is unchanged. See specs/okay-script.md "Hot-reload".
 */
class TestPage extends munit.FunSuite:

  private def tempMd(content: String): Path =
    val f = Files.createTempFile("okay-script-page-", ".md")
    Files.writeString(f, content)
    f

  test("render() twice with no file change compiles once -- the second call is far cheaper than a fresh compile") {
    val f = tempMd("```scala\nprintln(\"v1\")\n```\n")
    val page = Page(f)
    try
      val t0 = System.nanoTime()
      val r1 = page.render()
      val firstMs = (System.nanoTime() - t0) / 1000000
      assert(r1.ok, r1.errors.mkString("\n"))
      assert(r1.stdout.contains("v1"), r1.stdout)

      val t1 = System.nanoTime()
      val r2 = page.render()
      val secondMs = (System.nanoTime() - t1) / 1000000
      assert(r2.ok, r2.errors.mkString("\n"))
      assert(r2.stdout.contains("v1"), r2.stdout)
      // a bare reflective invoke is at least an order of magnitude
      // faster than a dotc compile -- a loose bound, not a benchmark
      assert(secondMs < firstMs, s"first=${firstMs}ms second=${secondMs}ms -- expected the cached call to be clearly faster")
    finally
      page.close()
      Files.deleteIfExists(f): Unit
  }

  test("editing the file and bumping its mtime picks up the new content on the next render()") {
    val f = tempMd("```scala\nprintln(\"v1\")\n```\n")
    val page = Page(f)
    try
      assert(page.render().stdout.contains("v1"))
      val t0 = Files.getLastModifiedTime(f)
      Files.writeString(f, "```scala\nprintln(\"v2\")\n```\n")
      Files.setLastModifiedTime(f, FileTime.fromMillis(t0.toMillis + 1000))
      val r2 = page.render()
      assert(r2.ok, r2.errors.mkString("\n"))
      assert(r2.stdout.contains("v2"), r2.stdout)
      assert(!r2.stdout.contains("v1"), r2.stdout)
    finally
      page.close()
      Files.deleteIfExists(f): Unit
  }

  test("editing the file's CONTENT but leaving its mtime UNCHANGED still returns the OLD compiled output -- the cache keys strictly on mtime") {
    val f = tempMd("```scala\nprintln(\"v1\")\n```\n")
    val page = Page(f)
    try
      assert(page.render().stdout.contains("v1"))
      val t0 = Files.getLastModifiedTime(f)
      Files.writeString(f, "```scala\nprintln(\"v2\")\n```\n")
      Files.setLastModifiedTime(f, t0) // same mtime as before the edit
      val r2 = page.render()
      assert(r2.stdout.contains("v1"), r2.stdout) // stale, on purpose -- documented limitation
    finally
      page.close()
      Files.deleteIfExists(f): Unit
  }

  test("Page reflects the same Meta.current machinery render() has -- front-matter, not a parallel implementation") {
    val f = tempMd(
      """---
        |name: okay
        |---
        |
        |```scala
        |import okay.script.Meta
        |print("hi " + Meta.current("name"))
        |```
        |""".stripMargin
    )
    val page = Page(f)
    try
      val r = page.render()
      assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
      assertEquals(r.stdout, "hi okay")
    finally
      page.close()
      Files.deleteIfExists(f): Unit
  }

  test("a compile error on the file's current content is reported through Page.render()'s Result.errors") {
    val f = tempMd("```scala\nval x: Int = \"not an int\"\n```\n")
    val page = Page(f)
    try
      val r = page.render()
      assert(!r.ok)
      assert(r.errors.nonEmpty, r.errors.mkString("\n"))
      assertEquals(r.thrown, None)
    finally
      page.close()
      Files.deleteIfExists(f): Unit
  }

  /**
   * Was watching the SHARED system temp directory
   * (script-temp-tests-watch-a-shared-directory, 2026-09-04): another
   * process's `okay-script-*` entry appearing between the two
   * snapshots -- a sibling worktree's own okay-script tests running
   * concurrently, say -- failed this assertion for a reason that had
   * nothing to do with this `Page`'s own cleanup. Fixed by giving the
   * `Page` its own private temp ROOT and snapshotting THAT, which also
   * drops the old need to exclude the test's own `.md` source file by
   * name -- nothing else writes into a private root.
   */
  test("Page.close() deletes the cached compiled program's temp output directory") {
    import scala.jdk.CollectionConverters.*
    val tmp = Files.createTempDirectory("okay-script-test-root-")
    def snapshot(): Set[String] =
      Files.list(tmp).iterator().asScala
        .map(_.toString)
        .toSet
    val before = snapshot()
    val f = tempMd("```scala\nprintln(1)\n```\n")
    val page = Page(f, tempRoot = tmp)
    try
      page.render(): Unit
      assert(snapshot() != before, "expected a temp dir to exist while the Page holds a compiled program")
    finally
      page.close()
      Files.deleteIfExists(f): Unit
    assertEquals(snapshot(), before)
    Files.deleteIfExists(tmp): Unit
  }

