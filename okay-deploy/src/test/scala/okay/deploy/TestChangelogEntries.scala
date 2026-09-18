package okay.deploy

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * A landing writes `changelog.d/<slug>.md` rather than the head of
 * everyone's `CHANGELOG.md` (changelog-d, 2026-09-18). The shape has
 * to be checked by something that runs, because a convention nothing
 * enforces is a convention that drifts — and `scripts/changelog.sh
 * --check` wired into nothing would be forgotten by the second lane.
 *
 * It lives beside `TestDocsIndex` for the reason that one gives:
 * okay-deploy is the module that owns "read the committed tree and
 * compare it against what it should be".
 */
class TestChangelogEntries extends munit.FunSuite:

  private val root: Path = Deploy.repoRoot()
  private val dir: Path = root.resolve("changelog.d")

  private lazy val entries: List[Path] =
    if !Files.isDirectory(dir) then Nil
    else Files.list(dir).iterator.asScala
      .filter(p => p.getFileName.toString.endsWith(".md"))
      .toList.sortBy(_.getFileName.toString)

  private val name = "^[a-z0-9][a-z0-9.-]*\\.md$".r

  test("every entry is named after its lane, in kebab-case"):
    entries.foreach: p =>
      val n = p.getFileName.toString
      assert(name.matches(n),
        s"$n — name an entry after the lane that landed it: <slug>.md, kebab-case")

  test("every entry begins with a '## ' title — the archive's own shape"):
    entries.foreach: p =>
      val first = Files.readAllLines(p).asScala.headOption.getOrElse("")
      assert(first.startsWith("## "),
        s"${p.getFileName}: the first line must be a '## ' title, got: $first")

  test("no two entries share a title"):
    val titles = entries.map(p => Files.readAllLines(p).asScala.head)
    val dupes = titles.groupBy(identity).collect { case (t, xs) if xs.sizeIs > 1 => t }
    assertEquals(dupes.toList, Nil, "two landings wrote the same headline")

  test("the archive is not edited any more — its head is the switch note"):
    // The one thing a directory cannot stop by itself: somebody
    // prepending to CHANGELOG.md out of habit. The archive's first
    // lines say where new entries go, so this asserts they are still
    // there rather than buried under a fresh entry.
    val head = Files.readAllLines(root.resolve("CHANGELOG.md")).asScala.take(6).mkString("\n")
    assert(head.contains("changelog.d"),
      "CHANGELOG.md's head no longer points at changelog.d — an entry was prepended to the " +
        "archive instead of being written as its own file (see AGENTS.md)")
