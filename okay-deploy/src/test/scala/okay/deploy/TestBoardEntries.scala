package okay.deploy

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * The boards are directories now (boards-d, 2026-09-18) — one file per
 * item, so a lane edits its own item instead of the middle of
 * everyone's file. The shape needs something that RUNS to hold it, for
 * the reason `TestChangelogEntries` gives: a convention nothing
 * enforces drifts, and `scripts/board.sh --check` wired into nothing
 * would be forgotten by the second lane.
 */
class TestBoardEntries extends munit.FunSuite:

  private val root: Path = Deploy.repoRoot()
  private val name = "^[a-z0-9][a-z0-9.-]*\\.md$".r

  private def rootBoards: List[Path] =
    List("sprint.d", "backlog.d").map(root.resolve).filter(Files.isDirectory(_))

  /** a module's OWN backlog, `<dir>/backlog.d` (okay2-backlog, 2026-09-24):
   * the same layout, checked with the root boards */
  private def moduleBacklogs: List[Path] =
    Files.list(root).iterator.asScala
      .map(_.resolve("backlog.d")).filter(Files.isDirectory(_)).toList.sortBy(_.toString)

  private def boards: List[Path] = rootBoards ++ moduleBacklogs

  private def sections(board: Path): List[Path] =
    Files.list(board).iterator.asScala.filter(Files.isDirectory(_)).toList
      .sortBy(_.getFileName.toString)

  private def items(section: Path): List[Path] =
    Files.list(section).iterator.asScala
      .filter(p => p.getFileName.toString.endsWith(".md"))
      .filter(p => p.getFileName.toString != "_section.md")
      .toList.sortBy(_.getFileName.toString)

  test("both boards exist as directories, with a preamble and an order"):
    assertEquals(rootBoards.map(_.getFileName.toString).sorted, List("backlog.d", "sprint.d"))
    assert(moduleBacklogs.map(root.relativize(_).toString).contains("okay2/backlog.d"), moduleBacklogs.toString)
    boards.foreach: b =>
      assert(Files.isRegularFile(b.resolve("_preamble.md")), s"$b has no _preamble.md")
      assert(Files.isRegularFile(b.resolve("_order")), s"$b has no _order")

  test("_order and the directories on disk name the same sections"):
    boards.foreach: b =>
      val listed = Files.readAllLines(b.resolve("_order")).asScala.filter(_.nonEmpty).toSet
      val present = sections(b).map(_.getFileName.toString).toSet
      assertEquals(listed, present, s"$b: _order and the directories disagree")

  test("every section carries its '## ' heading"):
    boards.flatMap(sections).foreach: s =>
      val f = s.resolve("_section.md")
      assert(Files.isRegularFile(f), s"$s has no _section.md")
      val first = Files.readAllLines(f).asScala.headOption.getOrElse("")
      assert(first.startsWith("## "), s"$s/_section.md must start with '## ', got: $first")

  test("every item is named after its lane and begins with a bullet"):
    boards.flatMap(sections).flatMap(items).foreach: p =>
      val n = p.getFileName.toString
      assert(name.matches(n), s"$n — name an item after its lane: <slug>.md, kebab-case")
      val first = Files.readAllLines(p).asScala.headOption.getOrElse("")
      assert(first.startsWith("- "), s"$n: an item starts with '- ', got: $first")

  test("the backlog holds OPEN work — a ticked entry has moved to the archive"):
    // the failure this catches is the one backlog-audit-0918 found
    // 82 times over: an entry closed in place and left on the board
    // that says "open work only", so the counts a reader takes from
    // it are wrong and a section can read as all-open with nothing
    // in it to do. Sprint entries are arcs and are not checked here.
    val closed = (root.resolve("backlog.d") :: moduleBacklogs).flatMap(sections).flatMap(items).filter: p =>
      Files.readAllLines(p).asScala.headOption.exists(_.startsWith("- [x]"))
    assertEquals(closed.map(_.getFileName.toString), Nil,
      "closed entries on the open board — move them to BACKLOG-ARCHIVE.md, verbatim")

  test("no slug is in two places at once — an item is in ONE board"):
    // the failure this catches is a promotion done by copy instead of
    // `git mv`, which leaves the same work on both boards and lets two
    // agents pick it
    val all = boards.flatMap(sections).flatMap(items).map(_.getFileName.toString)
    val dupes = all.groupBy(identity).collect { case (n, xs) if xs.sizeIs > 1 => n }
    assertEquals(dupes.toList.sorted, Nil, "the same item is filed twice")

  test("the pointers still point — nobody rewrote a board as a file again"):
    List("BACKLOG.md", "SPRINT.md").foreach: f =>
      val head = Files.readAllLines(root.resolve(f)).asScala.take(8).mkString("\n")
      assert(head.contains(".d/"),
        s"$f no longer points at its directory — an item was written into the pointer " +
          "instead of its own file (see AGENTS.md)")
