package okay.deploy

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * The module index in `docs/README.md` is a hand-kept table, and
 * nothing was checking it: eight rows had gone missing before anyone
 * noticed, and three modules (okay-crypto, okay-script,
 * okay-demo-e2e-browser) had shipped with no page at all. A doc that
 * silently stops covering the build is worse than a missing one — a
 * reader trusts an index.
 *
 * It lives here, next to `TestDemoDeploy`, for the same reason that
 * one does: `Deploy.repoRoot()` is where this repository already
 * keeps "read the committed tree and compare it against what it
 * should be", and okay-deploy is the module that owns that idea.
 */
class TestDocsIndex extends munit.FunSuite:

  private val root: Path = Deploy.repoRoot()

  private def read(rel: String): String =
    Files.readString(root.resolve(rel))

  /** every docs/modules/<name>.md */
  private lazy val pages: Set[String] =
    val dir = root.resolve("docs/modules")
    Files.list(dir).iterator.asScala
      .map(_.getFileName.toString)
      .filter(_.endsWith(".md"))
      .map(_.dropRight(3))
      .toSet

  /** every module the index links to */
  private lazy val rows: Set[String] =
    "modules/([a-z0-9-]+)\\.md".r.findAllMatchIn(read("docs/README.md"))
      .map(_.group(1)).toSet

  /** every module root the build declares — `file("okay-x")`, both
   * the `project in file(...)` and the crossProject `.in(file(...))`
   * forms. A path with a slash (okay-demo/web) is a sub-project of a
   * module that has its own page, not a module root */
  private lazy val modules: Set[String] =
    "file\\(\"(okay-[a-z0-9-]+)\"\\)".r.findAllMatchIn(read("build.sbt"))
      .map(_.group(1)).toSet

  test("every module page is linked from the docs/README.md index") {
    assertEquals(pages -- rows, Set.empty[String],
      "docs/modules pages missing from the index table in docs/README.md")
  }

  test("every index row points at a page that exists") {
    assertEquals(rows -- pages, Set.empty[String],
      "docs/README.md links a modules/<name>.md that is not there")
  }

  test("every module the build declares has a page") {
    assertEquals(modules -- pages, Set.empty[String],
      "modules declared in build.sbt with no docs/modules page")
  }
