package okay.deploy

import java.nio.file.Files
import scala.sys.process.*

/**
 * The build's version must not claim a release it is not
 * (version-snapshot, 2026-09-23).
 *
 * `v0.1.1` was tagged on 2026-09-14 and master went on saying
 * `ThisBuild / version := "0.1.1"` for 1387 commits, so every
 * `publishLocal` from master produced artifacts named like the release
 * that were not it. Nothing checked it, because the step that was
 * missing ("after tagging vX, move to the next -SNAPSHOT") is a step
 * somebody has to remember. This makes it a check:
 *
 *   - a version WITHOUT `-SNAPSHOT` is a release: the tag `v<version>`
 *     must exist and point at this very commit;
 *   - a `-SNAPSHOT` version must not be one already released: no tag
 *     `v<version without -SNAPSHOT>` may exist.
 *
 * Where git cannot answer (no repository, no tags fetched), the test
 * says so and skips rather than guessing.
 */
class TestVersionIsNotAReleasedTag extends munit.FunSuite:

  private val root = Deploy.repoRoot()

  private val version: String =
    val line = """^ThisBuild / version := "([^"]+)"""".r
    Files.readAllLines(root.resolve("build.sbt")).toArray.map(_.toString).collectFirst {
      case line(v) => v
    }.getOrElse(fail("build.sbt has no `ThisBuild / version := \"...\"` line"))

  private def git(args: String*): Option[String] =
    val out = new StringBuilder
    val code = Process("git" +: args, root.toFile).!(ProcessLogger(l => { val _ = out.append(l).append('\n') }, _ => ()))
    if code == 0 then Some(out.toString.trim) else None

  private lazy val tags: Set[String] =
    val t = git("tag", "--list", "v*").getOrElse("")
    assume(t.nonEmpty, "no v* tags visible here (a fresh clone without tags?); nothing to compare against")
    t.linesIterator.toSet

  test("a release version is the tagged commit; a snapshot is not an already released version"):
    if version.endsWith("-SNAPSHOT") then
      val base = version.stripSuffix("-SNAPSHOT")
      assert(!tags.contains(s"v$base"),
        s"version $version is a snapshot of v$base, which is already released; the next one is a higher version")
    else
      val tag = s"v$version"
      assert(tags.contains(tag),
        s"version $version claims a release, but there is no tag $tag; a work in progress is a -SNAPSHOT")
      val head = git("rev-parse", "HEAD").getOrElse(fail("git rev-parse HEAD failed"))
      val tagged = git("rev-list", "-n", "1", tag).getOrElse(fail(s"git rev-list $tag failed"))
      assertEquals(head, tagged,
        s"version $version is the release $tag, but HEAD is not that commit: after tagging, move to the next -SNAPSHOT")
