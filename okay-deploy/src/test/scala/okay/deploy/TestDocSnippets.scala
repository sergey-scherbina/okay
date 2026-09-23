package okay.deploy

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * A documented example must be a VERBATIM copy of a tested one
 * (docs-are-part-of-the-lane: "every snippet verbatim in a gated test").
 * The rule was followed by hand, and hand is how it broke: written
 * 2026-09-23 after a check run over the interop pages found seven lines
 * that no test contained — examples "tidied" for the page (a `val`
 * dropped, a native's column alignment squeezed) after their test was
 * written, on a page that had passed every gate since.
 *
 * For each listed document, every line of every fenced code block must
 * occur (trimmed) as a line of the Scala, Clojure or Frege sources under
 * the given roots. A block a test cannot pin — build configuration — is
 * marked in the document itself, on the line before its fence:
 * `<!-- not-a-test: <why> -->`. The list is opt-in: a page joins it when
 * its examples are all pinned, and the guides older than this check
 * join as they are verified, rather than all at once and red.
 */
class TestDocSnippets extends munit.FunSuite:

  private val root: Path = Deploy.repoRoot()

  /** a document, and the source roots its examples must come from */
  private val pinned: Vector[(String, Vector[String])] = Vector(
    "docs/jvm-languages.md" -> Vector("okay-java/src/test", "okay-clojure/src/test", "okay-frege/src/test"),
    "docs/modules/okay-java.md" -> Vector("okay-java/src/test"),
    "docs/modules/okay-clojure.md" -> Vector("okay-clojure/src/test"),
    "docs/modules/okay-frege.md" -> Vector("okay-frege/src/test"),
    "docs/python-and-r.md" -> Vector("okay-py/src/test", "okay-r/src/test"),
    "docs/typescript.md" -> Vector("okay-py/src/test", "okay-ts/src/test", "okay-codec/src/test", "okay-http/src/test",
      "okay-js/src/test", "scripts"),
  )

  // a .ts module a test runs (okay-py/src/test/resources) is a tested source
  // too, and so is a live check script's program (scripts/ts-npm-check.sh)
  private val sourceSuffixes = Vector(".scala", ".clj", ".fr", ".ts", ".sh")

  private def sourceLines(roots: Vector[String]): Set[String] =
    roots.flatMap { r =>
      val dir = root.resolve(r)
      if !Files.isDirectory(dir) then Vector.empty
      else Files.walk(dir).iterator.asScala
        .filter(p => sourceSuffixes.exists(p.toString.endsWith))
        .flatMap(p => Files.readAllLines(p).asScala.map(_.trim))
        .toVector
    }.toSet

  /** (line number, text) of every line inside a fenced block that is not
   * marked `not-a-test` */
  private def exampleLines(doc: String): Vector[(Int, String)] =
    val lines = Files.readAllLines(root.resolve(doc)).asScala.toVector
    val out = Vector.newBuilder[(Int, String)]
    var inBlock = false
    var exempt = false
    var i = 0
    while i < lines.length do
      val l = lines(i)
      if l.trim.startsWith("```") then
        if inBlock then inBlock = false
        else
          inBlock = true
          exempt = i > 0 && lines(i - 1).trim.startsWith("<!-- not-a-test:")
      else if inBlock && !exempt && l.trim.nonEmpty then out += ((i + 1, l.trim))
      i += 1
    out.result()

  test("every example in a pinned document is a verbatim line of a tested source") {
    val missing = pinned.flatMap { (doc, roots) =>
      val known = sourceLines(roots)
      exampleLines(doc).collect { case (n, text) if !known(text) => s"$doc:$n: $text" }
    }
    assertEquals(missing, Vector.empty[String],
      "documented examples that no test contains — copy the tested line, or mark a build block `<!-- not-a-test: … -->`")
  }

  test("the check can fail: a line no source contains is reported") {
    // the instrument's own control (instrument-needs-its-own-control)
    val known = sourceLines(Vector("okay-java/src/test"))
    assert(!known("val invented = Stream.never().gather(nothing)"))
    assert(known(exampleLines("docs/modules/okay-java.md").head._2), "a real example must be found")
  }
