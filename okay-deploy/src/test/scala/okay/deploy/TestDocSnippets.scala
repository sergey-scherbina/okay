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
    "docs/rust.md" -> Vector("okay-rust", "okay-py/src/test", "okay-rust/.jvm/src/test"),
    "docs/go.md" -> Vector("okay-py/src/test", "okay-rust/kernels"),
    "docs/typescript.md" -> Vector("okay-py/src/test", "okay-ts/src/test", "okay-codec/src/test", "okay-http/src/test",
      "okay-js/src/test", "scripts", "okay-live/src/test"),
  )

  // a .ts module a test runs (okay-py/src/test/resources) is a tested source
  // too, and so is a live check script's program (scripts/ts-npm-check.sh)
  private val sourceSuffixes = Vector(".scala", ".clj", ".fr", ".ts", ".sh", ".rs", ".go")

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

  // ---- THE RATCHET over every other document (doc-snippets-pin-all)
  //
  // The pinned list above is opt-in, and on 2026-09-23 it held six pages
  // out of 123 with Scala examples: the other 117 carried 3 656 example
  // lines, 1 948 of them in no test at all. Opting them in one by one
  // left the rest unguarded meanwhile — an example could drift the day
  // it was written and nothing would say so. So every document is
  // checked, against every test and benchmark source in the repository,
  // and what was ALREADY unpinned is recorded, line by line, in
  // `docs/snippet-debt.txt` (`<doc>\t<trimmed line>`). Two directions:
  //  - a line that is unpinned and NOT in the debt is new drift: red.
  //    Editing a debt line makes it a new line, so touching an old
  //    example means pinning it — which is the rule.
  //  - a debt entry that is pinned now (or gone) is red too, so the file
  //    only shrinks: delete the entry, the check has earned it.
  // `OKAY_SNIPPET_DEBT=write` rewrites the file after pinning, and it
  // can only SHRINK it: the new file is the old debt that is still
  // unpinned, never a line the old one did not hold — so a regenerate
  // cannot launder the lane's own drift into the debt. (The first file
  // was written once by a full dump, 2026-09-23; that door is closed.)

  private val debtFile: Path = root.resolve("docs/snippet-debt.txt")

  /** every test, benchmark and library tree, down to three levels
   * (`scala2/okay-scala2/probe/src/test` is the deepest there is).
   * `src/main` counts because a theory page QUOTES the library
   * (`// Free.scala:82`), and a quotation must match what it quotes —
   * ch. 4 still showed `case Pure(a: A)` on an invariant `Free` a
   * week after both had changed. */
  private lazy val allSourceLines: Set[String] =
    val one = dirs(root)
    val two = one.flatMap(dirs)
    val tops = Vector(root) ++ one ++ two ++ two.flatMap(dirs)
    sourceLines(tops.flatMap(d => Vector("src/test", "src/jmh", "src/main").map(d.resolve))
      .filter(Files.isDirectory(_)).map(root.relativize(_).toString) :+ "scripts")

  private def dirs(p: Path): Vector[Path] =
    Files.list(p).iterator.asScala
      .filter(d => Files.isDirectory(d) && !d.getFileName.toString.startsWith(".")
        && d.getFileName.toString != "target" && d.getFileName.toString != "node_modules")
      .toVector

  /** the documents the ratchet covers: every Markdown page under docs/
   * with a Scala example, except those the strict list pins already */
  private lazy val ratchetDocs: Vector[String] =
    val strict = pinned.map(_._1).toSet
    Files.walk(root.resolve("docs")).iterator.asScala
      .filter(p => p.toString.endsWith(".md"))
      .map(p => root.relativize(p).toString)
      .filterNot(strict)
      .toVector.sorted

  /** (line number, text) of every line in a ```scala block not marked
   * `not-a-test` — the other fences (sbt, shell, ts) are the strict
   * list's business, where a page names the roots they come from */
  private def scalaExampleLines(doc: String): Vector[(Int, String)] =
    val lines = Files.readAllLines(root.resolve(doc)).asScala.toVector
    val out = Vector.newBuilder[(Int, String)]
    var inBlock = false
    var counted = false
    var i = 0
    while i < lines.length do
      val t = lines(i).trim
      if t.startsWith("```") then
        if inBlock then inBlock = false
        else
          inBlock = true
          counted = t.drop(3).trim == "scala" && !(i > 0 && lines(i - 1).trim.startsWith("<!-- not-a-test:"))
      else if inBlock && counted && t.nonEmpty && !proseInCode(t) then out += ((i + 1, t))
      i += 1
    out.result()

  /** a line that is ONLY a comment, or an elision, is prose in a code
   * block — there is nothing a test could run. A code line with a
   * trailing comment is still checked whole: the comment is usually
   * the answer the page claims, which is exactly what drifts. */
  private def proseInCode(t: String): Boolean =
    t.startsWith("//") || t == "..." || t == "…"

  private def unpinned: Vector[(String, Int, String)] =
    ratchetDocs.flatMap(doc =>
      scalaExampleLines(doc).collect { case (n, t) if !allSourceLines(t) => (doc, n, t) })

  private def readDebt: Set[(String, String)] =
    if !Files.exists(debtFile) then Set.empty
    else Files.readAllLines(debtFile).asScala.iterator
      .filter(l => l.nonEmpty && !l.startsWith("#"))
      .map { l => val i = l.indexOf('\t'); (l.take(i), l.drop(i + 1)) }
      .toSet

  test("RATCHET: no example line outside the recorded debt is unpinned") {
    val now = unpinned
    if sys.env.get("OKAY_SNIPPET_DEBT").contains("write") then
      val old = readDebt
      val header = Vector(
        "# doc-snippets-pin-all: example lines in docs/ that no test or benchmark",
        "# source contains, recorded when the check began (TestDocSnippets). This",
        "# file only SHRINKS: pin a line (copy the tested line into the page, or the",
        "# page's line into a test) and delete its entry here.")
      val body = now.collect { case (d, _, t) if old((d, t)) => s"$d\t$t" }.distinct.sorted
      Files.write(debtFile, (header ++ body).asJava): Unit
    val debt = readDebt
    val fresh = now.collect { case (d, n, t) if !debt((d, t)) => s"$d:$n: $t" }
    assertEquals(fresh, Vector.empty[String],
      "example lines that no test contains, and that are not recorded debt — pin them (copy the tested line), or mark a block `<!-- not-a-test: … -->`")
  }

  test("RATCHET: the debt only shrinks — an entry that is pinned now must go") {
    val still = unpinned.map((d, _, t) => (d, t)).toSet
    val paid = readDebt.filterNot(still).toVector.sorted.map((d, t) => s"$d\t$t")
    assertEquals(paid, Vector.empty[String],
      "these debt entries are pinned (or their line is gone) — delete them from docs/snippet-debt.txt")
  }

  test("RATCHET: the check can fail — an invented line is unpinned, a real one is not") {
    assert(!allSourceLines("val invented = Stream.never().gather(nothing)"))
    assert(allSourceLines("class TestDocSnippets extends munit.FunSuite:"), "the instrument must see its own source")
  }

  test("the check can fail: a line no source contains is reported") {
    // the instrument's own control (instrument-needs-its-own-control)
    val known = sourceLines(Vector("okay-java/src/test"))
    assert(!known("val invented = Stream.never().gather(nothing)"))
    assert(known(exampleLines("docs/modules/okay-java.md").head._2), "a real example must be found")
  }
