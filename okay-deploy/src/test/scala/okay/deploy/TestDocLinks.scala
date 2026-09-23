package okay.deploy

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * EVERY RELATIVE LINK IN THE PROSE RESOLVES (readme-relative-links,
 * 2026-09-23). Nothing checked them: four module READMEs had been
 * pointing at files that do not exist — their first paragraph was
 * copied from the module's docs/modules page, where `okay-agent.md`
 * and `../building-a-chat-app.md` were right, and one directory up
 * they are not. `TestDocsIndex` covers the module index only; this
 * covers every `](target)` a reader can click.
 *
 * WHAT COUNTS AS A LINK is the whole of the difficulty, because this
 * repository's prose is full of code that looks like one: `f[A](x)`,
 * `Lens.field[P]("age")`, `Inject[Row, A](op)`. So:
 *   - fenced blocks are skipped, and a fence line with an info string
 *     (```` ```scala ````) OPENS a block and never closes one;
 *   - an indented line (four spaces or a tab) is code;
 *   - inline code is skipped, per paragraph, so a span that wraps a
 *     line is still skipped;
 *   - a target counts only when it is PATH-SHAPED: it has a `/`, or
 *     ends in a file extension. `(submit)` and `(op)` are arguments.
 * Measured when it was written: 868 links over docs, specs and every
 * README, no false positive on the fixed tree, and on the tree before
 * it exactly the four real ones.
 */
class TestDocLinks extends munit.FunSuite:

  private val root: Path = Deploy.repoRoot()

  private def rel(p: Path): String = root.relativize(p).toString

  private def mdUnder(dir: String): Vector[Path] =
    val d = root.resolve(dir)
    if !Files.isDirectory(d) then Vector.empty
    else Files.walk(d).iterator.asScala.filter(_.toString.endsWith(".md")).toVector

  /** a README one level down: every module's, and every scala2/ module's */
  private def readmes(dir: Path): Vector[Path] =
    Files.list(dir).iterator.asScala.filter(Files.isDirectory(_))
      .map(_.resolve("README.md")).filter(Files.isRegularFile(_)).toVector

  private lazy val files: Vector[Path] =
    (mdUnder("docs") ++ Files.list(root.resolve("specs")).iterator.asScala.filter(_.toString.endsWith(".md"))
      ++ readmes(root) ++ readmes(root.resolve("scala2"))
      ++ Vector("README.md", "ROADMAP.md").map(root.resolve).filter(Files.isRegularFile(_))).distinct

  private val fenceOpen = """^\s*(`{3,}|~{3,})""".r
  private val extension = """\.(md|scala|sbt|sh|png|svg|html|txt|json|tsv|java|py|R|yml|yaml|conf|csv)$""".r
  private val link = """\]\(([^)\s]+)\)""".r

  /** the text a reader sees as prose: no fenced block, no indented
   * code, no inline code */
  private def prose(text: String): String =
    var fence: Option[String] = None
    val lines = text.split("\n", -1).toVector.map: line =>
      fence match
        case None =>
          fenceOpen.findFirstMatchIn(line) match
            case Some(m) => fence = Some(m.group(1)); ""
            case None => if line.startsWith("    ") || line.startsWith("\t") then "" else line
        case Some(f) =>
          val t = line.trim
          // closing: the fence character alone, at least as long; an
          // info string makes it an opener, which inside a block is text
          if t.nonEmpty && t.forall(_ == f.head) && t.length >= f.length then fence = None
          ""
    lines.mkString("\n").split("\n\n").map(_.replaceAll("`[^`]*`", "")).mkString("\n\n")

  private def broken(file: Path): Vector[String] =
    link.findAllMatchIn(prose(Files.readString(file))).map(_.group(1)).toVector
      .filterNot(t => t.startsWith("http:") || t.startsWith("https:") || t.startsWith("mailto:") || t.startsWith("#"))
      .map(_.takeWhile(_ != '#'))
      .filter(t => t.nonEmpty && (t.contains('/') || extension.findFirstIn(t).isDefined))
      .filterNot(t => Files.exists(file.getParent.resolve(t).normalize))
      .map(t => s"${rel(file)} -> $t")

  test("the check reads what it should: hundreds of files, a README among them") {
    assert(files.size > 300, s"only ${files.size} files — the walk lost a directory")
    assert(files.exists(p => rel(p) == "scala2/okay-scala2/README.md"), "scala2/ READMEs are not read")
  }

  test("every relative link in docs, specs and the READMEs resolves") {
    assertEquals(files.flatMap(broken), Vector.empty[String],
      "a relative link points at nothing (a README's links are relative to the module's directory)")
  }

  test("the rules skip code that only looks like a link") {
    val code = "`f[A](x.md)` and\n\n```scala\nLens.field[P](\"a/b.md\")\n```\n    g[B](c/d.md)\n"
    assertEquals(link.findAllMatchIn(prose(code)).size, 0)
    assertEquals(link.findAllMatchIn(prose("see [the guide](../docs/x.md)")).map(_.group(1)).toList, List("../docs/x.md"))
  }
