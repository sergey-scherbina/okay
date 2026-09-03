package okay.script

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Reporter, Diagnostic}

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.net.URLClassLoader
import java.nio.file.{Files, Path, Paths}

/** A markdown file's ```scala fenced code blocks, taken as a single
 * literate Scala program: extracted, compiled through the REAL Scala 3
 * compiler (dotty.tools.dotc, in-process) and run. No new language, no
 * interpreter -- see specs/okay-script.md.
 */
final case class Block(code: String, startLine: Int)

/** A markdown document's `render` tokens, in document order --
 * see specs/okay-script.md "Interpolation".
 */
enum Segment:
  /** prose (or a non-scala fenced block, fence markers included),
   * verbatim -- rendered byte-identical except for any `${...}`
   * markers, which are split out as their own Interp segments. */
  case Text(s: String)
  /** a ```scala fence's content, verbatim -- runs for its side
   * effects (definitions visible to LATER segments, direct output via
   * println), exactly like a `run` block. */
  case Code(s: String)
  /** a `${expr}` marker's inner expression -- evaluated, `.toString`
   * printed in place. */
  case Interp(expr: String)

final case class Result(
  ok: Boolean,
  stdout: String,
  errors: Vector[String],
  thrown: Option[Throwable],
)

/** The set of classpath entries a script compiles and runs against.
 * `ambient` (the calling JVM's own `-cp`) is a HYPOTHESIS about the
 * environment, not a fact: it reads
 * `System.getProperty("java.class.path")`, which is only correct when
 * the JVM was launched with a real `-cp` -- an un-forked sbt test JVM
 * reports just `sbt-launch.jar` there (found 2026-09-03,
 * okay-script-scalac-classpath: sbt manages its actual classpath
 * through its own classloaders, invisible to that property, and a
 * script compiled against it saw no scala-library at all). A caller
 * building a RUNTIME app from a generated `.md` file -- a storefront,
 * say -- should not depend on inheriting the host process's classpath
 * at all: it supplies exactly what the script needs (its own
 * okay-ui/okay-jetty jars, resolved `using dep` artifacts) instead.
 */
final case class Classpath(entries: Vector[Path]):
  def ++(extra: Vector[Path]): Classpath = Classpath(entries ++ extra)
  def asString: String = entries.map(_.toString).mkString(File.pathSeparator)

object Classpath:
  val ambient: Classpath = Classpath(
    System.getProperty("java.class.path").split(File.pathSeparatorChar).toVector.map(Paths.get(_))
  )

/** `//> using dep "org:artifact:version"` directives -- scala-cli's own
 * convention, reused rather than inventing another one -- hoisted out
 * of a markdown file's ```scala blocks and resolved to jars.
 *
 * Fetching a Maven coordinate is inherently a network operation, so
 * this shells out to the standard `cs`/`coursier` CLI rather than
 * embedding a resolver in-process: dotc's own compilation stays
 * in-process (see ScalaScript.run), only dependency FETCHING crosses
 * the process boundary, and only when a script actually declares one.
 */
object Deps:
  private val usingDep = """//>\s*using\s+dep\s+"([^"]+)"""".r

  def declared(markdown: String): Vector[String] =
    ScalaScript.blocks(markdown).flatMap(b => usingDep.findAllMatchIn(b.code).map(_.group(1))).distinct

  enum Resolved:
    case Jars(paths: Vector[Path])
    case ToolMissing
    case Failed(message: String)

  def resolve(coords: Vector[String]): Resolved =
    if coords.isEmpty then Resolved.Jars(Vector.empty)
    else
      findTool() match
        case None => Resolved.ToolMissing
        case Some(bin) =>
          val proc = new ProcessBuilder((Vector(bin, "fetch") ++ coords)*).start()
          val out = new String(proc.getInputStream.readAllBytes(), "UTF-8")
          val err = new String(proc.getErrorStream.readAllBytes(), "UTF-8")
          val code = proc.waitFor()
          if code == 0 then
            Resolved.Jars(out.linesIterator.filter(_.nonEmpty).map(Paths.get(_)).toVector)
          else
            Resolved.Failed(err.trim)

  private def findTool(): Option[String] =
    val path = Option(System.getenv("PATH")).getOrElse("")
    val names = Vector("cs", "coursier")
    path
      .split(File.pathSeparatorChar)
      .toVector
      .flatMap(dir => names.map(n => Paths.get(dir, n)))
      .find(p => Files.isRegularFile(p) && Files.isExecutable(p))
      .map(_.toString)

object ScalaScript:

  private val fenceOpen = """```scala\s*""".r
  private val fenceClose = """```\s*""".r

  def blocks(markdown: String): Vector[Block] =
    val lines = markdown.linesWithSeparators.toVector.map(_.stripLineEnd)
    val out = Vector.newBuilder[Block]
    var i = 0
    while i < lines.length do
      if fenceOpen.matches(lines(i)) then
        val start = i + 1
        val body = Vector.newBuilder[String]
        var j = start
        while j < lines.length && !fenceClose.matches(lines(j)) do
          body += lines(j)
          j += 1
        out += Block(body.result().mkString("\n"), start + 1)
        i = j + 1
      else
        i += 1
    out.result()

  private val yamlFenceOpen = """```yaml(\s+\S+)?\s*""".r
  private val headingRe = """(#{1,6})\s+(.*?)\s*""".r

  /** The whole document, tokenized in document order, each segment
   * paired with its heading-ancestor path (root..nearest enclosing
   * heading) -- see specs/okay-script.md "Metadata as context". A
   * ```scala fence (found exactly as `blocks` finds them) becomes ONE
   * Code segment; a ```yaml fence is METADATA -- folded into the
   * current section's `yaml` (via `Meta.parseYaml`), producing no
   * Segment at all; a heading line updates the path AND is itself
   * kept as ordinary text (headings render); everything else --
   * prose, other-language fences, fence markers included -- is
   * scanned for `${...}` markers and split into Text/Interp segments.
   * Front-matter (if any) is skipped here -- it plays no role in the
   * PATH, only in `Meta.Context.get`'s fallback, via the separately
   * parsed `Meta.Doc`.
   */
  private def tokenize(markdown: String): Vector[(Segment, Vector[Meta.Section])] =
    val lines = markdown.linesWithSeparators.toVector.map(_.stripLineEnd)
    var i = 0
    if lines.headOption.contains("---") then
      val end = lines.indexWhere(_ == "---", 1)
      if end > 0 then i = end + 1

    final case class Building(level: Int, title: String, var yaml: Vector[Meta.Value])
    val stack = scala.collection.mutable.ArrayBuffer(Building(0, "", Vector.empty))
    def currentPath: Vector[Meta.Section] =
      stack.toVector.map(b => Meta.Section(b.level, b.title, b.yaml, Vector.empty))
    def closeTo(level: Int): Unit =
      while stack.length > 1 && stack.last.level >= level do stack.remove(stack.length - 1): Unit

    val out = Vector.newBuilder[(Segment, Vector[Meta.Section])]
    val textLines = Vector.newBuilder[String]
    def flushText(): Unit =
      val t = textLines.result()
      if t.nonEmpty then
        val path = currentPath
        splitInterpolations(t.mkString("\n")).foreach(seg => out += (seg -> path))
      textLines.clear()

    while i < lines.length do
      val line = lines(i)
      if fenceOpen.matches(line) then
        flushText()
        var j = i + 1
        val body = Vector.newBuilder[String]
        while j < lines.length && !fenceClose.matches(lines(j)) do
          body += lines(j); j += 1
        out += (Segment.Code(body.result().mkString("\n")) -> currentPath)
        i = j + 1
      else if yamlFenceOpen.matches(line) then
        flushText()
        var j = i + 1
        val body = Vector.newBuilder[String]
        while j < lines.length && !fenceClose.matches(lines(j)) do
          body += lines(j); j += 1
        stack.last.yaml = stack.last.yaml :+ Meta.parseYaml(body.result())
        i = j + 1
      else
        line match
          case headingRe(hashes, title) =>
            flushText()
            closeTo(hashes.length)
            stack += Building(hashes.length, title, Vector.empty)
            textLines += line
            i += 1
          case _ =>
            textLines += line
            i += 1
    flushText()
    out.result()

  /** A `Meta.Value` as Scala source -- a constructor call, structurally
   * recursive over the (small) parsed metadata tree.
   */
  private def metaValueLiteral(v: Meta.Value): String = v match
    case Meta.Value.Str(s) => s"okay.script.Meta.Value.Str(${scalaStringLiteral(s)})"
    case Meta.Value.Arr(items) =>
      s"okay.script.Meta.Value.Arr(Vector(${items.map(metaValueLiteral).mkString(", ")}))"
    case Meta.Value.Obj(fields) =>
      val fs = fields.map((k, v) => s"(${scalaStringLiteral(k)}, ${metaValueLiteral(v)})").mkString(", ")
      s"okay.script.Meta.Value.Obj(Vector($fs))"

  private def sectionLiteral(s: Meta.Section): String =
    val yaml = s.yaml.map(metaValueLiteral).mkString(", ")
    val children = s.children.map(sectionLiteral).mkString(", ")
    s"okay.script.Meta.Section(${s.level}, ${scalaStringLiteral(s.title)}, Vector($yaml), Vector($children))"

  private def docLiteral(d: Meta.Doc): String =
    val fm = d.frontMatter.map((k, v) => s"(${scalaStringLiteral(k)}, ${scalaStringLiteral(v)})").mkString(", ")
    s"okay.script.Meta.Doc(Map($fm), ${sectionLiteral(d.root)})"

  private def hasMeta(doc: Meta.Doc): Boolean =
    doc.frontMatter.nonEmpty || doc.root.yaml.nonEmpty || doc.root.children.nonEmpty

  /** Wraps a sequence of (heading-path, statement-source) pairs into
   * ONE body. A document with NO front-matter, yaml, or headings emits
   * no `okay.script.Meta` reference at all -- `run`/`render` stay
   * self-sufficient (scala-library only) for the common metadata-free
   * case, which is every script/example that predates this feature.
   * Otherwise: a `val` holding the whole `Meta.Doc` (literalized
   * ONCE), then -- whenever a path differs from the one before it --
   * `okay.script.Meta.setCurrent(...)` (a plain statement; `given`
   * cannot do this -- see Meta.scala's own comment: a plain `given` is
   * evaluated once, not per summon, confirmed empirically), then the
   * segment/block's own statement.
   */
  private def withMeta(doc: Meta.Doc, items: Vector[(Vector[Meta.Section], String)]): String =
    val sb = new StringBuilder
    if hasMeta(doc) then
      sb ++= "  val _okayScriptDoc_ : okay.script.Meta.Doc = " ++= docLiteral(doc) ++= "\n": Unit
      var prevPath: Option[Vector[Meta.Section]] = None
      for (path, code) <- items do
        if !prevPath.contains(path) then
          val p = path.map(sectionLiteral).mkString(", ")
          sb ++= s"  okay.script.Meta.setCurrent(okay.script.Meta.Context(_okayScriptDoc_, Vector($p)))\n"
          prevPath = Some(path)
        if code.nonEmpty then sb ++= code ++= "\n": Unit
    else
      for (_, code) <- items do
        if code.nonEmpty then sb ++= code ++= "\n": Unit
    sb ++= "  ()\n"
    sb.toString

  /** Splits a text run on `${expr}` markers (`$${` escapes to a
   * literal `${`). Brace-depth-aware, and quote-aware within the
   * expr -- a `{`/`}` inside a `"..."` span in the expression (e.g. a
   * NESTED `s"${x}"` string interpolation) does not affect the depth
   * that closes the marker.
   */
  private def splitInterpolations(text: String): Vector[Segment] =
    val out = Vector.newBuilder[Segment]
    val buf = new StringBuilder
    val n = text.length
    var i = 0
    while i < n do
      if i + 2 < n && text(i) == '$' && text(i + 1) == '$' && text(i + 2) == '{' then
        buf ++= "${"
        i += 3
      else if i + 1 < n && text(i) == '$' && text(i + 1) == '{' then
        if buf.nonEmpty then
          out += Segment.Text(buf.toString)
          buf.clear()
        val start = i + 2
        var depth = 1
        var inString = false
        var escaped = false
        var k = start
        while k < n && depth > 0 do
          val c = text(k)
          if inString then
            if escaped then escaped = false
            else if c == '\\' then escaped = true
            else if c == '"' then inString = false
          else if c == '"' then inString = true
          else if c == '{' then depth += 1
          else if c == '}' then depth -= 1
          k += 1
        val exprEnd = if depth == 0 then k - 1 else k
        out += Segment.Interp(text.substring(start, exprEnd))
        i = k
      else
        buf += text(i)
        i += 1
    if buf.nonEmpty then out += Segment.Text(buf.toString)
    out.result()

  /** A Text segment as Scala source: a raw triple-quoted string when
   * safe (no embedded `"""` run, does not end in `"` -- either would
   * make the closing `"""` ambiguous), else a normal escaped literal.
   */
  private def scalaStringLiteral(s: String): String =
    if s.contains("\"\"\"") || s.endsWith("\"") then
      val esc = s.flatMap {
        case '\\' => "\\\\"
        case '"' => "\\\""
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case c => c.toString
      }
      "\"" + esc + "\""
    else
      "\"\"\"" + s + "\"\"\""

  /** Compile and run a markdown file's ```scala blocks as one program
   * (see Block/Result docs). `classpath` defaults to the calling
   * process's own -- see Classpath's doc for why that is a hypothesis,
   * not a given -- and a script's own `//> using dep` directives are
   * resolved and appended to it before compiling. Every block sees a
   * local `given okay.script.Meta.Context` scoped to its own heading
   * position -- see specs/okay-script.md "Metadata as context".
   */
  def run(markdown: String, classpath: Classpath = Classpath.ambient): Result =
    withDeps(markdown, classpath): cp =>
      val doc = Meta.parse(markdown)
      val items = tokenize(markdown).collect { case (Segment.Code(code), path) =>
        (path, code.linesWithSeparators.map("  " + _).mkString)
      }
      compileAndRun(withMeta(doc, items), cp)

  /** The whole document -- prose AND code -- as one program: `${expr}`
   * markers in prose (outside ```scala fences) are evaluated and their
   * `.toString` substituted in place; everything else passes through
   * verbatim (a ```yaml fence is METADATA and does not appear in the
   * output; any OTHER non-scala language tag still does). The
   * rendered document is `Result.stdout` on success. Every segment
   * sees the same local `given okay.script.Meta.Context` `run` gives
   * its blocks. See specs/okay-script.md "Interpolation" and
   * "Metadata as context".
   */
  def render(markdown: String, classpath: Classpath = Classpath.ambient): Result =
    withDeps(markdown, classpath): cp =>
      val doc = Meta.parse(markdown)
      val items = tokenize(markdown).map {
        case (Segment.Text(s), path) if s.nonEmpty => (path, "  print(" + scalaStringLiteral(s) + ")")
        case (Segment.Text(_), path) => (path, "")
        case (Segment.Interp(expr), path) => (path, s"  print(($expr).toString)")
        case (Segment.Code(code), path) => (path, code.linesWithSeparators.map("  " + _).mkString)
      }
      compileAndRun(withMeta(doc, items), cp)

  private def withDeps(markdown: String, classpath: Classpath)(f: Classpath => Result): Result =
    val coords = Deps.declared(markdown)
    Deps.resolve(coords) match
      case Deps.Resolved.Failed(msg) =>
        Result(ok = false, stdout = "", errors = Vector(s"dependency resolution failed: $msg"), thrown = None)
      case Deps.Resolved.ToolMissing =>
        Result(ok = false, stdout = "", errors = Vector("`using dep` declared but no cs/coursier found on PATH"), thrown = None)
      case Deps.Resolved.Jars(extra) =>
        f(classpath ++ extra)

  private def compileAndRun(body: String, classpath: Classpath): Result =
    val wrapped =
      s"""@main def okayScriptMain(): Unit =
         |$body
         |""".stripMargin

    val dir = Files.createTempDirectory("okay-script-")
    try
      val srcFile = dir.resolve("OkayScriptMain.scala")
      Files.writeString(srcFile, wrapped)
      val outDir = Files.createDirectory(dir.resolve("out"))

      val diagnostics = Vector.newBuilder[String]
      val reporter = collectingReporter(diagnostics)

      val args = Array(
        "-classpath", classpath.asString,
        "-d", outDir.toString,
        "-color:never",
        srcFile.toString,
      )

      val driver = new Driver:
        override protected def sourcesRequired: Boolean = false

      val summary = driver.process(args, reporter, null)
      val errs = diagnostics.result()

      if summary.hasErrors then
        Result(ok = false, stdout = "", errors = errs, thrown = None)
      else
        // platform-only parent (okay-script-classloader-isolation,
        // 2026-09-03): getClass.getClassLoader would let a script
        // resolve anything on okay-script's OWN build classpath
        // (URLClassLoader is parent-first) regardless of what the
        // caller actually put in `classpath` -- defeating the
        // isolation Classpath/Deps exist for. A script sees exactly
        // its own compiled classes, its own Classpath, and the JDK.
        val loaderUrls = (outDir +: classpath.entries).map(_.toUri.toURL).toArray
        val loader = new URLClassLoader(loaderUrls, ClassLoader.getPlatformClassLoader())
        val cls = loader.loadClass("okayScriptMain")
        val method = cls.getMethod("main", classOf[Array[String]])

        val capturedOut = new ByteArrayOutputStream()
        var thrown: Option[Throwable] = None
        try
          val prevOut = System.out
          val ps = new PrintStream(capturedOut, true, "UTF-8")
          System.setOut(ps)
          try
            scala.Console.withOut(ps):
              try method.invoke(null, Array.empty[String])
              catch
                case e: java.lang.reflect.InvocationTargetException =>
                  thrown = Some(Option(e.getCause).getOrElse(e))
                case e: Throwable =>
                  thrown = Some(e)
          finally
            System.setOut(prevOut)
        finally
          loader.close()

        Result(
          ok = thrown.isEmpty,
          stdout = capturedOut.toString("UTF-8"),
          errors = Vector.empty,
          thrown = thrown,
        )
    finally
      deleteRecursively(dir)

  private def collectingReporter(sink: scala.collection.mutable.Builder[String, Vector[String]]): Reporter =
    new dotty.tools.dotc.reporting.StoreReporter(null):
      override def doReport(dia: Diagnostic)(using Context): Unit =
        if dia.level >= dotty.tools.dotc.interfaces.Diagnostic.ERROR then
          sink += dia.msg.message

  private def deleteRecursively(p: Path): Unit =
    if Files.exists(p) then
      Files.walk(p).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
