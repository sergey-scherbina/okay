package okay.script

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Reporter, Diagnostic}

import java.io.File
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
   * markers, which are split out as their own Interp segments. No
   * `startLine`: a raw string literal cannot itself carry a compile
   * error worth mapping precisely. */
  case Text(s: String)
  /** a ```scala fence's content, verbatim -- runs for its side
   * effects (definitions visible to LATER segments, direct output via
   * println), exactly like a `run` block. `startLine` is the 1-based
   * ORIGINAL markdown line of the block's first line (same convention
   * as `Block.startLine`) -- see specs/okay-script.md "Line-accurate
   * errors". */
  case Code(s: String, startLine: Int)
  /** a `${expr}` marker's inner expression -- evaluated, `.toString`
   * printed in place. `startLine` is the 1-based ORIGINAL markdown
   * line the `${` itself starts on. */
  case Interp(expr: String, startLine: Int)
  /** a ```scala declare fence's content -- emitted at OBJECT level,
   * outside `run`, so a `val` there is initialized once per compile
   * and a `def` there is a member every request can call (JSP's
   * `<%! %>`). See specs/okay-script.md "Declarations". */
  case Declare(s: String, startLine: Int)

final case class Result(
  ok: Boolean,
  stdout: String,
  errors: Vector[String],
  thrown: Option[Throwable],
)

/** `ScalaScript.check`'s verdict -- see specs/okay-script.md
 * "Output-comparison testing". `mismatches` is empty iff `ok`.
 */
final case class CheckResult(ok: Boolean, mismatches: Vector[String], run: Result)

/** An already-compiled program, invokable repeatedly WITHOUT
 * recompiling -- the compile/invoke split `Page` needs for hot-reload
 * (okay-script-page). See specs/okay-script.md "Hot-reload".
 */
trait Compiled:
  /** runs the ALREADY-COMPILED program again, fresh top-to-bottom --
   * no recompilation. */
  def invoke(): Result
  /** releases the classloader and deletes the temp output directory.
   * Must be called when no more `invoke()`s are coming. */
  def close(): Unit

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

  /** where okay-script's OWN classes live (a jar, or a classes
   * directory) -- what a page needs on its classpath to compile
   * against `okay.script.api`. Already inside `ambient`; a caller
   * with an explicit classpath appends this. See specs/okay-script.md
   * "Site — the container". */
  val api: Classpath = Classpath(
    Vector(Paths.get(classOf[okay.script.api.Web].getProtectionDomain.getCodeSource.getLocation.toURI))
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

  /** Parent directory for a compiled script's temp workspace --
   * defaults to the system default, but callers (tests, chiefly) can
   * pass a private directory instead so a "no litter left behind"
   * check watches only its own scoped tree rather than the whole
   * machine's shared temp dir, which any concurrent process can write
   * into (script-temp-tests-watch-a-shared-directory).
   */
  val defaultTempRoot: Path = Paths.get(System.getProperty("java.io.tmpdir"))

  private val fenceOpen = """```scala\s*""".r
  private val declareFenceOpen = """```scala\s+declare\s*""".r
  private val fenceClose = """```\s*""".r

  /** `blocks` takes BOTH kinds of scala fence -- a `using dep` in a
   * declare block must be found too */
  private def opensScala(line: String): Boolean =
    fenceOpen.matches(line) || declareFenceOpen.matches(line)

  def blocks(markdown: String): Vector[Block] =
    val lines = markdown.linesWithSeparators.toVector.map(_.stripLineEnd)
    val out = Vector.newBuilder[Block]
    var i = 0
    while i < lines.length do
      if opensScala(lines(i)) then
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

  private val stdoutFenceOpen = """```stdout\s*""".r

  /** Every ` ```stdout ` fence's content, in document order -- a
   * plain line-scanner mirroring `blocks`' own, independent of
   * `tokenize` (```stdout plays no role in compilation at all, only
   * in `check`) -- see specs/okay-script.md "Output-comparison
   * testing".
   */
  private def stdoutBlocks(markdown: String): Vector[String] =
    val lines = markdown.linesWithSeparators.toVector.map(_.stripLineEnd)
    val out = Vector.newBuilder[String]
    var i = 0
    while i < lines.length do
      if stdoutFenceOpen.matches(lines(i)) then
        var j = i + 1
        val body = Vector.newBuilder[String]
        while j < lines.length && !fenceClose.matches(lines(j)) do
          body += lines(j)
          j += 1
        out += body.result().mkString("\n")
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
    val textLines = scala.collection.mutable.ArrayBuffer.empty[String]
    var textStartLine = 0
    def flushText(): Unit =
      if textLines.nonEmpty then
        val path = currentPath
        splitInterpolations(textLines.mkString("\n"), textStartLine).foreach(seg => out += (seg -> path))
      textLines.clear()

    while i < lines.length do
      val line = lines(i)
      if opensScala(line) then
        flushText()
        val declare = declareFenceOpen.matches(line)
        val codeStart = i + 2 // 1-based line of the fence's first content line
        var j = i + 1
        val body = Vector.newBuilder[String]
        while j < lines.length && !fenceClose.matches(lines(j)) do
          body += lines(j); j += 1
        val code = body.result().mkString("\n")
        val seg = if declare then Segment.Declare(code, codeStart) else Segment.Code(code, codeStart)
        out += (seg -> currentPath)
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
            textStartLine = i + 1
            textLines += line
            i += 1
          case _ =>
            if textLines.isEmpty then textStartLine = i + 1
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

  /** The synthesized program, in two parts: `body` goes inside `def
   * run`, `decls` (the ```scala declare fences, in document order) at
   * object level above it. Each part carries its own line-origin map
   * -- see "Line-accurate errors". */
  private final case class Synth(body: String, bodyLines: Vector[Int], decls: String, declLines: Vector[Int])

  /** Wraps a sequence of (heading-path, source, startLine, isStatement)
   * items into ONE body PLUS a parallel line-origin map (one entry per
   * physical body line: the ORIGINAL markdown line it came from, `-1`
   * for a line with no original counterpart -- see
   * specs/okay-script.md "Line-accurate errors"). Indentation is
   * applied HERE, once, per physical line -- not by a caller
   * re-indenting already-assembled text afterward (that corrupted
   * multi-line string-literal DATA, see okay-script-web's Results).
   *
   * `isStatement = true` (a `render` segment's synthesized `print(...)`
   * call) indents ONLY its first physical line -- a `Text`/`Interp`
   * call's later physical lines, if any, are CONTINUATION DATA inside
   * a raw string literal (the lexer does not care about their
   * indentation at all), and adding spaces there would edit what gets
   * printed, the same class of bug the FIRST line-mapping attempt
   * found in `compileOnly` itself, here one level up. `isStatement =
   * false` (a verbatim ```scala `Code` segment) indents EVERY physical
   * line uniformly, preserving the block's own internal relative
   * indentation -- correct for genuine multi-line Scala source.
   *
   * A document with NO front-matter, yaml, or headings emits no
   * `okay.script.Meta` reference at all -- `run`/`render` stay
   * self-sufficient (scala-library only) for the common metadata-free
   * case, which is every script/example that predates this feature.
   * Otherwise: a `val` holding the whole `Meta.Doc` (literalized
   * ONCE), then -- whenever a path differs from the one before it --
   * `okay.script.Meta.setCurrent(...)` (a plain statement; `given`
   * cannot do this -- see Meta.scala's own comment: a plain `given` is
   * evaluated once, not per summon, confirmed empirically), then the
   * segment/block's own statement.
   */
  private def withMeta(
    doc: Meta.Doc,
    items: Vector[(Vector[Meta.Section], String, Int, Boolean)],
    declares: Vector[(String, Int)],
  ): Synth =
    val sb = new StringBuilder
    val origins = Vector.newBuilder[Int]
    def emitLine(text: String, origin: Int): Unit =
      sb ++= "    " ++= text ++= "\n": Unit
      origins += origin: Unit
    def emitCode(code: String, startLine: Int, isStatement: Boolean): Unit =
      val ls = code.linesWithSeparators.toVector.map(_.stripLineEnd)
      for (l, idx) <- ls.zipWithIndex do
        if idx == 0 || !isStatement then sb ++= "    " ++= l ++= "\n": Unit
        else sb ++= l ++= "\n": Unit
        origins += (if startLine < 0 then -1 else startLine + idx): Unit
    // object-level declarations: 2-space depth, every physical line
    // uniformly (genuine multi-line Scala source, never literal data)
    val db = new StringBuilder
    val dOrigins = Vector.newBuilder[Int]
    for (code, startLine) <- declares if code.nonEmpty do
      val ls = code.linesWithSeparators.toVector.map(_.stripLineEnd)
      for (l, idx) <- ls.zipWithIndex do
        db ++= "  " ++= l ++= "\n": Unit
        dOrigins += startLine + idx: Unit
    if hasMeta(doc) then
      emitLine("val _okayScriptDoc_ : okay.script.Meta.Doc = " + docLiteral(doc), -1)
      var prevPath: Option[Vector[Meta.Section]] = None
      for (path, code, startLine, isStatement) <- items do
        if !prevPath.contains(path) then
          val p = path.map(sectionLiteral).mkString(", ")
          emitLine(s"okay.script.Meta.setCurrent(okay.script.Meta.Context(_okayScriptDoc_, Vector($p)))", -1)
          prevPath = Some(path)
        if code.nonEmpty then emitCode(code, startLine, isStatement)
    else
      for (_, code, startLine, isStatement) <- items do
        if code.nonEmpty then emitCode(code, startLine, isStatement)
    emitLine("()", -1)
    Synth(sb.toString, origins.result(), db.toString, dOrigins.result())

  /** Splits a text run on `${expr}` markers (`$${` escapes to a
   * literal `${`). Brace-depth-aware, and quote-aware within the
   * expr -- a `{`/`}` inside a `"..."` span in the expression (e.g. a
   * NESTED `s"${x}"` string interpolation) does not affect the depth
   * that closes the marker. `textStartLine` (1-based, the ORIGINAL
   * markdown line `text`'s own first line came from) plus a running
   * newline count give each `Interp` segment its own `startLine`.
   */
  private def splitInterpolations(text: String, textStartLine: Int): Vector[Segment] =
    val out = Vector.newBuilder[Segment]
    val buf = new StringBuilder
    val n = text.length
    var i = 0
    var line = textStartLine
    while i < n do
      if i + 2 < n && text(i) == '$' && text(i + 1) == '$' && text(i + 2) == '{' then
        buf ++= "${"
        i += 3
      else if i + 1 < n && text(i) == '$' && text(i + 1) == '{' then
        if buf.nonEmpty then
          out += Segment.Text(buf.toString)
          buf.clear()
        val markerLine = line
        val start = i + 2
        var depth = 1
        var inString = false
        var escaped = false
        var k = start
        while k < n && depth > 0 do
          val c = text(k)
          if c == '\n' then line += 1
          if inString then
            if escaped then escaped = false
            else if c == '\\' then escaped = true
            else if c == '"' then inString = false
          else if c == '"' then inString = true
          else if c == '{' then depth += 1
          else if c == '}' then depth -= 1
          k += 1
        val exprEnd = if depth == 0 then k - 1 else k
        out += Segment.Interp(text.substring(start, exprEnd), markerLine)
        i = k
      else
        if text(i) == '\n' then line += 1
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
   * resolved and appended to it before compiling. If the document has
   * front-matter/yaml/headings, `okay.script.Meta.current` reflects
   * the right position for every block as execution reaches it -- see
   * specs/okay-script.md "Metadata as context".
   */
  def run(markdown: String, classpath: Classpath = Classpath.ambient, tempRoot: Path = defaultTempRoot): Result =
    resolvedClasspath(markdown, classpath).fold(identity, cp =>
      val doc = Meta.parse(markdown)
      val toks = tokenize(markdown)
      val items = toks.collect { case (Segment.Code(code, startLine), path) =>
        (path, code, startLine, false)
      }
      compileAndRun(withMeta(doc, items, declares(toks)), cp, tempRoot))

  private def declares(toks: Vector[(Segment, Vector[Meta.Section])]): Vector[(String, Int)] =
    toks.collect { case (Segment.Declare(code, startLine), _) => (code, startLine) }

  /** The whole document -- prose AND code -- as one program: `${expr}`
   * markers in prose (outside ```scala fences) are evaluated and their
   * `.toString` substituted in place; everything else passes through
   * verbatim (a ```yaml fence is METADATA and does not appear in the
   * output; any OTHER non-scala language tag still does). The
   * rendered document is `Result.stdout` on success. Same
   * `okay.script.Meta.current` `run` has. `web` seeds `Web.current`
   * for a script that reads it -- NOT synchronized (a one-shot call
   * was never meant to serialize); `Page.render` is the safe path for
   * concurrent per-request data -- see specs/okay-script.md "Request
   * context". See also "Interpolation" and "Metadata as context".
   * One-shot: compiles, invokes once, closes -- see `compileRender`/
   * `Page` for a compile-once-invoke-many alternative (specs/
   * okay-script.md "Hot-reload").
   */
  def render(markdown: String, classpath: Classpath = Classpath.ambient, web: api.Web = api.Web.current, tempRoot: Path = defaultTempRoot): Result =
    api.Web.setCurrent(web)
    compileRender(markdown, classpath, tempRoot).fold(identity, c => try c.invoke() finally c.close())

  /** `render`'s compile step, split from invocation: `Left` carries a
   * `Result` with dependency-resolution or compile errors (never
   * throws); `Right` an invokable `Compiled` handle, callable
   * repeatedly without recompiling -- the primitive `Page` (hot-reload)
   * is built on. See specs/okay-script.md "Hot-reload".
   */
  def compileRender(markdown: String, classpath: Classpath = Classpath.ambient, tempRoot: Path = defaultTempRoot): Either[Result, Compiled] =
    resolvedClasspath(markdown, classpath).flatMap: cp =>
      val doc = Meta.parse(markdown)
      val toks = tokenize(markdown)
      val items = toks.map {
        case (Segment.Text(s), path) if s.nonEmpty => (path, "print(" + scalaStringLiteral(s) + ")", -1, true)
        case (Segment.Text(_), path) => (path, "", -1, true)
        case (Segment.Interp(expr, startLine), path) => (path, s"print(($expr).toString)", startLine, true)
        case (Segment.Code(code, startLine), path) => (path, code, startLine, false)
        case (Segment.Declare(_, _), path) => (path, "", -1, true) // object-level, see `declares`
      }
      compileOnly(withMeta(doc, items, declares(toks)), cp, tempRoot)

  /** mdoc-style: runs the whole document once via `run`, then checks
   * every ` ```stdout ` fence's (trimmed) content appears as an
   * IN-ORDER substring of the actual output -- proving the right
   * output happened in the right relative sequence without injecting
   * a checkpoint into the compiled program itself. See
   * specs/okay-script.md "Output-comparison testing".
   */
  def check(markdown: String, classpath: Classpath = Classpath.ambient, tempRoot: Path = defaultTempRoot): CheckResult =
    val expected = stdoutBlocks(markdown)
    val r = run(markdown, classpath, tempRoot)
    if !r.ok then
      CheckResult(ok = false, mismatches = Vector(s"run failed before any output could be checked: ${r.errors.mkString("; ")}"), run = r)
    else
      var pos = 0
      val mismatches = Vector.newBuilder[String]
      for (chunk, i) <- expected.zipWithIndex do
        val needle = chunk.trim
        val idx = r.stdout.indexOf(needle, pos)
        if idx < 0 then
          mismatches += s"expected output #${i + 1} not found (in order, from position $pos): ${needle}"
        else
          pos = idx + needle.length
      CheckResult(ok = mismatches.result().isEmpty, mismatches = mismatches.result(), run = r)

  private def resolvedClasspath(markdown: String, classpath: Classpath): Either[Result, Classpath] =
    val coords = Deps.declared(markdown)
    Deps.resolve(coords) match
      case Deps.Resolved.Failed(msg) =>
        Left(Result(ok = false, stdout = "", errors = Vector(s"dependency resolution failed: $msg"), thrown = None))
      case Deps.Resolved.ToolMissing =>
        Left(Result(ok = false, stdout = "", errors = Vector("`using dep` declared but no cs/coursier found on PATH"), thrown = None))
      case Deps.Resolved.Jars(extra) =>
        Right(classpath ++ extra)

  private def compileAndRun(synth: Synth, classpath: Classpath, tempRoot: Path): Result =
    compileOnly(synth, classpath, tempRoot).fold(identity, c => try c.invoke() finally c.close())

  /** The script's classloader: isolated from the host (platform-only
   * parent, okay-script-classloader-isolation) for everything EXCEPT
   * two prefixes delegated to the loader that loaded okay-script
   * itself -- one class identity on both sides, the way a servlet
   * container shares the servlet API with an otherwise-isolated
   * webapp. `okay.script.api.*` is the API; `scala.*` is the runtime
   * that API is written in: an API method returning `Option`/`Map`
   * fails the JVM's loader constraint check (`LinkageError: loader
   * constraint violation`, found by TestWeb the first time the API
   * package alone was delegated) unless BOTH sides see the same
   * `scala.Option`. A script therefore runs on the host's Scala
   * runtime -- the same version dotc compiled it against. A `scala.*`
   * class the host does not have (none in practice) falls back to
   * the script's own classpath. See specs/okay-script.md "Site — the
   * container".
   */
  private final class ScriptClassLoader(urls: Array[java.net.URL], host: ClassLoader)
      extends URLClassLoader(urls, ClassLoader.getPlatformClassLoader()):
    override protected def loadClass(name: String, resolve: Boolean): Class[?] =
      val shared =
        if name.startsWith("okay.script.api.") then Some(host.loadClass(name))
        else if name.startsWith("scala.") then
          try Some(host.loadClass(name)) catch case _: ClassNotFoundException => None
        else None
      shared match
        case Some(c) =>
          if resolve then resolveClass(c)
          c
        case None => super.loadClass(name, resolve)

  /** Compiles `body` (an `object OkayScriptMain: def run(args: Array[
   * String]): Unit` body -- NOT `@main`, see the `args`-encoding
   * comment below for why) and, on success, loads it into a fresh
   * isolated `URLClassLoader` -- platform-only parent, see
   * okay-script-classloader-isolation -- WITHOUT invoking or deleting
   * anything: the returned `Compiled` owns both (its `close()` deletes
   * the temp dir this creates).
   */
  private def compileOnly(synth: Synth, classpath: Classpath, tempRoot: Path): Either[Result, Compiled] =
    // `synth`'s parts are already at their FINAL depth (withMeta
    // indents per physical line, once) -- do NOT re-indent here by
    // prefixing every physical line: the body can contain a Text
    // segment's raw triple-quoted string literal spanning several
    // physical lines, and blindly adding spaces to EVERY line would
    // corrupt the LITERAL DATA inside it, not just the source
    // formatting (found by TestScalaScriptRender's no-interpolation
    // test failing with extra leading spaces on every rendered line,
    // when this WAS a line-prefixing pass).
    //
    // Shape: `object OkayScriptMain:` / the declare fences at object
    // level / `def run(args): Unit =` / the body. `args` is unused
    // since okay-script-site: the request reaches a page through the
    // shared `okay.script.api` package, not through an encoding.
    val declLines = synth.decls.linesWithSeparators.toVector
    val wrapped = "object OkayScriptMain:\n" + synth.decls + "  def run(args: Array[String]): Unit =\n" + synth.body
    // one entry per PHYSICAL line of `wrapped`, aligned with dotc's
    // own 0-based line() -- the two header lines have no original
    // counterpart (-1); the declare and body maps (built alongside
    // their text in `withMeta`) supply the rest, entry for entry.
    assert(declLines.length == synth.declLines.length, "declare line map out of step with its text")
    val fullLineMap = (-1 +: synth.declLines) ++ (-1 +: synth.bodyLines)

    val dir = Files.createTempDirectory(tempRoot, "okay-script-")
    val srcFile = dir.resolve("OkayScriptMain.scala")
    Files.writeString(srcFile, wrapped)
    val outDir = Files.createDirectory(dir.resolve("out"))

    val diagnostics = Vector.newBuilder[String]
    val reporter = collectingReporter(diagnostics, fullLineMap)

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
      deleteRecursively(dir)
      Left(Result(ok = false, stdout = "", errors = errs, thrown = None))
    else
      // platform-only parent (okay-script-classloader-isolation,
      // 2026-09-03): getClass.getClassLoader would let a script
      // resolve anything on okay-script's OWN build classpath
      // (URLClassLoader is parent-first) regardless of what the
      // caller actually put in `classpath` -- defeating the
      // isolation Classpath/Deps exist for. A script sees exactly
      // its own compiled classes, its own Classpath, and the JDK.
      val loaderUrls = (outDir +: classpath.entries).map(_.toUri.toURL).toArray
      val loader = new ScriptClassLoader(loaderUrls, getClass.getClassLoader)
      val cls = loader.loadClass("OkayScriptMain")
      val method = cls.getMethod("run", classOf[Array[String]])

      // okay-script-page (2026-09-03) found that host-side
      // `scala.Console.withOut` never reached a script's `println`:
      // the isolated loader had its OWN `scala.Console`, bound at
      // first touch to whatever `System.out` was then, forever. The
      // fix was reflection into that copy on every invoke. Since
      // okay-script-site `scala.*` is delegated to the host (see
      // ScriptClassLoader), so the script's `println` IS the host's
      // `Console` -- and `Capture.capturing` scopes it per thread with
      // plain `Console.withOut`, no reflection, concurrent-safe.
      Right(new Compiled:
        def invoke(): Result =
          val (thrown, out) = Capture.capturing[Option[Throwable]] {
            try
              method.invoke(null, Array.empty[String]): Unit
              None
            catch
              case e: java.lang.reflect.InvocationTargetException =>
                Some(Option(e.getCause).getOrElse(e))
              case e: Throwable =>
                Some(e)
          }
          Result(ok = thrown.isEmpty, stdout = out, errors = Vector.empty, thrown = thrown)

        def close(): Unit =
          loader.close()
          deleteRecursively(dir))

  /** `lineMap` maps a 0-based synthetic-source line (dotc's own
   * `SourcePosition.line()` convention, confirmed empirically -- see
   * specs/okay-script.md "Line-accurate errors") to the 1-based
   * ORIGINAL markdown line, or `-1` when there is none. A diagnostic
   * WITH a position that maps to a real line is prefixed `"L<n>: "`;
   * one with no position, or a position outside/unmapped in
   * `lineMap` (synthesized code -- an `okay-script` bug, not the
   * markdown author's), is reported bare, exactly as before this
   * feature.
   */
  private def collectingReporter(
    sink: scala.collection.mutable.Builder[String, Vector[String]],
    lineMap: Vector[Int],
  ): Reporter =
    new dotty.tools.dotc.reporting.StoreReporter(null):
      override def doReport(dia: Diagnostic)(using Context): Unit =
        if dia.level >= dotty.tools.dotc.interfaces.Diagnostic.ERROR then
          val origin =
            if dia.position().isPresent then
              lineMap.lift(dia.position().get().line()).filter(_ >= 1)
            else None
          sink += origin.fold(dia.msg.message)(n => s"L$n: ${dia.msg.message}")

  private def deleteRecursively(p: Path): Unit =
    if Files.exists(p) then
      Files.walk(p).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
