package okay.script

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap

/**
 * Modules: a definition crosses a file (specs/site-framework.md
 * stage 1).
 *
 * `include` composes OUTPUT and `declare` is per page, so until now a
 * `def` written on one page was unreachable from another — which is
 * why a site of any size ended as one enormous file. A page now
 * declares what it offers in its front matter and asks for it with a
 * markdown LINK, exactly as scalascript's `.ssc` does:
 *
 * {{{
 * ---
 * exports:
 *   - money
 * ---
 * [t, plural](/lib/i18n.md)
 * }}}
 *
 * The link reads as prose, survives a markdown renderer, and leaves
 * the file a document. Names rather than a wildcard: what crosses the
 * boundary is said, so a private helper does not become surface by
 * accident.
 */
object Modules:

  /** one import link: the names it asks for, and the file it asks */
  final case class Import(names: Vector[String], target: String, line: Int)

  /** what a page's `prepare` yields: the module outputs its compile
   * needs, the `import` lines to put above it, and a STAMP — the
   * newest mtime in its transitive module set, so a page recompiles
   * when a module it imports changes and not otherwise */
  final case class Prepared(classpath: Classpath, prelude: Vector[String], stamp: Long)

  object Prepared:
    val none: Prepared = Prepared(Classpath(Vector.empty), Vector.empty, 0L)

  private val fenceLine = """^\s*```.*""".r
  private val importLine = """^\s*\[([^\]]+)\]\(([^)\s]+)\)\s*$""".r

  /** the import links of a page: a whole line that is one markdown
   * link, outside every code fence. A link inside a sentence is
   * prose, not an import — the rule is the line, as it is in the
   * files this was taken from. */
  def imports(markdown: String): Vector[Import] =
    val out = Vector.newBuilder[Import]
    var inFence = false
    for (line, i) <- markdown.linesIterator.zipWithIndex do
      if fenceLine.matches(line) then inFence = !inFence
      else if !inFence then
        line match
          case importLine(names, target) =>
            val ns = names.split(",").toVector.map(_.trim).filter(_.nonEmpty)
            if ns.nonEmpty && ns.forall(isName) then out += Import(ns, target.trim, i)
          case _ => ()
    out.result()

  /** is this whole line one import link? What `imports` recognises,
   * asked of a single line — the renderer needs it to keep a
   * dependency out of the page's own output */
  def isImportLine(line: String): Boolean = line match
    case importLine(names, _) =>
      val ns = names.split(",").toVector.map(_.trim).filter(_.nonEmpty)
      ns.nonEmpty && ns.forall(isName)
    case _ => false

  /** a name a Scala identifier could be: a link to a heading or a URL
   * is prose, and this is what tells them apart without a parser */
  private def isName(s: String): Boolean =
    s.nonEmpty && (s.head.isLetter || s.head == '_') && s.forall(c => c.isLetterOrDigit || c == '_')

  /** the front matter's `exports:` list — what this page offers. The
   * shared `Meta` keeps only scalar front matter, so the list is read
   * here rather than by widening a type every page pays for. */
  def exports(markdown: String): Vector[String] = frontList(markdown, "exports")

  /** `route: false` — a library that answers no URL. The default is
   * true: a page is a page. */
  def routed(markdown: String): Boolean =
    !frontScalar(markdown, "route").exists(v => v == "false" || v == "no")

  private def front(markdown: String): Vector[String] =
    val lines = markdown.linesIterator.toVector
    if lines.headOption.exists(_.trim == "---") then
      val end = lines.indexWhere(_.trim == "---", 1)
      if end > 0 then lines.slice(1, end) else Vector.empty
    else Vector.empty

  private def frontScalar(markdown: String, key: String): Option[String] =
    front(markdown).collectFirst {
      case l if l.trim.startsWith(key + ":") => l.trim.drop(key.length + 1).trim
    }.filter(_.nonEmpty)

  private def frontList(markdown: String, key: String): Vector[String] =
    val lines = front(markdown)
    val at = lines.indexWhere(_.trim.startsWith(key + ":"))
    if at < 0 then Vector.empty
    else
      // the inline form `exports: [a, b]` and the block form, both —
      // a front matter is written by hand and both are written
      val inline = lines(at).trim.drop(key.length + 1).trim
      if inline.nonEmpty then
        inline.stripPrefix("[").stripSuffix("]").split(",").toVector.map(_.trim).filter(_.nonEmpty)
      else
        lines.drop(at + 1).takeWhile(l => l.trim.startsWith("- "))
          .map(_.trim.drop(2).trim).filter(_.nonEmpty)

  /** the object a module compiles to: stable for a path, and a legal
   * Scala identifier whatever the file is called */
  private[script] def objectName(root: Path, file: Path): String =
    val rel = if file.startsWith(root) then root.relativize(file).toString else file.toString
    val safe = rel.map(c => if c.isLetterOrDigit then c else '_').mkString
    val hash = Integer.toHexString(rel.hashCode).replace("-", "n")
    s"M_${safe}_$hash"

  val Package = "okay.script.mod"

  /**
   * The loader: compiles a page's modules, in dependency order, once
   * each, and caches them by mtime.
   *
   * A cycle is a hard error naming the ring: a module graph that
   * cannot be ordered cannot be compiled, and saying so beats a stack
   * overflow (the same choice `include`'s depth cap made).
   */
  final class Loader(root: Path, classpath: Classpath, tempRoot: Path):
    // `cp` is TRANSITIVE, not just this module's own directory: a
    // module's class references the classes of the modules IT
    // imports, so an importer that carried only the first level
    // compiled and then failed at run time with NoClassDefFoundError
    // (found by the diamond test, which is why it is a test)
    private final case class Built(mtime: Long, cp: Vector[Path], obj: String,
                                   exports: Vector[String], compiled: Compiled, stamp: Long)
    private val built = ConcurrentHashMap[Path, Built]()

    /** what `file` needs to compile: every module it imports,
     * transitively, already built */
    def prepare(file: Path, markdown: String): Either[Result, Prepared] =
      resolve(file, markdown, Vector(file))

    private def resolve(file: Path, markdown: String, ring: Vector[Path]): Either[Result, Prepared] =
      val links = imports(markdown)
      if links.isEmpty then Right(Prepared.none)
      else
        val acc = Vector.newBuilder[(Classpath, String, Long)]
        var failure: Option[Result] = None
        for link <- links if failure.isEmpty do
          target(file, link.target) match
            case None =>
              failure = Some(error(s"${file.getFileName}:${link.line + 1}: no such module '${link.target}'"))
            case Some(mod) if ring.contains(mod) =>
              val names = (ring :+ mod).map(p => rel(p)).mkString(" -> ")
              failure = Some(error(s"a module cycle cannot be ordered: $names"))
            case Some(mod) =>
              build(mod, ring) match
                case Left(r) => failure = Some(r)
                case Right(b) =>
                  val missing = link.names.filterNot(b.exports.contains)
                  if missing.nonEmpty && b.exports.nonEmpty then
                    failure = Some(error(
                      s"${rel(mod)} does not export ${missing.mkString(", ")} " +
                        s"(it exports ${b.exports.mkString(", ")})"))
                  else
                    val imp = s"import $Package.${b.obj}.{${link.names.mkString(", ")}}"
                    acc += ((Classpath(b.cp), imp, b.stamp))
        failure match
          case Some(r) => Left(r)
          case None =>
            val parts = acc.result()
            Right(Prepared(
              Classpath(parts.flatMap(_._1.entries).distinct),
              parts.map(_._2).distinct,
              parts.map(_._3).foldLeft(0L)(math.max)))

    private def build(mod: Path, ring: Vector[Path]): Either[Result, Built] =
      val markdown = Files.readString(mod)
      val mtime = Files.getLastModifiedTime(mod).toMillis
      Option(built.get(mod)) match
        case Some(b) if b.mtime == mtime && b.stamp >= mtime => Right(b)
        case _ =>
          // its own modules first -- depth before breadth, so a
          // diamond builds the shared module once and a ring is met
          // on the way down rather than after a compile
          resolve(mod, markdown, ring :+ mod).flatMap { inner =>
            val obj = objectName(root, mod)
            ScalaScript.compileModule(markdown, obj, Package,
              classpath ++ inner.classpath.entries, tempRoot, inner.prelude) match
              case Left(r) =>
                Left(Result(ok = false, stdout = "",
                  errors = r.errors.map(e => s"${rel(mod)}: $e"), thrown = r.thrown))
              case Right((c, dir)) =>
                Option(built.get(mod)).foreach(_.compiled.close())
                val b = Built(mtime, (dir +: inner.classpath.entries).distinct, obj,
                  exports(markdown), c, math.max(mtime, inner.stamp))
                built.put(mod, b): Unit
                Right(b)
          }

    /** relative to `root`, for a message a reader can act on */
    private def rel(p: Path): String =
      if p.startsWith(root) then root.relativize(p).toString else p.toString

    private def target(from: Path, spec: String): Option[Path] =
      val p =
        if spec.startsWith("/") then root.resolve(spec.drop(1))
        else from.getParent.resolve(spec)
      val norm = p.normalize
      Option.when(Files.isRegularFile(norm))(norm)

    private def error(message: String): Result =
      Result(ok = false, stdout = "", errors = Vector(message), thrown = None)

    def close(): Unit =
      built.values.forEach(_.compiled.close())
      built.clear()
