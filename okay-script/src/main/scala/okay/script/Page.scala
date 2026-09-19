package okay.script

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

/** A `render`-mode `.md` file, compiled ONCE and cached by the file's
 * mtime, re-INVOKED (not re-compiled) on every `render()` call while
 * the file is unchanged -- the hot-reload half of "a new JSP": a JSP
 * page's servlet class compiles once and its `_jspService` runs once
 * per request, it does not recompile on every hit. See
 * specs/okay-script.md "Hot-reload".
 *
 * `Site` maps a directory of these to URLs and serves them over
 * okay-http/okay-jetty -- see "Site — the container".
 */
final class Page(path: Path, classpath: Classpath = Classpath.ambient, tempRoot: Path = ScalaScript.defaultTempRoot,
                 /** the site's module loader, when this page belongs to
                  * one (specs/site-framework.md stage 1): it builds what
                  * the page imports and says how fresh that is */
                 modules: Option[Modules.Loader] = None):
  // the module STAMP rides in the key beside the file's own mtime: a
  // page whose module changed is as stale as one whose own text did,
  // and nothing else can tell the two apart
  private var cached: Option[((FileTime, Long), Either[Result, Compiled])] = None

  /** Compiles on the FIRST call, or whenever `path`'s mtime has
   * changed since the last compile; otherwise re-invokes the
   * already-compiled program. Compile errors are reported the same
   * way `ScalaScript.render` reports them, through `Result.errors`.
   * Only the cache check/compile is locked: `web` is per-thread
   * (`okay.script.api.Web.current`), and `Compiled.invoke` captures
   * output per thread (`Capture`), so many requests can render the
   * same page at once -- two threads calling `render(webA)` and
   * `render(webB)` each see their own `Web`.
   */
  def render(web: api.Web = api.Web.current): Result =
    api.Web.scoped.where(web):
      compiled() match
        case Left(r) => r
        case Right(c) => c.invoke()

  private def compiled(): Either[Result, Compiled] = synchronized:
    val mtime = Files.getLastModifiedTime(path)
    val markdown = Files.readString(path)
    val prepared = modules match
      case None => Right(Modules.Prepared.none)
      case Some(l) => l.prepare(path, markdown)
    val stamp = prepared.fold(_ => -1L, _.stamp)
    cached match
      case Some((key, c)) if key == (mtime, stamp) => c
      case _ =>
        cached.foreach { case (_, Right(c)) => c.close(); case _ => () }
        val c = prepared.flatMap { p =>
          ScalaScript.compileRender(markdown, classpath ++ p.classpath.entries, tempRoot, p.prelude)
        }
        cached = Some((mtime, stamp) -> c)
        c

  /** Compiles the page WITHOUT invoking it, answering its compile
   * errors (empty when it compiled) -- what `Site.warm` calls at
   * boot so a broken page is named then rather than found by the
   * first visitor (okay-script-warm). Idempotent: the compiled
   * program is the one a later `render` re-invokes. */
  def warm(): Vector[String] = compiled() match
    case Left(r) => if r.errors.nonEmpty then r.errors else Vector(r.thrown.map(_.toString).getOrElse("failed"))
    case Right(_) => Vector.empty

  /** Releases the cached compiled program's classloader and deletes
   * its temp output directory. Call when no more `render()`s are
   * coming (e.g. the server that owns this `Page` is shutting down).
   */
  def close(): Unit = synchronized:
    cached.foreach { case (_, Right(c)) => c.close(); case _ => () }
    cached = None
