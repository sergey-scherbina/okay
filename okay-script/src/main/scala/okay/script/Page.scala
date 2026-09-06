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
final class Page(path: Path, classpath: Classpath = Classpath.ambient, tempRoot: Path = ScalaScript.defaultTempRoot):
  private var cached: Option[(FileTime, Either[Result, Compiled])] = None

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
    api.Web.setCurrent(web)
    compiled() match
      case Left(r) => r
      case Right(c) => c.invoke()

  private def compiled(): Either[Result, Compiled] = synchronized:
    val mtime = Files.getLastModifiedTime(path)
    cached match
      case Some((t, c)) if t == mtime => c
      case _ =>
        cached.foreach { case (_, Right(c)) => c.close(); case _ => () }
        val markdown = Files.readString(path)
        val c = ScalaScript.compileRender(markdown, classpath, tempRoot)
        cached = Some(mtime -> c)
        c

  /** Releases the cached compiled program's classloader and deletes
   * its temp output directory. Call when no more `render()`s are
   * coming (e.g. the server that owns this `Page` is shutting down).
   */
  def close(): Unit = synchronized:
    cached.foreach { case (_, Right(c)) => c.close(); case _ => () }
    cached = None
