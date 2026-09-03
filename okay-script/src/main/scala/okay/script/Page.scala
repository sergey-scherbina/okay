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
 * Needs no dependency beyond what `ScalaScript.render` already has --
 * an actual HTTP route (an `okay-jetty` `PartialFunction[Request,
 * Response ! Async]` wrapping `render().stdout` into a `Response`) is
 * glue code a caller writes; `Page` itself stays inside `okay-script`.
 */
final class Page(path: Path, classpath: Classpath = Classpath.ambient):
  private var cached: Option[(FileTime, Either[Result, Compiled])] = None

  /** Compiles on the FIRST call, or whenever `path`'s mtime has
   * changed since the last compile; otherwise re-invokes the
   * already-compiled program. Compile errors are reported the same
   * way `ScalaScript.render` reports them, through `Result.errors`.
   * `web` is set FIRST, inside this call's own lock -- two threads
   * calling `render` concurrently on the SAME `Page` must never let
   * one thread's script read the other's `Web` (specs/okay-script.md
   * "Request context").
   */
  def render(web: Web = Web.current): Result = synchronized:
    Web.setCurrent(web)
    val mtime = Files.getLastModifiedTime(path)
    cached match
      case Some((t, c)) if t == mtime =>
        invoke(c)
      case _ =>
        cached.foreach { case (_, Right(c)) => c.close(); case _ => () }
        val markdown = Files.readString(path)
        val compiled = ScalaScript.compileRender(markdown, classpath)
        cached = Some(mtime -> compiled)
        invoke(compiled)

  private def invoke(c: Either[Result, Compiled]): Result = c match
    case Left(r) => r
    case Right(compiled) => compiled.invoke()

  /** Releases the cached compiled program's classloader and deletes
   * its temp output directory. Call when no more `render()`s are
   * coming (e.g. the server that owns this `Page` is shutting down).
   */
  def close(): Unit = synchronized:
    cached.foreach { case (_, Right(c)) => c.close(); case _ => () }
    cached = None
