package okay.clojure

import okay.Operations

/**
 * The core effects' operations, for Clojure to `perform`
 * (specs/clojure.md, stage 2): plain static methods answering the
 * operation VALUE, reached by Java interop —
 *
 * {{{
 * (ok/mlet [env (ok/perform (okay.clojure.Ops/ask))
 *           _   (ok/perform (okay.clojure.Ops/set (inc env)))]
 *   (ok/done env))
 * }}}
 *
 * The okay row decides at run time whether an operation is its own
 * (refused by name if not). The operations themselves are the core's
 * `okay.Operations` (interop-shared); these are the names Clojure binds.
 * Your own effect's operations are one method each, the same way.
 */
object Ops {
  def ask(): AnyRef = Operations.ask()
  def get(): AnyRef = Operations.get()
  def set(s: Any): AnyRef = Operations.set(s)
  def raise(e: Any): AnyRef = Operations.raise(e)
  def choose(options: java.util.List[?]): AnyRef =
    Operations.choose(scala.jdk.CollectionConverters.ListHasAsScala(options).asScala.toSeq)

  /** `Async`: park for `millis` on the platform timer, answering the
   * milliseconds slept */
  def sleep(millis: Long): AnyRef = Operations.sleep(millis)

  /** a CALLBACK of the caller's, by name (foreign-one-ops): what a namespace
   * `okay.py.Jvm.clojure` generated calls, answered by the `calls` given to
   * `Program.run` rather than by the row */
  def call(name: String, arg: Any): AnyRef = okay.Foreign.Call(name, okay.Foreign.obj(arg))
}
