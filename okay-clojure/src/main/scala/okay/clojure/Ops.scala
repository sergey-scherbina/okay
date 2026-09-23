package okay.clojure

import okay.{Async, Choose, Reader, State, Throws, Timer}
import okay.given

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
 * (refused by name if not). Your own effect's operations are one method
 * each, the same way.
 */
object Ops {
  def ask(): AnyRef = Reader.Ask[Any, Any]()
  def get(): AnyRef = State.Get[Any, Any]()
  def set(s: Any): AnyRef = State.Set[Any, Any](s)
  def raise(e: Any): AnyRef = Throws[Any, Nothing](e)
  def choose(options: java.util.List[?]): AnyRef =
    Choose(scala.jdk.CollectionConverters.ListHasAsScala(options).asScala.toSeq)

  /** `Async`: park for `millis` on the platform timer, answering the
   * milliseconds slept */
  def sleep(millis: Long): AnyRef =
    val timer = summon[Timer]
    Async.Await[java.lang.Long](k => timer.after(millis)(() => k(Right(Long.box(millis)))))
}
