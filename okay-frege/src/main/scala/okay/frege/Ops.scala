package okay.frege

import okay.Operations

/**
 * The core effects' operations, for Frege to `perform` (specs/frege.md):
 * each is a plain static method answering the operation VALUE, so a
 * Frege module binds it as a pure native and hands the value to
 * `perform`, which runs it in the okay program's row.
 *
 * {{{
 * pure native askOp okay.frege.Ops.ask :: () -> Operation Long
 * pure native getOp okay.frege.Ops.get :: () -> Operation Long
 * pure native setOp okay.frege.Ops.set :: Long -> Operation Long
 * }}}
 *
 * Untyped on the JVM, TYPED in Frege: the native's `Operation a` names
 * the answer (`Prog.perform :: Operation a -> Prog a`), and the okay row
 * decides at run time whether the operation is its own (refused by name
 * if not). The operations themselves are the core's `okay.Operations`
 * (interop-shared); these are the names Frege natives bind. Your own
 * effect's operations are one line each, the same way.
 */
object Ops {
  def ask(): AnyRef = Operations.ask()
  def get(): AnyRef = Operations.get()
  def set(s: Long): AnyRef = Operations.set(Long.box(s))
  def raise(e: String): AnyRef = Operations.raise(e)
  def choose2(a: Long, b: Long): AnyRef = Operations.choose(Seq(Long.box(a), Long.box(b)))

  /**
   * `Async`: park for `millis` on the platform timer (cancellable, as
   * `Async.sleep` is) and answer the milliseconds slept — an
   * `Operation Long` rather than `()`, whose Java form in Frege is a
   * `short` and would not take the boxed `Unit` a sleep answers.
   */
  def sleep(millis: Long): AnyRef = Operations.sleep(millis)

  /** a CALLBACK of the caller's, by name (foreign-one-ops): what a module
   * `okay.py.Jvm.frege` generated binds, answered by the `calls` given to
   * `Frege.run` rather than by the row */
  def call(name: String, arg: AnyRef): AnyRef = okay.Foreign.Call(name, arg)
}
