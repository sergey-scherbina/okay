package okay.frege

import okay.{Choose, Reader, State, Throws}

/**
 * The core effects' operations, for Frege to `perform` (specs/frege.md):
 * each is a plain static method answering the operation VALUE, so a
 * Frege module binds it as a pure native and hands the value to
 * `perform`, which runs it in the okay program's row.
 *
 * {{{
 * data Obj = pure native java.lang.Object
 * pure native askOp okay.frege.Ops.ask :: () -> Obj
 * pure native getOp okay.frege.Ops.get :: () -> Obj
 * pure native setOp okay.frege.Ops.set :: Long -> Obj
 * }}}
 *
 * Untyped at this edge on purpose: the operation's answer type lives in
 * the Frege signature of the `perform` that uses it, and the okay row
 * decides at run time whether the operation is its own (a `Left` by
 * name if not). Your own effect's operations are one line each, the
 * same way.
 */
object Ops {
  def ask(): AnyRef = Reader.Ask[Any, Any]()
  def get(): AnyRef = State.Get[Any, Any]()
  def set(s: Long): AnyRef = State.Set[Any, Any](Long.box(s))
  def raise(e: String): AnyRef = Throws[String, Nothing](e)
  def choose2(a: Long, b: Long): AnyRef = Choose(Seq(Long.box(a), Long.box(b)))
}
