package okay.frege

import okay.{Choose, Reader, State, Throws}

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
 * if not). Your own effect's operations are one line each, the same way.
 */
object Ops {
  def ask(): AnyRef = Reader.Ask[Any, Any]()
  def get(): AnyRef = State.Get[Any, Any]()
  def set(s: Long): AnyRef = State.Set[Any, Any](Long.box(s))
  def raise(e: String): AnyRef = Throws[String, Nothing](e)
  def choose2(a: Long, b: Long): AnyRef = Choose(Seq(Long.box(a), Long.box(b)))
}
