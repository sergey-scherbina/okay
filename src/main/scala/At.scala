package okay

import scala.quoted.*

/**
 * WHERE THIS WAS WRITTEN, as a compile-time constant
 * (delim-diagnostics, 2026-09-17).
 *
 * A captured continuation has no useful JVM stack trace — it is
 * resumed on another thread, in another process, a week later, and
 * the interpreter's own frames are what a debugger shows. What the
 * machine CAN say is where in the program's own structure it is, and
 * that needs one fact the compiler has and the runtime does not: the
 * source position of a call site.
 *
 * It is a GIVEN rather than an `inline def` called by hand, because
 * the position wanted is the CALLER's: a method that takes
 * `(using At)` gets the line of whoever called it, since implicit
 * search runs there. A library then needs no inline wrapper per door.
 *
 *     def delimited[R, F[+_]](body: …)(using At): R ! F
 *     Delim.delimited[Int, Pure](…)      // At("Booking.scala:31")
 *
 * Cost: one reference to an interned string literal per call that
 * builds a prompt. Nothing is computed at run time.
 *
 * THE ONE RULE FOR USING IT INSIDE THIS MODULE: a macro cannot be
 * expanded in the compilation run that defines it, so `okay`'s own
 * main sources must never SUMMON an `At` — they thread the one their
 * caller supplied. `Delim` does exactly that: every internal call
 * passes its own `using` parameter along, and nothing in
 * `src/main/scala` writes `summon[At]` or calls an `At`-taking method
 * without one in scope.
 */
final case class At(where: String) extends AnyVal:
  override def toString: String = where

object At:

  /** for a call that has no position to offer — a prompt built by
   * machinery rather than by a line of somebody's program */
  val unknown: At = At("<unknown>")

  /** the caller's `file:line` */
  inline given here: At = ${ hereImpl }

  // NOT private: an inline body reaching a private member makes the
  // compiler synthesize an accessor with an unstable name (E192) —
  // the same finding as `Delim.Emitting.in`'s and `Cont.Shift`'s,
  // measured here 2026-09-17
  def hereImpl(using Quotes): Expr[At] =
    import quotes.reflect.*
    val pos = Position.ofMacroExpansion
    val name =
      try pos.sourceFile.name
      catch case _: Throwable => "<unknown>"
    val line = pos.startLine + 1
    val where = Expr(s"$name:$line")
    '{ At($where) }
