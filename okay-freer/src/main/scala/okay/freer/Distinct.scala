package okay.freer

import okay.{TypeableK}

import scala.quoted.*

/**
 * A ROW WHOSE MEMBERS CAN ACTUALLY BE TOLD APART, checked by the
 * compiler.
 *
 * `split` decides a union by a runtime test on the left signature and
 * takes the right by exclusion. That is sound exactly when no
 * member's test accepts another member's operations. Nothing checked
 * it, and `TestRowIdentity` has demonstrated the consequence for as
 * long as it has existed: `Reader % Int + Reader % String` sends both
 * asks to the Int handler, and the String continuation dies of a
 * ClassCastException at the first wrong answer.
 *
 * `summon[Distinct[R]]` is that demonstration moved to compile time.
 *
 * WHAT IT COMPARES is the test, not the type, and the difference is
 * the whole design. Two members of the same signature are fine when
 * the signature's test reads the operation's VALUE — which is why
 * `Writer % String + Writer % Int` works and must keep compiling.
 * They are broken when the test is the erasure, which is the default
 * and the common case. No macro can read the semantics of a
 * hand-written `TypeableK`, so the instance declares it:
 * `TypeableK.ByValue` is the opt-in, `Writer.byValue.writerK` is the one
 * instance in this tree that carries it (imported where a row holds
 * two Writers; the default `writerK` is by class), and everything
 * unmarked is taken to test
 * by class. That direction is the safe one — an unmarked fine
 * instance is refused and fixed by one word, while the reverse would
 * pass a row that misroutes.
 *
 * THE TWO WRAPPERS carry identity of their own and are read
 * structurally: `Tag.Of[K, F]` collides only with the same key over a
 * colliding F, so `Of["a", Reader % Int] + Of["b", Reader % Int]` is
 * a good row and `Of["same", Reader % Int] + Of["same", Reader %
 * String]` is not. `Instances.Of[F]` would collide with another
 * `Instances.Of[F]`, and that is a case the language forbids before
 * the check can reach it: `+` is a UNION, `F | F` is `F`, so a row
 * cannot repeat a member at all — two `Instances.Of[Ping]` are the
 * one member it was written to be, and so are two `Reader % Int`.
 * What this check exists for is the pair that is two DIFFERENT types
 * with one runtime identity.
 *
 * WHAT IT CANNOT SEE it allows, deliberately: an abstract row (`G` in
 * an interpreter's residual), a row hidden behind a type alias that
 * dealiases past its own members. A check that fired on those would
 * be refusing what it does not know, and row-generic code —
 * `Logic`, the effectful streams — is written against exactly those.
 *
 * See docs/many-instances.md for what to do when a row DOES need two
 * instances of one signature.
 */
final class Distinct[R[+_]] private ()

object Distinct:

  inline given derive[R[+_]]: Distinct[R] = ${ okay.freer.macros.DistinctMacros.impl[R] }

  /**
   * THE ESCAPE HATCH, and the reason this is a class and not an
   * opaque `Unit`.
   *
   * `Row.In` is an opaque Unit and can be, because it is summoned
   * from OUTSIDE `Row` — where the opacity holds. A witness meant
   * to be summoned from inside `package okay`, as half this library's
   * rows are, cannot: the alias is transparent in its own scope, so
   * `Distinct[R]` reads as `Unit`, the given's type constrains R to
   * nothing at all, and implicit search satisfies EVERY row with the
   * macro run at some inferred R. Measured, not reasoned about — the
   * first cut of this file was the opaque one, and all four of
   * `TestDistinct`'s refusals silently passed while its four
   * acceptances passed for the wrong reason.
   *
   * So the witness is a real type, which leaves a real constructor to
   * account for: `unchecked` is it, deliberately public. Use it where
   * the macro cannot see and you can — a row behind an alias it
   * dealiases past, a test finer than its declared type — and say in
   * a comment which it is. You are promising what the macro otherwise
   * proves.
   */
  def unchecked[R[+_]](): Distinct[R] = shared.asInstanceOf[Distinct[R]]

  /** THE ONE CAST, and why it is right: the witness carries no data, so
   * every `Distinct[R]` is the same object whatever R is. Shared rather
   * than allocated because since distinct-on-handlers (2026-09-24) the
   * handlers ask for one on every call, and they run in measured loops. */
  private val shared: Distinct[Pure] = new Distinct[Pure]()

