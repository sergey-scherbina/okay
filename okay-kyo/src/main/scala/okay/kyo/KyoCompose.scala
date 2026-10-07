package okay.kyo

import okay.Async
import okay.freer.{!}
import _root_.kyo.{<, Abort, Flat}

/**
 * ONE EXPRESSION ACROSS LIBRARIES (specs/interop-compose.md), kyo's
 * share: a kyo computation and a function returning one cross into okay
 * with okay's `asOkay` (this file's `ToOkay` instance); an okay program
 * and an `A => B ! Async` cross out with `asKyo`.
 */

/**
 * Any kyo computation whose effects an async run can discharge crosses
 * into okay as one `Async` operation — [[KyoInterop.fromKyoAsync]].
 * By the WHOLE type `A < S`, with `S` narrower than
 * `Abort[Nothing] & Async` (a pure `A < Any`, an `A < IO`): kyo's
 * contravariance in `S` is the evidence.
 */
given kyoToOkay[A: Flat, S](using ev: (A < S) <:< (A < (Abort[Nothing] & _root_.kyo.Async))): okay.ToOkay[A < S, A] =
  k => KyoInterop.fromKyoAsync(ev(k))

extension [A](p: => A ! Async)
  /** this okay program as a kyo IO suspension — [[KyoInterop.toKyo]] */
  def asKyo: A < _root_.kyo.IO = KyoInterop.toKyo(p)

extension [A, B](f: A => B ! Async)
  /** this okay function as a kyo one */
  def asKyo: A => B < _root_.kyo.IO = a => KyoInterop.toKyo(f(a))
