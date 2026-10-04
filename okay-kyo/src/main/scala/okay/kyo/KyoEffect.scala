package okay.kyo

import okay.{!, Async, Free, effect}
import okay.!.*
import _root_.kyo.{<, Abort, Flat}

/**
 * KYO AS AN EFFECT OF THE TREE (specs/foreign-effects-in-tree.md). The row member is kyo's own pending type,
 * written with the value's place open — `Int ! <[*, Async & Abort[Nothing]]` — so okay invents no name for it
 * (`Kyo` is kyo's own object). `KyoEffect.perform(v)` keeps a kyo value as one operation, and `KyoEffect.run` lowers every
 * one of them to okay's `Async` when the program is handled. A row of kyo members ONLY: `A < S` is opaque, a pure
 * value of it is the value itself at run time, so no test can tell a kyo operation from another effect's and no
 * handler could split such a row (`perform` on a kyo value is refused for the same reason: `F[A]` unifies with
 * the wrong parameter of `A < S`).
 */
object KyoEffect:
  def perform[A: Flat, S](v: A < S): A ! <[*, S] = effect[<[*, S], A](v)

  /** every kyo step run as one okay `Async` operation, as `fromKyoAsync` runs one */
  def run[A, S >: Abort[Nothing] & _root_.kyo.Async](p: A ! <[*, S]): A ! Async = (p.resume: @unchecked) match
    case Return(a) => Return(a)
    case Inject(e) => step(e)
    case Bind(Inject(e), k) => runBind(e, k)

  private def runBind[A, S >: Abort[Nothing] & _root_.kyo.Async, X](e: X < S, k: X => A ! <[*, S]): A ! Async =
    Free.Bind(step[X, S](e), x => run(k(x)))

  // `Flat[X]` was proven at the door (`perform` asks for it); the step only names it again for kyo's runner
  private def step[X, S >: Abort[Nothing] & _root_.kyo.Async](e: X < S): X ! Async =
    KyoInterop.fromKyoAsync[X](e)(using Flat.unsafe.bypass[X])
