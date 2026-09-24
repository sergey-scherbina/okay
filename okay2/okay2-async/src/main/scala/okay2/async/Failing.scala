package okay2.async

import okay2._
import okay2.Split.over

/**
 * How a forwarded ASYNC operation reports its failure to the Resource
 * scope that forwarded it: a `Run` thunk throws, an `Await` callback
 * answers Left — the only two ways an operation fails on the outer
 * handler — and `guard` attaches the hook that runs first.
 *
 * TWO INSTANCES, as in the Scala 3 core: the typed one for `Async`
 * alone, and the default for ANY row, which is the typed one LIFTED
 * over the row by the kernel's prism (`Split.over`): the class test
 * proves the operation is an Async, the typed instance rebuilds it,
 * and `over` puts it back under the row's type — the one cast a row
 * costs, made in the kernel beside `split`'s, not here. A row with no
 * Async operation in it never reaches it. Both arrive with
 * `import okay2.async._`; the value wins over the polymorphic def for
 * `Failing[Async]` by specificity.
 */
object AsyncFailing {
  val async: Failing[Async] = new Failing[Async] {
    def guard[X](e: Async.Op[X], onFailure: () => Unit): Async.Op[X] = e match {
      case Async.Run(f) => Async.Run(() => try f() catch { case t: Throwable => onFailure(); throw t })
      case Async.Await(reg) => Async.Await(k => reg { r => if (r.isLeft) onFailure(); k(r) })
    }
  }
}

trait AsyncFailingLow {
  /** ANY row, by the operation's own class — total over every nesting */
  implicit def anyRowFailing[F <: Row]: Failing[F] = new Failing[F] {
    def guard[X](e: F#Op[X], onFailure: () => Unit): F#Op[X] =
      over[Async, F, X](e)(AsyncFailing.async.guard(_, onFailure))
  }
}
