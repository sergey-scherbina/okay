package okay

import okay.!.*

/**
 * The Reader effect: ask for an environment of type R. The handler
 * answers every Ask with the same value, which makes it exactly
 * tail-resumptive — run is the relay, at relay speed.
 */
enum Reader[R, +A] {
  /** read the environment */
  case Ask() extends Reader[R, R]
}

object Reader {
  /** the environment */
  inline def ask[R]: R ! Reader % R = effect(Ask())

  /** answer every Ask with r, forwarding the effects F */
  def run[R, A, F[+_]](r: R)(a: A ! Reader % R + F): A ! F =
    relay[A, A, Reader % R, F](a)(pure(_)):
      [X, Y] => e => e match
        case Ask() => Cont.Pure(r)
}
