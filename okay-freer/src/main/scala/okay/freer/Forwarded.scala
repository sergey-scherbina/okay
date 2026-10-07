package okay.freer


/**
 * THE MATCHED NODE, FORWARDED (relay-forward-same-inject, 2026-09-27).
 * A handler loop that meets `Inject(e)` and finds by `split` that `e` is
 * NOT its effect forwards the operation to the rest of the row — and it
 * used to build a new `Inject(e)` to do it: 16 of the 72 B a forwarded
 * operation cost per handler level (effect-row-cost's `ProbeRowCost`).
 * The node it matched IS that `Inject`: the erased row changes nothing
 * in it, and `split`'s test has just proved `e` a `G[X]`, so the node is
 * a `Free[G, X]` by the claim `split` already makes — the excluded
 * middle of the union — made once more here, as ONE door instead of a
 * cast at every forwarding site. Call it ONLY on the node whose
 * operation `split` sent to its G arm. (`ReadyMerge` forwards its
 * sources' `Inject(Say)` the same way, merge-cap256-gap.)
 */
//
// The row is named, the answer type is not: a forwarding site holds the
// node under an existential answer type it cannot write, so `X` comes
// by inference in a clause of its own — the `using` between the two
// type clauses is only what lets them be two (`split`'s shape).
inline def forwarded[F[+_], G[+_]](using DummyImplicit)[X](i: Free[F + G, X]): Free[G, X] =
  i.asInstanceOf[Free[G, X]]

