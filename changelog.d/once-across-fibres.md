## once-across-fibres - `SharedOnce`: one store for handles that cross fibres

`Once.run` threads its cells, so a `!.once` value demanded from two
fibres ran twice, each in its own store. `SharedOnce` (okay-async) is
the `memoize`/`Deferred` reading: one store, a demand met while the
program is in flight WAITS — an `Async` await resumed by the store —
and the first store wins. `run` for `Once + Async`, `runIn` forwarding
a wider row; a `translate` handler, so the machine needs nothing new;
one cast, the same as `Once.stored`'s, isolated in `cell`. Said out
loud in the doc: not replayable, and a knot is a hang rather than
`Once.run`'s exception (nothing in `Async` names a fibre). TestSharedOnce
(okay-platform, JVM): runs once across `par` with the second fibre
waiting; `Once.run` per fibre pinned as running twice for contrast;
store-then-demand; `runIn` with a `Writer`. specs/direct-macro.md
"Once across fibres"; docs/direct-style.md paragraph and example.
