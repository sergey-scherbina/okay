## put-de-diagonal - Put's answer is Unit, not the element

`Put[S[_]]` was diagonal: `put[A](a: A): A /> F[A]` forced every
carrier to answer with the value it was just told, which is why the
third instance was `Teller[A] = A ! Writer % A` and why no real seam
ever took one. Measured 2026-09-19: outside `Generate.scala`, zero
production call sites of `Put`/`take`/`put`/`generate` — `nats`/`fibs`
are the only real callers, and the echoed answer bought nothing.

Now `Put[S[_]] { def put[W](w: W): Unit /> S[W] }`. Every instance
survives, resuming its continuation with `()` instead of the element:
`LazyList` (still captured in the lazy tail), `Producer` (still an
ordinary emit). `Teller` is gone, replaced by the sound writer-stream
carrier `Unit ! Writer % W` — one unfold, four carriers now, not
three. `Source` (okay-stream) gains a `Put` instance it structurally
could not have had before: its own answer is always `Unit`, never the
element, so no diagonal `Put[Source]` could ever have existed.
`generate`/`nats`/`fibs` now produce a live, asynchronous source for
free.

`docs/guide.md` gets the "one unfold, four carriers" section it never
had. Gate: `TestGenerate`/`TestStream` (core) and a new
`TestSourceProducer` case proving the Source carrier actually streams
(pulls 5 nats and 10 fibs off a live `Source`, not just compiles).

Pairs with `producer-to-writer-carrier` (BACKLOG, in progress in
parallel): that lane decides whether `Producer` stays; this one only
needed `Put[Producer]` to stop echoing.
