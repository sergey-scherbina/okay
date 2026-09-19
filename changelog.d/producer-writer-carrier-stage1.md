## producer-writer-carrier-stage1 - the pure writer stream is named Feed[W]

`specs/producer-to-writer-carrier.md` stage 1 was conditioned on stage
0's verdict — "only if the loss is confined to `Chunks`" — and it was
(`Chunks.fold` specifically, not the carrier). This lands the rule and
the name.

`type Feed[W] = Unit ! Writer % W` (`src/main/scala/Writer.scala`),
the operator's choice over no-alias and `Told[W]`, symmetric with
`Source[W] = Unit ! (Writer % W + Async)` — Source is Feed with Async
added. Every instance a plain type alias inherits for free: the `Put`
instance that used to sit on an anonymous type now reads `Put[Feed]`,
and the existing `Stream[[W] =>> A ! Writer % W, Pure]` given already
covers it at `A = Unit`. `docs/guide.md` states the rule for new
seams: name the element in the type — `Feed[W]` with no other effect,
`Source[W]` under `Async`; `Producer`'s identity signature stays the
pure special case for code already written against it, not the
default for code being written now.

One correction from the plan, found while writing the proof rather
than assumed: the `pure(a)` trap is NOT closed by a type error at
`Feed`/`Source`. `compileErrors` turned out to be the wrong tool to
test it — it reports hard compile errors only, and munit's macro drops
warnings entirely (checked directly against the classic `val u: Unit
= 5` shape before trusting this, since `okay-stream`'s own
`TestSourceProducer` carried an identical, never-actually-verified
claim about `-Wvalue-discard` in a comment). What a REAL compile shows
for `val f: Feed[Int] = pure(5)`: `[E190] Potential Issue Warning:
Discarded non-Unit value of type Int`, which this repo's zero-warnings
gate refuses like any other warning — the trap is made inspectable,
not type-closed, and `Producer`'s identical-looking mistake gives a
reviewer no such tell at all. `TestGenerate` documents the fact and
asserts what compileErrors CAN prove (no hard error, same as
`Producer`); `TestSourceProducer`'s comment is left alone — not wrong
in substance, just under-proven by its own test, and not this lane's
file to touch.

`sprint.d/queue/producer-to-writer-carrier.md` names stage 2 as the
next claimable slice, per module, leaves first, `Chunks` last and
gated on a chunk-aware writer fold that does not exist yet (stage 0's
finding).
