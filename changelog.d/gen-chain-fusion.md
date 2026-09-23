## gen-chain-fusion - a Gen chain read as one walk: the pipeline under the hand road

`g.map(f).filter(p).take(n).toList` was three walks and a reader — a
program re-telling into the next per element. Now `Gen[W]` (still a
value class) holds a `Chain[W]`: a source program and `Xf`, a chain of
stages with two readings — FUSED, a transformer of the reader
(Hickey's transducer with the state each stage adds carried as a type
member, `St[S]`, so `Gen.read` is unchanged and `done` decides for the
whole chain), and MATERIALISED, the walks it was (`program`, for
`iterator`, `flatMap`, `++`, a `Gen` marked in a block). `map`/`filter`/
`take`/`takeWhile`/`drop` append a stage; every stopping reader walks
the source once, `f` and `p` inside `add`; a fused `take` is done at
its n-th kept element and the body runs no further.
Measured (compare `GenBenchmark`, quiet alternated pairs, `gcf-*`):
`map.filter.toList` over 10k 282 → 202 µs, 354 → 231 B/elem — 0.72 /
0.65 of the walks and UNDER the hand road (215 / 272), which still
walks `Writer.map`; `take(10k)` 246 → 189 µs, 295 → 239 B/elem; the
identity chain byte-identical. Two first cuts the rows refuted before
landing: `program` as a def that always delays (+96 B/elem under every
walk's `emit(w).program` — now a method of `Chain`, a plain chain IS
its source, and the walks say `say(w)`), and `(Int, S)` as `take`'s
state (boxed the count, +40 B/elem — now a `Counted[S]` class). Laws:
`TestGen` +2 (fused = materialised = stepper on 200 random chains;
the counter through `map.filter.take`; `find` through `map`; `take(0)`
runs nothing; a `Stop` ends a fused read). Docs: direct-style "What it
costs", benchmarks §21, theory ch. 7 (Hickey 2014), typepedia;
specs/gen-chain-fusion.md closed.
