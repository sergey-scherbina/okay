## fused-out-of-core - Fused is a test fixture, not a core API

`src/main/scala/Fused.scala` moved to `src/test/scala/Fused.scala`.
It held the handler-fusion arc's stage-0 hand-written loops and the
stage-B composite `Control` interpreter — the artifacts specs/handler-fusion.md
measured against (stage 0 gated OFF at 1.13–1.29x, stage B REFUTED at
0.58–0.86x). Nothing outside `TestFused` and `FusionBenchmark` called
it, and nothing will: a grep of every module's main sources found no
file that runs two continuation-aware handlers at all, so the shape
fusion would speed up does not occur in production code. The Jmh
configuration extends Test, so the benchmark still sees it and
`fusedSWr` stays as THE FLOOR (122 640 B/op) that every runner change
is held to. Core loses 150 lines it never called.

Prose that presented `Fused` as a library road reworded: README,
docs/guide.md and docs/tutorial.md now name `Interpr` and say when the
inline program actually wins (static at the call site); the `Cont`
and `Handler` comments point at the probes' new home; `direct-staged`
in the backlog notes that its `Fused.runCtrl` call site is now a test
fixture and would need a library-side runner first.
