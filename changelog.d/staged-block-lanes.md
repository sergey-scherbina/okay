## staged-block-lanes - two fusion predictions refuted upward, both roads reopened with numbers

Two JMH lanes, thresholds written into the specs BEFORE the run, two
rounds × two forks, per-lane minima, `-prof gc` (history rows `sbl-*`).

**Road 2's real shape** (`FusionBenchmark.block*`): the same 1 000
operations as `rightSW` grouped as a STATIC 10-op block inside a
recursive loop — what a `direct` block inside a loop is, where stage B
had measured one operation per step. Six ways. The ceiling — binds AND
handler static, each operation written as its shift arm, nothing
dispatched — is **1.55x** over the fused Free fixture (7.53 vs
11.67 µs, 82 968 vs 123 712 B/op) and **1.99x** over the shipping
`State.run(Writer.run(_))` on the same tree. Prediction was ~1.25x.
The lane beside it names the lever: the same block over Func with the
handler passed as a VALUE is **0.89x** — static binds buy nothing, the
entire win is the handler being known per operation at compile time
so that no `split` runs. So the emission target worth building is not
"`direct` over Control with an opaque handler" (refuted) but "`direct`
with the handler inlined per operation", which needs handlers the
macro can read — filed on `direct-staged` with the numbers, a design
before a lane. Free finding: the tree built-and-run equals the tree
prebuilt (11.70 vs 11.67, +80 B) — a Free program is re-materialised
by every `k(x)`, so prebuilding saves nothing on a loop.

**`Handler.flat`'s ceiling** (`FlatDispatchBenchmark`): a hand-written
one-match handler over `E1 + (E2 + (E3 + E4))` against the nested
`Handler.union` chain, 10 000 right-nested operations. Position 4:
**1.24x** (87.9 vs 108.6 µs); position 1: 1.08x; bytes identical to
the byte on all four lanes (957 904) — pure dispatch. Prediction was
"under 5%". Threshold (1.1x) cleared: `handler-fusion-flat` promoted
from GATED OFF to the sprint queue with the shape, laws and number it
must hit. It had been gated off by stage 0's pass-fusion number, which
said nothing about comonadic dispatch — the wrong gate for the box.
