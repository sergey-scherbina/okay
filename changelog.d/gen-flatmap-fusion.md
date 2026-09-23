## gen-flatmap-fusion - flatMap, ++ and zipWithIndex fused into the Gen reader

gen-chain-fusion left three barriers that materialised the chain as
walks: `flatMap`, `++`, `zipWithIndex`. Now a read is FROM a state —
`Chain.readState(K)(s0)` answers a `Halt` (the reader's state where
the walk ended, and whether a `Stop` ended it) and every `Xf` wraps
and unwraps the state it adds (`inject`/`project`) — so `++` is a
`Cat` node that reads its left side, then its right from that state
(a `take` over the concatenation counts through; each side's own
`take` counts its own; indices continue across), and `flatMap` is a
stage that reads the inner generator's chain from the reader's state
inside `add`, carrying an inner `Stop` as its own `done` so the whole
generation ends as the spliced program's would (`TERMINATION 2`'s
law, kept). `zipWithIndex` is a `Counted` stage. `program` still
materialises everything as walks for `iterator` and a block.
Laws (`TestGen` +3): fused = materialised = stepper on 200 random
chains with all three; the right side never runs when the left is
enough; a fused `flatMap` lazy to the inner counter.
Measured (compare `GenBenchmark`, `gfm-*`; bytes from the first
alternated pair — load-proof; the box carried sibling gates at load
85–200 for hours, so times await a quiet pair): `++` 287 → 239 B/elem
(the plain read's exactly), `flatMap` 495 → 407, `zipWithIndex`
311 → 215; the pipeline and identity lanes unmoved. Docs: the spec's
interface/laws/design, typepedia, direct-style "What it costs".
