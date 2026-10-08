## the machine's stream pulls a chunk at a time

Lane stream-chunks. `StreamCont`'s pull answers a chunk of up to 256 elements
(a lazy view) instead of one, so its program nodes are paid per chunk; `map` and
`filter` stay lazy per element and `take` computes no element past what it
keeps. StreamContBenchmark: range collected 2786 → 593 us per 100k, mapped 4578
→ 659 (the classic per-element Source: 1323 and 3842).
