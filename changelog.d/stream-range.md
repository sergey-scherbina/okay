## the machine's chunked stream overtakes the classic Chunks

Lane stream-range. `StreamCont.range` builds its chunk as a long[] filled by a
loop; `ArraySeq.range` boxed every element (25 B an element, found with
`-prof gc`). StreamContBenchmark per 100k: collected 450 → 227 us, mapped 474 →
256 — against the classic `Chunks`' 296 and 325, at the same allocation.
