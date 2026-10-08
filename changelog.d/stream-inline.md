## the machine's stream maps a chunk by an inline loop

Lane stream-inline. `StreamCont`'s `map`, `filter` and `foldLeft` are `inline`:
the chunk loop is built once at the call site (the classic `Chunks`'
`ChunkBuf.mapper`/`filterer`) and the recursion takes it as a value. Range
mapped 635 → 474 us per 100k, collected 469 → 450 (classic `Chunks`: 325 and
296).
