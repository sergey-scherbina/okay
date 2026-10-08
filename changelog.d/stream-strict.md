## the machine's stream chunk is strict

Lane stream-strict. `StreamCont`'s chunk is an `ArraySeq` (as the classic
`Chunks`), not a lazy view: range collected 593 → 469 us per 100k, mapped
within noise (659 → 635); the classic `Chunks` is 296 and 325. A pure `map` now
computes its chunk at once; `take` still never pulls the producer past the
chunk it needs, and `fromIterator` still reads one element a pull.
