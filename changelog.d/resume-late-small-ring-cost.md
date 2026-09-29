## resume-late-small-ring-cost — a foreign answer goes home with fork, not forkLong

f50932cbe sent a fiber answered from outside the pool home with
`forkLong`, which unparks a sleeper every time. A 7-slot `Source.zip`
resumes a side every few elements and paid it on each: 3709 us an op,
and the zip's lost-pairs race (a sibling's source-zip-lost-pairs) met ~6x
more often. Measured on one build with the handoff as a temporary
switch: `fork` (wakes only when nobody is awake) reads merge 92 / 61 us at
capacity 7 / 64 and zip 1383 / 370 — faster than Loom everywhere and than
`forkLong` everywhere; the one shape it loses is a tiny-ring zip against
running the producer inline (1383 vs 483). New diagnostic
`ZipCapBenchmark`. specs/adaptive-elementwise-small-ring.md, "Correction".
