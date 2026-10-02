## okay-bayes-test-budget - okay-bayes's tests sized for a shared, loaded box

The ci-runner's whole build went red again on an okay-bayes timeout:
`TestVector`'s eight schools by adaptive Metropolis, 79 s against munit's
30 s at load ~30 (5.4 s quiet; passed alone, so recorded as a flake and
pushed). Every okay-bayes test timed on JVM, Scala.js and Native, and the
work cut where it bought nothing: eight schools adaptive 20 000 → 4 000
draws and AD NUTS 3 000 → 1 500, the Bulk adaptive 4 000 → 2 000, the ch.3
mixture's chains 25 000 → 10 000 and its NUTS 2 000 → 1 000, Bulk NUTS
1 000 → 600, Dark Worlds' ten skies 1 000 → 500 each. `TestVector` takes
the module's sampling-suite timeout (5 min). Quiet-box totals: ten skies
57 → 44 s, ch.3 NUTS 57 → 33 s, Bulk NUTS 19 → 13.5 s, ch.3 adaptive
19 → 11 s; every check still passes (ten skies 10 of 10, median 37).
Dark Worlds' start search (operator: "why a Vector?") is now ONE pass over
the Bulk — the accumulator is the whole 85 x 85 x 15 search grid, each
galaxy adding its term to every point; the grid oracle keeps its own
Vector search, to stay independent.
