## okay-bayes-streams - Bayes over okay's streams and Bulk

okay-bayes stage 6 (specs/okay-bayes.md; operator: "does our bayes work
with our streams and Bulk?" — now it does). okay-bayes depends on
okay-stream. `Online.filter(start, step, particles, seed)`: a bootstrap
particle filter as a value (`push`, `particles`), and `filter.stage`, a
`Stage` from observations to a posterior per observation. `observeBulk` in
`Bayes` and `Smooth`: the log-likelihood over any `Bulk[D]` as one
commutative `Aggregator`, and over `Grad` its gradient too (a small tape
per row, summed), entered on the model's tape as one linear node; `Tape`
takes an initial capacity. TestStreams: the online filter through a Stage
against the Kalman filter at every step (worst 0.078 sds), Bulk and Vector
forms equal in density and gradient, `Bayes.observeBulk` by adaptive.
TestBulkNuts: AD NUTS over 20 000 rows of a Bulk[Chunks] against the
large-n posterior, 17.7 s.
