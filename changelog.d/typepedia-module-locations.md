## typepedia-module-locations - two sentences the core-modules arc made false

The arc moved five clusters out of `okay` and nothing checks prose, so
the typepedia was audited for claims about WHERE things live. Two were
wrong and both were wrong in the same quiet way - they had been true
when written.

- "`split` is what every walker in THE CORE uses (… the stream
  walkers …)". The stream walkers are in okay-stream now. It reads
  "every walker in this library", and says which walkers are where.
- The optics section was headed "Optics (Optic.scala)", a file path
  with no module in front of it. It is "Optics (okay-optics:
  Optic.scala)".

Everything else survived the audit, including the two "zero
dependencies in the core" lines, which are still true, and
wroclaw-streams-benchmark's "now fixed in the core as
`Aggregator.summary`" - `Aggregator` is one of the interfaces that
stayed.
