## handlers-as-dollar - handlers as delimited control, executable (FSCD 2019), and why the library keeps its loop

specs/shift0-dollar.md stage 3.

- TestHandlersAsDollar (5). A deep State handler written as
  `ret $ body` with every operation a `shift0`, and a shallow one
  written with `control0` that re-installs the handler around `k`.
  Both are `Bisim`-equivalent to `State.handle` on the residual Writer
  row, and both run 10 000 operations in constant stack. A mutant Set
  clause is refused, with the first differing tell named.
- Priced in DelimBenchmark (state* lanes): 3.8x and 4.7x the time and
  7.4x and 7.8x the bytes of `State.handle`. VERDICT: nothing in
  Handler.scala adopts it. The encoding is kept as a reference semantics
  and an oracle.
- Also settled: a shallow handler needs no typed control0-to-dollar,
  because its return clause rides inside a plain `push`. Stage 1's
  refusal costs nothing here.
- docs/continuations/11-four-captures.md says this beside the `dollar`
  section.
