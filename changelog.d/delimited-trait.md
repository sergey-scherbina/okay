## delimited-trait - `Delimited[M]`: the continuation machine behind an interface, DPJS's framework in λ$'s variant

The operator's ask (2026-10-01): separate the machine from the
operations, as Dybvig, Peyton Jones and Sabry do with
`MonadDelimitedCont` (JFP 2007). Spec: specs/delimited.md.

- `trait Delimited[M]` (Delimited.scala): `delimiter`, `pure`, `bind`,
  `run`, `dollar` (`ret $ body`, their `pushPrompt`), `shift0` (their
  `withSubCont`, in our variant: `k` keeps the delimiter and its
  `ret`), `resume` (their `pushSubCont`: a COMPUTATION run inside `k`).
  `reset`, `shift`, `abort` are derived once, in the trait.
- `Delimited.machine[F]` is the frame machine: one object for every
  `F`; `resume(k)(m)` is `Bind(m, k)`, the machine's own resumption
  rule. `Cont0` keeps the data; `Delim`, `Lexical`, the `Cont` facade,
  `TestKont` and `KontBenchmark` build through the instance.
- A second instance in the tests, `DelimitedReference` (a list context,
  nothing subtle, not stack-safe): TestDelimited's seven laws are
  written against the trait alone and run on both, which agree.
- New: resuming with a computation — "throwing into a continuation" —
  an abort or a capture to a delimiter only `k` carries, run inside `k`.
- No cost: 0.98-1.01x and identical bytes against master on
  delimPushOnly, delimGenerator, contAnswer, statePara.
- Docs: docs/delimited.md (examples pinned in TestDelimited).
