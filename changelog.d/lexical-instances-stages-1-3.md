## lexical-instances-stages-1-3 - tail, stacked instances, and the default; effect-instances-tunnelling closed

specs/lexical-instances.md stages 1-3. The operator asked for all three
in order.

- `Lexical.tail`: evidence passing (Xie et al., ICFP 2020). An operation
  calls its clause in place, and the handler's state is in a cell made
  per run. Its one unsafe shape is checked, not assumed: a capture from
  outside the installation that resumes its body twice throws
  `MultiShotAcrossTail`, because the installation is a `dollar` whose
  return function runs once per resumption. A multi-shot capture inside
  the body gives the same answer as `deep` (pinned). A mutant guard was
  watched failing.
- `Inst[F, G]` lost its answer type, so the same body runs under every
  strategy and switching is one word.
- `Lexical.Stacked.deep` / `.tail`: the instance IS its delimiter
  (`Delim.Stacked.In` is now an open class), so use outside the
  installation is a compile error.
- `Lexical.handle` picks by clause kind (TailClauses → tail, Clauses →
  deep, ShallowClauses → shallow), and `Lexical.State(s0)` is tail. Every
  strategy stays callable by name.
- Price: tail 1.65x the row's bytes (~2x time, noisy), deep 6.8x. The
  spec expected tail "near row", and that was wrong. Backlog
  lexical-tail-allocs has the +72 B per operation.
- effect-instances-tunnelling is closed and archived. docs/many-instances.md
  describes the strategies and the default.
