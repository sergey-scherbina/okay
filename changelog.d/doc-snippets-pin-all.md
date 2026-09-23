## doc-snippets-pin-all - every doc's Scala examples checked; the old gaps are a shrink-only debt

`TestDocSnippets` pinned six pages; the other 117 with Scala examples
carried 3 656 example lines, and 1 948 of them were in no test at all.

- A RATCHET in `TestDocSnippets` (okay-deploy): every line of every
  ```scala block under docs/ must occur, trimmed, in the repository's
  test, benchmark or LIBRARY sources — library too, because theory
  pages quote it (`// Free.scala:82`), and a quotation must match what
  it quotes. Comment-only lines and `...` are prose and are skipped; a
  code line with a trailing answer comment is checked whole.
- `docs/snippet-debt.txt` records the lines that were unpinned when the
  check began. A line outside it is red (so editing an old example
  means pinning it), and an entry that is pinned now is red too, so the
  file only shrinks. `OKAY_SNIPPET_DEBT=write` rewrites it and can only
  remove lines — tried with an invented line: red, and not laundered.
  Both directions were watched fail. 1 948 -> 1 410 over the lane.
- Paid down, and what paying found:
  - theory ch. 4 quoted `enum Free[F[+_], A]` with `case Pure(a: A)` a
    week after `Free` became covariant with `Return`; the excerpt, the
    `Delay` case and four line references now match Free.scala.
  - continuations-in-practice: every example pinned in two new suites,
    `TestDocExamplesContinuationsInPractice` (okay-direct, 8) and
    `TestDocExamplesDurableProgram` (okay-persist). THREE did not
    compile as printed: `Delim.replay(booking)(j2)` (the inferred row
    starts a second machine — the types are spelled now), the durable
    `booking` had no result type (`no Applicative[F]`), and its drive
    was `.run(Dialogue.asking(oracle))` where the API is
    `.runWorkflow(oracle)`.
  - docs/modules/okay-sql.md, named by the backlog entry, was clean.
- AGENTS.md states the rule under Specs. The remainder (guide,
  direct-style, tutorial, …) is `doc-snippet-debt` in the backlog.
