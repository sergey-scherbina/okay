## lexical-tail-guard-abort - the tail guard counts resumptions, not returns

Found by reading the shift0/$ arc back (2026-09-25). `Lexical.tail`
keeps its state in a cell and refuses a multi-shot capture from OUTSIDE
the installation with `MultiShotAcrossTail`. The guard was a `dollar`
whose `ret` set a flag, so it fired on the second RETURN through the
delimiter. A resumption that leaves the body by `abort` never returns
through `ret`, and had already read the cell the first resumption wrote.

- The red test first: outer `shift` calling `k` twice, `set(get + 1)`
  then `abort(p0)(get)` inside a tail instance. `deep` answers 11; tail
  answered 12 with no exception (TestLexicalTail "ACROSS, leaving by
  abort", red on master, green now).
- `Delim.dollarResumed(p)(ret, resumed)(body)`: a `dollar` the machine
  tells each time it ENTERS the delimiter, with a count fresh per
  capture (`Delim.Shots`). The cut copies a `Ret` frame with a new
  count, `reify` hands it to every program built from that capture, and
  the `Dollar` step bumps it — so a continuation built and dropped is
  not a resumption. `Closing.guarded` and `Lexical.Stacked.tail` are
  `dollarResumed` with `n => if n > 1 then throw`, and the flag and its
  `Free.delay` around `ret` are gone.
- The plain `dollar` carries a null count; `Mark` and the plain cut are
  untouched. Priced in DelimBenchmark: see specs/lexical-instances.md
  Decisions and history.d.
- specs/lexical-instances.md and specs/shift0-dollar.md Decisions;
  docs/continuations/11-four-captures.md says what `ret` cannot see.
