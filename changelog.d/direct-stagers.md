## direct-stagers - Direct.staged for the rows people write: Stager.All and four singles

`Direct.staged` shipped with one `Stager`, `StateWriter`; a block that
reads a configuration and may fail had nothing to pass. Now
`Stager.All[E, S, W, Err, A]` (core, Staged.scala — the file renamed
from Handled.scala at the operator's ask) stages `Reader % E + State % S
+ Writer % W + Throws % Err` in one layout — the environment an
argument, the state and the log threaded, the error the answer's Left;
a subrow puts `Unit`/`Nothing` in the slots it does not use and the
arms for those members are never chosen. `Reading`, `Stateful`,
`Logging`, `Failing` are the same arms with the tuple removed. `raise`
inside a staged block drops the continuation and answers `Left`; `run`
is the block's catch.
Why one class over the full row and not a composed one: a stager
composed from per-effect arms cannot be written in plain Scala — the
product's `inline def stage` would call an arm through the trait's
ABSTRACT member, which never inlines (direct-staged's own reason for a
method-less `Stager`); fifteen hand-written combinations is the
boilerplate the operator asked to avoid; so the unused slots were
PRICED instead (specs/direct-stagers.md, Results).

Laws (`TestStagers` 9): a four-effect block against the same text as
a Free block under `State.run(Writer.run(runEither(Reader.run(_))))`
on 300 generated cases — state, log, answer or error identical; the
Reader+Throws subrow against `Reader.run` + `runEither`; the
State+Writer subrow against `StateWriter`; a raise mid-loop leaves the
state and log at the raise and runs nothing after (a counter); each
single on its block; the docs example verbatim.

Numbers (okay-direct `StagedBenchmark`, history rows `dst-*`): the
10-op State+Writer block through `All` with two unused slots 7.81 µs
/ 90 184 B against `StateWriter`'s 7.64 / 85 368 — +2% time, +5.6%
bytes, the extra `env =>` currying level per arm (the tuple is the
same); the Reader+Throws block (nine asks, a guarded raise) 4.40 µs /
51 288 B as `Direct.staged(All)` against 11.25 / 112 896 as the Free
block under `Reader.run` + `runEither` — **2.56x** — and 4.35 /
48 888 by hand: parity to 1%.
The first hand-written Reader+Throws parity lane read 2.2x SLOWER
than the macro's output and allocated 75% more — a `def ask` and
braced lambdas with named vals, not the flat shape `handBlock` has;
rewritten in that shape it is the parity target it was meant to be.
Docs: direct-style.md Layer 2½ (which stagers ship, the subrow
spelling, the `raise` rule; the stale "a marked program must be a
leaf" line from before loops v2 corrected), tutorial ch. 11,
typepedia `Handled`/`Stager` entry.
