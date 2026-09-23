## clojure-core-async - core.async channels as okay Channels, under okay's own channel laws

`CoreAsync.channel[A](capacity)` and `CoreAsync.of[A](chan)` present a
core.async channel as an okay `Channel`. okay streams, merges and actors
read and write it, and Clojure sees an ordinary core.async channel (`go`
blocks, `into`, a transducer inside it, which can be an okay stage).
okay's `Channel` guarantees more than a queue: two-phase close, the end
only after the buffer, and acceptance is final. The view is checked by
the same battery every okay channel answers to. `TestChannelLaws`
became `ChannelLawsSuite(impls)` so it can be reused; okay-stream's own
list and results are unchanged (97 passed). The view passes 13 laws,
including the drain tier, four runs in a row.

It works around core.async's callbacks by keeping one `take!` in flight
with receivers queued locally, plus a stash so a cancelled receive loses
nothing. One `put!` is in flight with sends queued locally, so a queued
send can be withdrawn, and `close!` is called only after those finish.

Defects found on the way, all fixed and each guarded by a test:
- a core.async callback must be an `AFunction` (core.async attaches
  metadata);
- `Clj.fn` could not find a namespace created at run time;
- `Clj.eval` evaluated in `clojure.core` when called from Java, so it
  defined functions inside it. It now binds `user`, or a named namespace.

Four mutants each fail their own test. Docs: module page, guide §3;
specs/clojure.md stage 3.
