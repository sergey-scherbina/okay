# okay-clojure — Clojure interop, and a Stage IS a transducer

## Overview

Clojure runs on the JVM and publishes a Java API for calling it
(`clojure.java.api.Clojure`: `var(ns, name)` answers an `IFn`, `read`
answers data), so the plumbing is small. The idea worth a module is the
one java-gatherers already proved for the JDK: Hickey's **transducers**
\[Hickey 2014\] are a transformation of a reducing step with early
termination (`reduced`) and a completion arity for the flush — the
push-form enumeratee (theory ch. 7) — and okay's `Stage` is the same
thing as a program. So the bridge is a translation, both ways:

- `Transducers.of(stage)`: an okay `Stage` as a Clojure transducer —
  usable in `into`, `transduce`, `sequence`, `eduction`, core.async
  channels, composable with `comp` against Clojure's own.
- `Transducers.stage(xf)`: any Clojure transducer (`(map inc)`,
  `(partition-all 3)`, `(dedupe)`, a library's own) as a `Stage` in an
  okay pipeline.
- `Clj`: the calling side — `Clj.fn("ns", "name")`, `Clj.require`,
  `Clj.eval(source)` — thin over the Java API, typed at the edges.

JVM only (Clojure is a JVM language); `org.clojure:clojure` 1.12.6,
which runs on JDK 8+, so the module takes the build's default floor.

## The transducer contract, and how a Stage meets it

A transducer `xf` is a function of a reducing function `rf` answering
a new one with three arities: `()` init, `(acc)` completion, `(acc x)`
step. State (a `volatile!`) is made when `xf` is APPLIED to `rf`, once
per transducing process. A step may answer `(reduced acc)` to stop the
process; a process that sees it stops feeding and unwraps it before
calling completion. A stateful transducer flushes in its completion
arity, then calls `(rf acc)`.

For `Transducers.of(stage)` applied to `rf`:
- the process's state is the stage suspended at its next `await` (the
  java-gatherers `Pos`), made fresh at each application to `rf`;
- step `(acc x)` resumes the stage with `Some(x)`; each `tell o` is
  `acc = rf(acc, o)`; if `rf` answers reduced, the stage is abandoned
  and the reduced value returned (the downstream stopped);
- a stage that ANSWERS returns `(ensure-reduced acc)` — the upstream
  stops, like a gatherer's `false`;
- completion `(acc)` resumes the stage with `None` until it answers,
  feeding its tells, then calls `rf(acc)`.

For `Transducers.stage(xf)`: `xf` is applied to a collecting `rf` when
the stage STARTS (a `Free.delay`), steps are `(xrf acc x)`, and a
reduced answer ends the stage after the completion arity has flushed.
Like `Gather.stage`, a pipeline BUILT with `through` over it is a
VALUE (since windows-stage-rerun-loses-pane `through` starts its drive
at run time), and what it refuses by name is a continuation from
inside a run resumed after that run finished: a Clojure transducer's
`volatile!` cannot be snapshotted.

## Behavior

- [x] build: `okayClojure` (JVM) depends on core + okay-stream and
      Clojure 1.12.6; in the root aggregate; module page and index row
- [x] `Clj.fn(ns, name)` resolves a var (requiring the namespace), and
      `Clj.eval(src)` reads and evaluates one form; a missing var or
      namespace is refused BY NAME, not a bare NPE from `invoke`
- [x] law, stage -> Clojure: for id, a 3-chunker, mapAccumulate, a
      transduceUntil take-3, a stage that tells before awaiting —
      `(into [] (Transducers.of s) coll)` equals okay's own run
- [x] composes: `(comp (Transducers.of s) (map inc))` and
      `(comp (map inc) (Transducers.of s))` both equal the okay-side
      composition
- [x] a stage that answers stops an INFINITE `(range)` under `into`
      (bounded in the test so a broken bridge fails, not hangs)
- [x] a downstream `(take 2)` after the stage stops it mid-element: a
      stage telling 1000 copies of one element makes at most 3 tells
- [x] `sequence` (lazy, element-at-a-time through a TransformerIterator)
      and `transduce` with a completing rf agree with `into`
- [x] law, Clojure -> stage: `(map inc)`, `(filter odd?)`,
      `(partition-all 3)`, `(take 3)`, `(dedupe)`, and a `comp` of them
      run through `Transducers.stage` equal `(into [] xf coll)`
- [x] a reduced from a Clojure transducer (take) stops the stage
      awaiting: a 1000-element producer is pulled at most 3 times
- [x] round trip: `Transducers.stage(Transducers.of(s))` equals `s`
- [x] a built pipeline over `Transducers.stage` runs twice, the same
      batches; a continuation from inside a run resumed after it
      finished is refused by name (was: "refuses a second run" —
      the eager drive was `through`'s, fixed there 2026-09-23)
- [x] docs: module page, guide §5 paragraph, theory ch. 7 sentence;
      every snippet verbatim in a gated test

## Stage 2: okay's effects from Clojure, and lazy seqs (clojure-effects-seqs, 2026-09-23)

The operator's direction for Frege holds here too: okay's effects enter
the other language through OUR thin wrapper, as data. `okay.core` is a
Clojure namespace shipped in the okay-clojure jar (a `.clj` resource,
loaded by `require`, no AOT): a program is `(done v)` or `(step op k)`,
`k` a Clojure function from the operation's answer to the rest; `bind`,
`perform`, `await`, `tell`, and `mlet` — a monadic let, cats' shape —
in place of `do`. The okay driver (`okay.clojure.Program`) walks it:
every operation under okay's handlers, every continuation a Clojure
function, so multi-shot handlers call it per branch and no thread is
involved.

- [x] `okay.core` loads from the jar (`(require 'okay.core)`), and
      refers nothing that shadows clojure.core without saying so
      (`await` is excluded from clojure.core in its own ns)
- [x] `Program.stage[I, O](prog)` / `stageWith[I, O, F]`: a Clojure
      program that awaits and tells as an okay `Stage` — law against the
      same stage written in okay
- [x] `Program.run[F, A](prog)`: `perform op` as an operation of F —
      Reader and State in `mlet` order; a Throws from Clojure reaches
      `runEither`; an operation outside the row refused by name
- [x] MULTI-SHOT: Choose × Choose from Clojure gives all four branches
- [x] a hundred thousand Clojure steps on the default stack
- [x] `okay.clojure.Ops`: the core effects' operations for Clojure to
      perform (Reader, State, Throws, Choose, Async sleep)
- [x] seqs: a Clojure lazy seq as okay `Chunks` — an infinite `(range)`
      read partially; okay `Chunks` as a Clojure lazy seq — an infinite
      okay source under `(take 10 …)` produces at most one chunk;
      PURE by type (only `Chunks` becomes a seq)
- [x] docs: module page (effects, seqs), every snippet in a gated test

## Stage 3: core.async channels as okay Channels (clojure-core-async, 2026-09-23)

`CoreAsyncChannel[A]` is an okay `Channel` over a core.async channel:
okay streams, merges and actors read and write it, Clojure code on the
other end sees an ordinary core.async channel (`go` blocks, `alts!`, a
transducer inside it). okay's `Channel` promises more than a queue —
two-phase close, the end after the buffer, acceptance final — so the
view is checked by the SAME `TestChannelLaws` battery every okay channel
answers for, made reusable for it as `ChannelLawsSuite`.

- [x] `CoreAsync.channel[A](capacity)` / `CoreAsync.of[A](chan)`
- [x] the laws: `ChannelLawsSuite` over `CoreAsyncChannel`, DRAIN tier
      included; `TestChannelLaws` unchanged over okay-stream's own
- [x] one `take!` in flight, receivers queued here: a cancelled receive
      loses nothing and nothing overtakes (a stash)
- [x] sends queued here, one `put!` in flight: a queued send can be
      withdrawn; the one in core.async cannot, and is delivered
- [x] close is two-phase: a send queued before close is delivered, a
      send after it refused at once; `finished` only once closed AND empty
- [x] interop for real: a `go` block producing, `into` consuming, an okay
      stage as the channel's own transducer (`(chan 10 xf)`)
- [x] docs: module page section, guide section; examples pinned

## Decisions

- **A transducer, not a Clojure seq, is the seam.** A lazy seq is a
  producer and would bridge to `Chunks` in a line; the transducer is
  the part of Clojure that is a *transformation*, reusable across
  collections, channels and processes — the same reason the JDK side
  bridged `Gatherer` and not `Stream`.
- **The drive loop is Gather's, copied, not shared.** okay-java's
  `Gather.drive` and this `drive` are the same twenty lines over
  `resume`/`split`, differing in how a tell leaves (a `Downstream.push`
  answering a Boolean vs `rf` threading an accumulator that may come
  back reduced). A shared push-driver in okay-stream would put both
  bridges' needs into okay-stream's API and widen its blast radius
  (~97/113 modules) for two call sites. Trigger for sharing: a THIRD
  door — grep `split\[Take % I, Writer % O\]` outside okay-stream.
- **No AOT, no gen-class.** The transducer is a Scala `AFn` subclass;
  Clojure sees an ordinary `IFn`. Nothing is compiled from Clojure
  source, so the module has no Clojure build step.

## Results

Landed 2026-09-23 (okay-clojure). 13 tests in `TestTransducers`, 3 in
`TestDocExamplesClojure`, green on the first compile of the bridge.

**Mutants: four, and the fourth survived the first test.** (A) a stage
that answers not returning `reduced`, (B) a reduced answer from the
downstream `rf` ignored, (C) a reduced step from a Clojure transducer
ignored — each failed exactly its own test. (D) the re-run check at a
STEP removed PASSED: the check in completion still refused the second
run, because `partition-all` tolerates a step after completion. But
the step check is the one that refuses BEFORE the spent state is
touched — the JDK's `windowFixed` NPE'd inside its own array there
(java-gatherers). The test now uses a strict transducer (it throws
"stepped after completion" of its own), and D fails it: the message is
the transducer's, not ours.

**Types at the seam, measured:** `ClassTag[Long]` accepts Clojure's
`java.lang.Long` elements; a String is refused naming
`java.lang.String`.

**Stage 2 (clojure-effects-seqs).** `okay.core` loads from the jar as a
resource and its records are recognised by class once loaded
(`RT.classForName`), not by key lookups. 12 new tests (28 in the module),
green on the first run. Mutants: an eager okay->seq (the infinite-source
test caught it), an eager seq->Chunks (caught by the infinite AND the
counted test — the counted one is bounded, `(range 1000)`, so an eager
bridge fails instead of hanging, the java-gatherers lesson). `Row` and
`Ops` are copies of okay-frege's for now; the shared-driver lane decides
where one copy lives.

**Stage 3 (clojure-core-async).** Written code-first, against the
spec-dev rule — this section was added after the code and says so. The
law battery found the first defect before any reasoning did: a callback
passed to `take!` must be a `clojure.lang.AFunction`, because core.async
attaches metadata to it (an `IObj`); a bare `AFn` failed the first
`take!` with a ClassCastException on a virtual thread nobody joined, and
the law that owned it waited — the gate's watchdog named it STALLED
(1 s of CPU in 8 minutes) and its dump showed the consumer gone. Then 13
laws green, four times running. The go-block test found two defects in
`Clj` itself: `Clj.fn` could not find a namespace made at run time
(`require` looks for a file), and `Clj.eval` evaluated in whatever
`*ns*` was — `clojure.core`, from Java — so a `defn` landed INSIDE
clojure.core; `eval` now binds a namespace (`user` by default). Four
mutants — no stash, a no-op `cancelSend`, a single-phase close, `eval`
without its namespace — each failed exactly its own test.
