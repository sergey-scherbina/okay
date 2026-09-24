# Backlog

> **The backlog is [`backlog.d/`](backlog.d/), one file per item**
> (boards-d, 2026-09-18) — a lane edits its own item instead of the
> middle of everyone's file, so two lanes filing work in the same hour
> no longer conflict.
>
> ```
> scripts/board.sh backlog          # read it, assembled, by section
> backlog.d/<section>/<slug>.md     # one item; the slug is the lane's name
> ```
>
> Filing is a new file. Promoting is `git mv backlog.d/<section>/<x>.md
> sprint.d/queue/<x>.md`. Landing is `git rm` plus
> `changelog.d/<slug>.md`. `scripts/board.sh --check` guards the shape
> and `TestBoardEntries` runs that check in the gate.
>
> Closed work is still in `BACKLOG-ARCHIVE.md`, untouched.
>
> This file stays because prose all over the repository says "filed in
> BACKLOG.md", and a pointer is cheaper than editing fifteen specs to
> say something they do not really care about.
Open work only, grouped by the module that owns it. Everything closed —
landed, refuted, declined or answered — moved VERBATIM to
`BACKLOG-ARCHIVE.md` on 2026-09-10 (backlog-cleanup): 314 closed entries
against 51 live ones had made the live list unreadable, which is the
flat-root-file failure the `scrumban` skill names. Nothing was deleted.

Refutations are the expensive part of this repository's memory, so the
verdicts stay HERE as one-liners (below) and the reasoning stays in the
archive. Read them before re-taking anything.

New work goes under its module's heading; a cross-module or unscoped item
goes under "not yet scoped". When a heading grows past a screen, the
skill's next step is that module's own `<module>/BACKLOG.md`.

## bench-native-lanes — a competitor's row should measure THEIR api, not ours

- [x] **The five in-process lanes: DONE** (2026-09-10). The plain JVM,
      `java.util.stream`, fs2, zio-streams and kyo all fold
      `Native.Fold` — panes as keys in a map, nothing evicted, a
      mutable cell, a sort for the top-5 — with no okay type in the
      lane, each carried by the library's own combinators and each at
      1/2/4/8 cores. §20 has the numbers and the reversal they show.
- [x] **Flink and Spark: DONE** (2026-09-10,
      bench-engine-native-arithmetic). Flink accumulates through its
      own `AggregateFunction` over POJO accumulators (`StatsAcc`,
      `TopAcc`), Spark's RDD lane through its own `aggregateByKey`
      over a flat `(Long, Long, Int)` and a sort per window for the
      top-5. Both still answer the same eleven checksums. The finding
      the rewrite produced is worth more than the fairness: our
      accumulator was a HANDICAP — `((Long, Long), Option[Int])` is
      unreadable to Flink's type extractor, so its window state went
      through Kryo, and Spark wrote it across every shuffle. `toFlink`
      and `SparkInterop` stay in their own suites, where "one value
      answers on every engine" is a claim about the interop rather
      than a benchmark row.

## okay core
- [ ] two-accumulating-validators — `okay-codec`'s `Validate.gather`
      is `Validated.app` written by hand on `Either`, and its own
      comment says so: "the applicative step: both sides' errors
      survive". Two answers to one question now live in this
      repository (found 2026-09-18 while checking whether okay-ui's
      applicative traversals work with `Validated` — they do, freely,
      and TestUiApplicative pins it). NOT a rewrite request:
      `Validate` works, is tested, and sits in the schema hot path.
      What is worth doing is a BRIDGE, so a form's errors can be read
      either way, and a line in each file pointing at the other.
      Trigger: the first consumer that wants a schema walk's errors
      inside a `direct` block, or the second time someone asks which
      of the two to use.
- [ ] di-needs-from-static — `okay-di` asks a module's author to
      DECLARE what it needs; `Static.leaves` answers what a program may
      perform before it runs (specs/applicative-static.md), so the
      needs can be derived from the program instead. P13 item 4. The
      shape to settle first: a module's body is not a `Static` today,
      so either the declaration stays and `leaves` CHECKS it (cheap,
      catches drift) or the body is written as a spine (honest, and a
      bigger change). Trigger: the next time a declared need and the
      code disagree.
- [ ] dataflow-ceiling — publish the honest number at which a cluster
      becomes the right answer: throughput and state size past which
      the embedded engine should not be used. Flink never publishes
      one; we can measure ours, and the measurement IS the product
      claim (ROADMAP P13 item 3). Needs: a sweep of key cardinality
      and window count on the Wrocław job until the box is the
      bottleneck, with the bottleneck NAMED (heap, GC, cores).
- [ ] dataflow-flink-migration-path — `okay-flink` already proved an
      `Aggregator` IS an `AggregateFunction` field for field, and that
      the claim survives serialization into a job graph. Turn that
      into a documented road: take a Flink job of this shape, run it
      here, keep the arithmetic. P13 item 3; the engine work is done,
      what is missing is the page and the worked example.
- [ ] once-across-fibres — `Once.run`'s cells are threaded state, so a
      fibre forked inside the program takes a snapshot and two fibres
      demanding one handle run it twice, each in its own store. A
      shared-cell variant (the store in an `Async` cell, a demand that
      WAITS for a running program instead of throwing) is the
      `Deferred`/`memoize` of the async libraries. Trigger: a consumer
      that shares a `!.once` across fibres; none yet (direct-once,
      2026-09-16).
- [ ] dialogue-patch — DONE 2026-09-17 in core (dialogue-asks) and
      through the log (wf-durable-journal, `Dialogue.workflow`). What
      is left is only retirement tooling: something that says which
      programs are still present in a topic, so a branch can be
      deleted with evidence rather than hope.
      Was: stage 2: `patch(id)` (Temporal's `getVersion`),
      so a program that changed can carry its old runs to the end
      instead of stopping them. Stage 0 made the change a loud stop;
      this is how the stop goes away. Needs the `Patched` entry the
      envelope already has room for.
- [ ] dialogue-continue-as — stage 3: `continueAs(seed)` to bound
      history (Temporal's continueAsNew) and a resume cache so a
      process holding many dialogues replays each once. Chapters cut
      the READING; nothing yet cuts the RUNNING.
- [ ] workflow-operations — stage 4, one spec each when picked:
      durable timers, retry policies on `perform`, signals distinct
      from answers, cancellation, a visibility index, a worker pool
      with leases, child workflows. Named so that nobody mistakes the
      model for an engine.
- [ ] gate-warm-warning-blindness — PARTLY ANSWERED 2026-09-17 by
      gate-stall-watchdog: a GREEN whose warning check was blind now
      SAYS SO in the log ("the WARNING CHECK WAS BLIND — this worktree
      was already built"), so silence no longer reads as cleanliness.
      What is still open is making it impossible rather than visible:
      `gate.sh` reporting how many of its module compiles actually
      compiled something, or the gate removing test-classes first.
      The measured incident is in that lane's CHANGELOG entry.
- [ ] delim-region-prompts — NARROWED 2026-09-17 by
      delim-forward-not-throw: the NESTING case is solved (the nested
      forms, the OneMachine guard, and `runNested` for a machine you
      genuinely have), so this is now only about evidence that
      ESCAPES its own `delimited`. Nothing has asked for it.
      `Prompted[R]` proves a delimiter was
      installed, not that the machine running the capture is the one
      holding it, so an outer evidence used inside an inner
      `delimited` is still a runtime `NoPrompt` (pinned in
      TestDelimLimits). `scope`/`collecting`/`pausing` make the
      RIGHT spelling available (delim-nesting), but the wrong one is
      still only caught at run time. Closing it is the region trick
      `runST` uses — a rank-2 scope parameter on the evidence — and it
      would also close the "evidence escapes its delimited" case the
      header already names. Trigger: someone actually hitting it
      twice; the nested form is now the documented road.
- [ ] collect-early-stop — `Delim.collect` has no way to stop: `exit`
      inside the body does not resolve (collect hands out `Emitting`,
      not `Prompted`), and aborting the collect's own prompt would
      drop the cons frames of everything already emitted, so the
      prefix cannot be answered with. The shape that wants it is
      `take n` over a push producer; `Generate`/`Producer` already
      covers it by making the producer lazy, and the practice doc now
      points there. Revisit only if a consumer wants the eager form
      with a stop (continuations-audit, 2026-09-17).
- [ ] handlers-fused-walk — road 1 of specs/continuations-roadmap.md:
      one walker over a whole row with a `Handlers[R]` vector, `O(ops)`
      nodes where N nested handle/relay layers re-emit `O(N · ops)`.
      LANE FIRST: a four-effect lane in HandlerBenchmark run as nested
      handle, nested relay, and the fused walk. Ceiling known before
      starting: handler-fusion measured nesting at 1.1–1.3x, so this is
      allocation plus that, not a third; under 1.1x it is a refutation.
- [ ] direct-staged — road 2: a `direct` block emitted as a `Func`
      program over `Control` when the handlers are static at the call
      site (`Fused.runCtrl[Func, …](s)(direct { … })`), no tree; parity to
      the byte with the hand-written `rightCtrl[Func]` is the goal, the
      tree version (13.7 µs / 122 641 B) the baseline. The macro's
      pipeline gains a second emission target; the lowering is unchanged.
- [ ] freer-base-stage2 — road 3: a `Prog[F, A, S, R]` facade over the
      same tree, transitions sealed by smart constructors, `NoPrompt` a
      compile error first (the 4af08745 probe), then one real module
      protocol. Zero bytes, zero time on every core lane is the number
      owed; the deliverable is the compile error.
- [ ] continuations-as-data-spike — road 4, a SPIKE with a written
      verdict: one effect, one program, defunctionalized `k` measured
      against the closure version on the same lane. Not planned until
      the verdict exists.
- [x] deep-recursive-direct — LANDED 2026-09-15: inside `direct` at the
      program type a self-call is deferred wherever it is marked or
      auto-coloured, so `def fib(n: Int): Long ! Pure = direct: ... fib(n - 1)
      + fib(n - 2)` needs no annotation (with `Direct.given` imported). A
      separate `deepRecursive` was built first and removed at the operator's
      ask. TestDirectDeep; specs/direct-macro.md "Deep recursion". The
      `.?` finding filed beside it was withdrawn: `.?` is retired as a mark
      on purpose (Direct.scala's own comment), `.reflect`/`.!?`/`!p` are the
      spellings. The entry as written: the article's `deepRecursive` (Halotu Kozak,
      "Deep recursion in Scala 3", a macro that rewrites a self-recursive
      body into TailRec's tailcall/flatMap/done) as a `direct` rule. It
      already works BY HAND on master, probed 2026-09-15 with scala-cli
      against the built classes, native stack 512 KB: `def fib(n) = direct:
      if n < 2 then n else !.tailcall(fib(n - 1)).reflect + !.tailcall(fib(n
      - 2)).reflect` (75025 at 25), `sum(1_000_000)` non-tail in 65 ms,
      `isEven(1_000_001)` through `isOdd` — mutual recursion, which the
      article's macro refuses. Her TailRec IS our Free: tailcall = `Delay`,
      flatMap = `Bind(Delay, k)`, done = `Pure`, `.result` = `!.run`. What is
      missing is the zero-annotation form: a rule in Direct.scala that
      colours a call to the ENCLOSING def as `!.tailcall(call).reflect` —
      `asMark`/`opColor` colour by type today, this one colours by the
      enclosing method's symbol. ~30 lines plus a test on fib/sum/isEven;
      the lowering of if/match/blocks/`a + b` is Direct's already.
- [x] delay-node — LANDED 2026-09-15 (tailcallChain 5.3x, handleCapture
      1.35x, controls identical to the byte; specs/core-cleanup.md
      "delay-node"). The entry as written, for the record: a `Delay(thunk)` case beside `Defer`, so that a
      deferred call with NOTHING to do afterwards does not pay for a
      continuation. Found by the Free/Cont/Effects review (2026-09-15,
      specs/core-cleanup.md Decisions). The mechanism is the one
      `hff-defer-cost` priced: `Defer(t, pure)` resumes to `Bind(t(),
      pure)`, and when `t()` is itself a `Bind` the rotation pushes a
      `.flatMap(pure)` tail down every bind of the deferred subprogram
      — one closure and one `Bind` per bind, then a `Bind(Pure(a), g)`
      chain of the same length at the end. `handle` stopped paying it
      for handlers that ANSWER (handle-forward-fast); it still pays on
      the capturing arm (Throws, Choice), and so do `!.tailcall`,
      `Effects.tailcall`, `Eff.flatMap` (every bind of the Church
      encoding is a `Cont.defer`) and the codecs' trampolines past
      `NativeThreshold` (`Cont.defer(...)(Cont.Pure)`). With `Delay`:
      `case Delay(t) => t().resume`, `case Bind(Delay(t), g) =>
      Bind(t(), g).resume` — no composition, no tail.
      WHY NOT JUST DONE: it is a fifth case in `Free.resume` and
      `Cont.step`, the two loops whose bytecode shape has already cost
      1.44x (shrinking `resume` under FreqInlineSize, 409c06e2) and 10%
      (`relay` four bytes over it) — and the `mapnode` row (2026-08-29)
      is a case added to this interpreter that lost 1.22x on fib100
      "despite strictly less work per element". So: (1) a JMH lane
      that HAS the shape — a `tailcall` chain of depth 10 000 and a
      capturing handler over the 10k-op tree (`hff-defer-cost` read
      210 vs 151 µs for it); (2) `Delay`, with `tailcall`, `handle`'s
      capturing arm, `Eff.flatMap`/`defer` and `Cont.defer`-with-Pure
      on it; (3) fib10/50/100/1000, relayPrebuilt, handlePrebuilt,
      statePara as controls, `-f 3`, three rounds. Lands only if the
      controls hold; a refutation with numbers is a fine outcome.
      Typed route, no cast: a `Defer(t, null)` or an identity-compared
      shared `pure` continuation were considered and refused.
- [x] split-over-either — LANDED 2026-09-15 as mostly a refutation: the Either
      was scalar-replaced on every walker but the Source/Pipe road (bytes
      −2..−6% there, identical to the byte everywhere else, time neutral);
      all fifteen converted for one idiom, Resource keeps `<|>` (its arms
      `return`). specs/core-cleanup.md. The entry as written: sixteen walkers outside the three core files
      still split their row with `<|>`, an `Either` per operation:
      Pipe (10 sites), Writer (5), Source, Resource, Logic, Refs,
      Generate, Condition, Channel, Delim. `split` replaced it in
      State and Writer.tell (split-without-either) and in `translate`
      (core-cleanup). Each of these is a `@tailrec` loop, so every
      conversion is an inlining-budget question (`relay`'s 325-byte
      cliff): one walker per lane, `-XX:+PrintInlining` before and
      after, the lane's own benchmark or none. NOTE (defer-eff-removal,
      2026-09-15): `Free.resume` is 323 bytes now and inlines into every
      loop under 325 — a walker converted to `split` may also cross
      that line; `relay` gained 6% from the paste, `handle` lost 15%
      until its cold arms were extracted. Read that entry first.
- [x] freer-base-stage0-verdict — DONE. The verdict the entry asked
      for is "land", and stage 0 landed: nothing more than 2.4% slower
      on any core lane, eight lanes faster, allocation at or below
      master everywhere. What closed it after the entry was written:
      `map` reaching the absorbing path, `relay`'s loop getting back
      under the JIT inlining threshold (landed separately, 25102517),
      and `Once` becoming an enum so the runner's call has one target
      (the operator's proposal — worth 0.955-0.968 on the Fib lanes,
      while making the same site merely bimorphic was worth nothing).
      Four refuted theories and every number are in
      specs/freer-base.md Results; rows `freer0*` and `once-*`.
- [x] free-one-rotation — DONE. `Free.resume` is a member and the one
      rotation; `Free.fold`, `runFree` and Async's loop are three-case
      matches over it; `!.resume` is gone. Only `Cont.step` keeps a
      copy, because it composes through `bind` for absorption. Seven
      lanes faster (effCont24 0.862 at −1016 B/op, effFunc24 0.975 at
      −1064), the `fusedSWr` floor held at 0.985 with B/op identical.
      COST, recorded rather than netted away: relayPrebuilt 1.039,
      relayForward 1.029 — those lanes end in `runWith` over a 9 900-op
      residual, so `runFree` pays a `resume` call per operation, which
      is where it was predicted. Rows `onerot-*`.
- [x] handle-decompose — DONE (2026-09-15). `relay` and
      `Effects.handle` were being compared by two numbers that each
      included 24.6 µs of tree construction. `handlePrebuilt` is the
      missing twin of `relayPrebuilt`: same pre-built 10 000-node tree,
      nothing different but the handler. Like for like the gap is
      **1.51x** (223.3 µs against 148.1), and it is ALLOCATION —
      2 869 306 B/op against 1 753 945, identical to the digit across
      two rounds in opposite lane order, which over 9 900 forwarded
      operations is **+112.7 B per forwarded operation**. At ~12 GB/s
      that megabyte is ~93 µs against a 75 µs time delta, so there is
      nothing else to explain. Rows `hd-*`; the stale 1.45x is
      corrected in docs/benchmarks.md §2 and in `relay`'s own comment.
- [x] handle-forward-fast — DONE (2026-09-15), and BOTH of its
      numbers were wrong in the entry that proposed it. `Effects.handle`
      now re-emits a FORWARDED operation on the tree the way `relay`
      does and enters `Cont` only for one the handler claims.
      RESULT: `handlePrebuilt` 223.3 -> **154.2 us**, and the row that
      matters, allocation 2 869 306 -> **1 753 945 B/op**, which is
      `relay`'s number TO THE DIGIT (the build-on-every-call pair
      agrees too: 2 154 017 on both sides). The 1.51x gap is 1.03x.
      WRONG #1, the entry's mechanism: the `shift` per forwarded
      operation was only a THIRD of the gap.
      WRONG #2, and this is the keeper: the other two thirds were
      introduced by this lane's own first version. Trampolining every
      handled operation through `Free.defer` cost 59 us of the 61 —
      a `Defer` whose continuation is `Pure` rotates into a LEFT-nested
      `Bind`, left-nesting is the one shape `resume` rewrites, so each
      handled operation taxes every operation after it. Found by
      removing the node and measuring (row `hff-defer-cost`): 151.3 us,
      and only the 100k-handled test fails, by StackOverflow. The
      shipped version keeps the node ONLY for a handler that really
      captures; one that does not answers with `Cont.Pure`, and the
      loop goes on from the answer with a tail call
      (`Cont.onAnswer`, inline, no `Option`, no closure).
      The order of work was the entry's own condition and it held:
      TestHandleForward's seven tests were written first, passed
      against the definition, and four of them were watched to FAIL
      against a deliberately wrong forwarding arm before the real one
      was written. Disqualifier checked: `fusedSWr` 13.2 us /
      122 640 B against the recorded floor's 13.7 / 122 641.
      Rows `hff-*`.
- [x] handle-loop-inlining — REFUTED (2026-09-15), and it is the THIRD
      face of a rule this repository has now paid for three times.
      The diagnosis was right and the fix bought nothing.
      DIAGNOSIS, by -XX:+PrintInlining before touching anything, which
      the entry demanded: `Effects.handle`'s loop compiles to 388 bytes
      against `FreqInlineSize` 325 and is refused SIX times ("hot
      method too big"), while `relay`'s loop is 262 bytes and inlined
      hot four times. So the suspicion was exact.
      THE FIX WORKED AND DID NOTHING. Extracting the terminal case and
      the capturing fallback — the same move `relay.last` exists for —
      brought the loop to 318 bytes and flipped the verdicts to
      "inline (hot)" x3. The lane did not move: three rounds after
      (157.3, 153.0, 153.2 µs) against two before (154.2, 156.2), with
      `relay` drifting the same way in the same rounds, and the RATIO
      measured in-run at 1.039 / 1.030 / 1.011 against 1.029 / 1.039.
      The ranges overlap completely. Reverted; rows `hli-*`.
      THE THREE FACES, so nobody re-derives them one at a time:
      over the line COSTS when the body is straight-line (`relay`, 10%,
      25102517); under the line COSTS when the method is a loop
      inlined into other loops (`Free.resume`, up to 1.44x, 409c06e2);
      and here, under the line is simply NEUTRAL — a loop whose caller
      is a 10-byte wrapper gains nothing by being pasted into it. The
      question to ask is not "is it over the line" but "who is the
      caller, and is the body a loop".
      WHAT THE 3% IS, then, by elimination and by reading the two
      loops: allocation is identical to the digit, inlining is ruled
      out, and `handle` does strictly one more test per handled
      operation than `relay` (`Cont.onAnswer`, on top of the `split`
      they share). That test is what buys `handle` the two things
      `relay` cannot do — abort, and perform G. It is a price, not an
      overhead, and this entry closes.
- [ ] runfree-inlined-rotation — the FIRST attempt is REFUTED and the
      refutation is the useful part. Diagnosing with
      -XX:+PrintInlining found the real cost of free-one-rotation:
      `Free.resume` is 352 bytes against FreqInlineSize 325 and
      inlines into NO caller. Shrinking it (the two cold `Defer`
      shapes into their own method, 352 -> 306, confirmed to go from
      "hot method too big" x8 to "inline (hot)" x8) made every lane
      WORSE — effCont24 1.435, fusedSWr 1.063, and relayPrebuilt
      1.065, worse than the 1.039 it was meant to fix. Allocation
      identical throughout. WHY, and do not retry it: `resume` is a
      LOOP, and inlining a loop into callers that are themselves loops
      (`fold`, `runFree`, relay's `loop`) nests loops and costs more
      than the call. The threshold was protecting these lanes. Rows
      `rfinline-*`.
      STILL UNTRIED: the entry's original idea, `runFree` keeping its
      own inlined rotation while `resume` stays as it is — which is a
      third copy rather than a smaller shared one, and after the above
      it must be measured before it is believed. Also worth knowing
      before anyone starts: relay's 3-4% buys four rotation copies
      folded into one, and that trade was already accepted once.
- [ ] freer-base — specs/freer-base.md. STAGE 0 IS LANDED: `Cont` is
      `Freer[Shift, …]`, one enum, one absorption rule, at parity or
      better on every core lane. NEXT IS STAGE 1: `Free` on the same
      base, `A ! F` an alias at a pinned `Unit` index, `object !`
      exporting the cases so the 89 `(x.resume: @unchecked)` sites and
      the 20 files outside the core compile unchanged, and the
      rotation law (already written, `TestFreer`) extended to the Free
      side. It is the bigger half and the one the design is for: the
      rotation still exists FOUR times (`Free.fold`, `runFree`,
      `!.resume`, Async's loop) and stage 1 is what makes it one.
      Three findings from stage 0 apply to it directly and should be
      used rather than rediscovered: a hot loop's BYTECODE SIZE can
      matter more than its data (relay's 305-vs-325 cliff), a `map`
      that misses its carrier's own path costs a node per element, and
      one `apply` body beats two because the JIT counts call targets,
      not receiver types. Stage 2 (the indexes as typestate, Delim
      first) is independent and does not block it.
      SUPERSEDED THE SAME DAY by cont-on-free: `Freer` deleted, `Free`
      kept as the base untouched, `Cont` an opaque facade over
      `Free[Shift, A]` with the indexes on the facade — every core lane
      within 1% of master, allocation identical to the byte, two
      trusted lines under one invariant. Stage 2 (typestate) is now one
      more facade over `Free[F, A]` and cannot leak the way stage 1 did.
      STAGE 2's LANGUAGE QUESTION IS ANSWERED — YES (2026-09-15,
      stage2-probe), asked BEFORE claiming the lane because stage 1
      died on this class of question after its implementation was
      written. A prompt's IDENTITY does reach the type level, so
      `NoPrompt` can become a compile error:
      `scripts/stage2-prompt-identity-probe.scala` runs it — five
      positives compile, three negatives are refused, including a
      prompt that ESCAPES its reset, which is exactly today's throw.
      Four compiler facts were paid for and are in specs/freer-base.md
      so nobody re-buys them: a for-comprehension HEAD has no expected
      type (so the stack must be a given, not an inferred parameter); a
      CURRIED dependent context function is refused outright; a
      non-curried one compiles but CRASHES dotty when it carries the
      stack (`wildApprox failed to remove uninstantiated R`); and a
      `using` clause after the continuation loses to the lambda's own
      typing, so it goes before and the stack is a type MEMBER.
      COST AT THE CALL SITE, measured by writing it: `reset { p => … }`
      becomes `reset { s => import s.given; … }`, one line per reset.
      STILL UNPRICED, and the lane must not assume them: `shift0` and
      `control0` CONSUME the delimiter so their index is unbalanced and
      the probe never exercised it; `abort` still drops a promised
      transition (the spec's own caveat, already a required test); the
      four files outside the core that name Delim (okay-ui `Scope`,
      `Screen`, okay-agent `Stepper`, okay-llm `Cut`) pay the per-reset
      line and none was read; and nothing is measured.
      The refutation below is kept because it is why the facade is the
      shape, not a step toward something else.
      STAGE 1 IS REFUTED AS SPECIFIED (2026-09-15, branch
      `feature/freer-base-stage1`, WIP commit kept and never to be
      merged). `Bind` carries the LEFT side's answer index, so a match
      on a `Free` hands back its continuation at an existential index
      while all 89 sites want `A ! F`. An existential outer index
      fixes elimination and breaks construction; a pinning `unapply`
      is refuted by the compiler, which infers its free parameter as
      `Nothing` instead of skolemizing, so the link between an
      operation's answer type and its continuation is lost. The one
      road that would work is a UNIFORM-INDEX bind case in the base
      (`Seq[G, A, B, R]`), and it costs what stage 1 was for: `Cont`
      still needs the non-uniform `Bind` for `PState`, so the base
      carries both and `resume` rotates both. THAT IS A DECISION, not
      a task — specs/freer-base.md Results has the full reasoning. ONE enum `Freer[G, A, S, R]` — Pure,
      Op, Bind, Defer, one `resume` rotation — under `Cont`
      (`Freer[Shift]`, index = answer type) and `Free` (`Freer[Lift[F]]`
      at a pinned `Unit` index, later typestate). Three stages, each
      its own lane with its own gate and disqualifying numbers in the
      spec: 0 = enum + Cont (absorbs cont-fuse-one-step below: fusion
      becomes the `Shift.Absorbed` class, no budget, 40 B/shift vs 48);
      1 = Free on it, `object !` exports the cases so the 89 `resume:
      @unchecked` sites and 20 outside files compile unchanged, a
      rotation law lets eliminators inline; 2 = the index as typestate,
      Delim first (`NoPrompt` → compile error). Start with stage 0.
- [x] cont-fuse-one-step — DONE, absorbed by freer-base stage 0:
      `Cont.Fuse` no longer exists. Absorption is one step, carried by
      the leaf function's own class (`Shift.Once`), and the depth was
      settled by a sweep of 1/4/16/128 rather than by lowering a
      constant — the Fib lanes are flat across it with identical
      allocation, `statePara` reads 0.861 at depth 1 against
      1.19/1.15/1.17 deeper. Rows `fuse0-*`, `fuse1-*`,
      `freer0b-absorb-sweep`.
- [x] tag-distinct-keys — DONE (2026-09-11) as `Distinct[R]`, and
      the entry's own plan did not survive the first measurement.
      "Collect the singleton keys and refuse duplicates" would refuse
      `Of["k", Ping] + Of["k", Peng]`, which tag-test-the-signature-too
      had made legal that same morning. Comparing ERASURES instead
      would refuse `Writer % String + Writer % Int`, which
      `TestRowIdentity` runs and which routes correctly. Neither the
      key nor the type decides it: the TEST does, so the instance
      declares it — `TypeableK.ByValue`, carried by `writerK` alone,
      with unmarked meaning "tests by erasure". That default is the
      safe direction: an unmarked fine instance is refused and fixed
      by one word, the reverse would pass a row that misroutes.
      The check is therefore not Tag-specific at all. It catches the
      UNTAGGED `Reader % Int + Reader % String` — the defect the whole
      Tag/Refs/Delim machinery exists to work around — which is what
      it was asked for and more.
      WHERE: `Handler.union`, in a SECOND using clause (`(using
      TypeableK, Handler, Handler)(using Distinct[F + G])`), not
      `Row.at`/`plus`. That is where `split` claims the excluded
      middle, it is 15 call sites against 147, and a second clause
      leaves the sites that pass the first one explicitly alone.
      MEASURED, both disqualifiers: zero breakage (whole tree
      compiles, gate GREEN at 4247 results, all 10 okay-agent union
      sites included) and no compile-time cost worth naming (full
      Test/compile 44 s; cold core 70+105 sources 34 s, agent 16+22
      6 s, zero warnings).
      THREE TRAPS, all in `Distinct.scala`'s comments: `TypeRepr.of[R]`
      for a higher-kinded parameter arrives as an HKTypeLambda, never
      as the applied `+` the call site wrote; a `Tag.Of[K, F][A]`
      member is an applied ALIAS, so `baseType(tagSym)` is the only
      thing that sees it; and `F | F` is `F`, so a row CANNOT repeat a
      member — the check only ever fires on two different types with
      one runtime identity.
- [x] tag-test-the-signature-too — DONE (2026-09-11), and the feared
      cost was ZERO. `Tag`'s `Effect` given now asks `TypeableK[F]`
      beside the key, so the test is key AND signature — what
      `Instances` does — and `Of["k", Beep] + Of["k", Buzz]` is an
      ordinary row instead of a collision. The source-compatibility
      worry was measured before the change was kept: the whole build
      and every test compile with zero errors, because an `Effect` IS
      a `TypeableK` and every effect that goes under a key already
      has one. The half no runtime test can reach — same signature,
      same key — still misroutes and is still pinned.
- [x] instances-of-any-effect — DONE (2026-09-11): `Instances[F]` is
      `Tag` with the key read at RUN time — one row member per
      signature, however many instances, identity by a fresh `Handle`
      compared by reference. `at`/`route` to perform and to send an
      already-written program to an instance, `handler(pick)` to run
      them all in one pass, `only(h)` to strip one back to the plain
      signature for the effect's own runner (the others stay in the
      row), `exhausted` to assert none survived. No cast: the handle
      is compared by reference and the operation is already typed.
      `TestInstances` pins instances made IN A LOOP, which is the
      case neither `Tag` nor `Refs` could serve.
- [x] wroclaw-table-refresh — DONE (2026-09-11): §20's table is one
      whole `scripts/wroclaw-bench.sh 8 3 1` at load 1.7-3.1, every
      engine, after okay's lanes changed underneath the old one. It
      also corrected a claim made from mixed runs: okay's one-core row
      is a TIE with `java.util.stream` (563 vs 561), not a win. And it
      measured the table's own noise floor — two rows that are the
      same code since wroclaw-flat-by-default read 9% apart.
- [ ] wroclaw-parallel-ceiling — where okay's merge-parallel lane's
      6.5% serial part actually is, now that the obvious answer is
      REFUTED. The lane scales 1.88x / 3.30x / 5.55x at 2 / 4 / 8
      fibres; Karp-Flatt reads 0.062 / 0.071 / 0.063 — flat, so a
      genuine serial share of ~6.5%, and Amdahl puts the ceiling at
      7.6x on a 14-core box.
      NOT THE PREP PASS (wroclaw-parallel-prep-pass, 2026-09-11).
      `OkayLane.parallel` opens with a serial walk of every event
      (slice maxima + the greatest backwardness), which looked like
      exactly that serial share. Parallelising it as a reduction —
      each slice reduces its own range, the coordinator combines in
      O(lanes) — was measured A/B, both roads alternating INSIDE one
      JVM, five rounds, minimum kept:
        width 2   448 -> 454 ms   (-1.3%)
        width 4   258 -> 257 ms   (+0.4%)
        width 8   154 -> 158 ms   (-2.6%)
      Nothing, and it bounds the scan from above: if 8 threads save
      at most ~4 ms, the whole scan is under 1% of the 855 ms
      single-fibre run. Reverted.
      REMAINING SUSPECTS, unmeasured: the coordinator — `Sink.absorb`
      per slice, `merged` over the partial pane maps, and the
      bunching stitch, all serial and all proportional to the number
      of BOUNDARY panes rather than to the events; and fibre
      spawn/join at width 8. Measure the coordinator's share first by
      timing it separately inside the lane.
- [ ] merge-chunk-size-curve-inverted — CAUSE ISOLATED, A QUARTER OF
      IT FIXED (2026-09-10). okay's chunked merge gets slower as the
      chunk grows where every competitor's gets faster. Measured, one
      stage at a time, quiet box:
        chunk building alone   154 / 197 / 167 us  — FLAT (and its
          allocation flat to +-0.1 B/op, so the first suspect,
          `Stage.chunked`'s per-chunk buffer, is refuted)
        merge alone            245 / 237 / 280 us  — nearly flat
        merge + unchunk        232 / 262 / 430 us  — the rise
      So the k-dependence lives in `unchunked`. It was
      `through(s)(Stage.unchunk)`: a Take/Writer COROUTINE PAIRING
      with every element crossing the handshake. `Writer.expand`
      replaces it — one walk, elements re-told into a plain Free
      chain — and buys 7.3% of the lane's allocation (5 014 435 ->
      4 643 659 B/op at k=16, 4 827 290 -> 4 474 639 at k=1024, both
      exact) and 8% of its time at k=1024 (430.3 +-18 -> 396.4 +-7).
      The premium `unchunked` charges over a bare merge fell from
      ~150 us to ~111 at k=1024 — a quarter of it.
      WHAT REMAINS: the other three quarters, and the curve still
      rises. Refuted along the way, so nobody re-takes them: the
      chunk buffer (flat), the channel's element BUDGET (a lane with
      capacity scaled to hold 1 024 elements at every k rises
      identically: 216 / 297 / 424), and allocation growth (flat at
      every k, before and after).
- [ ] merge-lane-variance — REPRODUCED THREE TIMES, TWO FIXES
      REFUTED, and one trap in the experiment itself recorded.
      `ChunkFlushBenchmark.okayChunked` (`merge(chunked = true)`)
      swings 4x on unchanged code. Per-fork, 5 iterations each,
      2026-09-10/11:
        run A  322 330 460 542 301 237   (min 231, max 700 per iter)
        run B  451 456 984 497 260 391   (min 227, max 1079)
        run C  217 358 351 554 347 406   (min 207, max 828)
      It is not warm-up: the iterations inside a fork often hold
      steady at one level and the FORKS differ, which is a per-JVM
      decision (thread placement on a 10P+4E box is the obvious
      suspect and is not yet tested — pin with `taskset`-equivalent
      or measure with `-jvmArgs` fixing the scheduler's parallelism).
      REFUTED, so nobody re-takes them: (1) replacing the fused
      road's `through(s)(Stage.unchunk)` with `Writer.expand` — run B
      is that build, and it is no better; (2) making the fused flag
      DELEGATE to `a.chunked() merge b.chunked()` when there is no
      flush — run C is that build, also no better.
      THE TRAP, worth more than either: run A read the composed lane
      at a steady 188-199 in the same session and it looked like the
      road was the difference. It may be the ORDER — JMH runs lanes
      sequentially, `okayChunked` was FIRST while the box was still
      settling, and the composed lanes ran minutes later. Any retry
      must alternate the two lanes A/B/A/B in separate invocations on
      a quiet box before concluding anything about the roads.
- [x] aggregator-zip-allocates — DONE (2026-09-10):
      `Aggregator.summary` is the flat count/sum/min/max accumulator,
      beside `Mean` and `Variance`, and it takes the Wrocław job's
      windowed lane from 810 ms to 580 (1.40x) — level with a
      hand-written MUTABLE cell, while keeping the value semantics
      `merge` needs. 83 B per `add` becomes 37.
- [x] topk-stops-sorting-the-corpus — DONE (2026-09-10):
      `Aggregator.topK` consed and re-sorted for EVERY element, and
      `MemoryStore.search` folds a whole corpus through it. Guarded on
      the k-th kept element: 5 358 168 B to select 8 of 10 000 records
      became 30 328 (docs/benchmarks.md §9h), and the store's search
      lane 351 713 B/op and 2542 us became 49 943 and 1036.
- [ ] topk-insert-instead-of-sort — what §9h left on the table. An
      element that DOES make the cut still sorts k+1 through
      `List.sorted` (array copy out, sort, list back), and about
      `k · ln(n/k)` elements make the cut: 57 of 10 000 at k = 8,
      which is the whole 30 KB residual. Inserting into the
      already-sorted list would allocate the prefix and nothing else.
      Small, and only worth doing with the probe in front of you —
      `compare/runMain okay.TopKProbe 10000 8` prints the exact bytes.
- [x] aggregator-zip-flat-general — DONE (2026-09-10):
      `OfLong.zipLong` keeps the specialization through the pair — a
      flat `Longs2` accumulator, both sides stepped by `addLong` — and
      `AggregatorZipBenchmark` prices it in BYTES, which a loaded box
      cannot blur: 90.5 B/element for `count zip sumLong` against 50.8
      for `count zipLong sumLong`, error ±0.4 B/op. A new name rather
      than an overload of `zip`, so no inferred accumulator type
      changes under a caller who did not ask.
- [x] parse-depth-test-asserts-wall-clock — DONE the same day
      (parse-depth-tests-out-of-the-gate, 63db4156): the suite is
      `Live` and runs in `sbt integrationTest`. The minimum-of-three
      repair was tried and MEASURED to fail — 19.6x and 11.5x with
      minima on both sides, and the 50 000-level test hit munit's 30 s
      timeout at three runs, because the two sides of the ratio differ
      20x in duration and a contended scheduler perturbs the long one
      far more often. The gate that proved the fix ran green at load
      average 65, the condition that had produced five reds.
- [ ] aggregator-sum-hides-its-specialization — `sum[N]`'s declared
      return type is `Aggregator[N, N, N]`, so the `OfLong` underneath
      is invisible and `zipLong` cannot be reached from the idiomatic
      spelling (it needs `Aggregator.sumLong`). A match type on the
      return, or an `OfLong`-returning overload for the `Long` case,
      would close it. The same shape as `zip` hiding the
      specialization, one level down.
- [x] windows-one-lookup-per-pane — REFUTED BY MEASUREMENT
      (2026-09-10, windows-int-key-panes). The idea: `Windows` holds a
      one-field `Cell` per pane, so folding an element is ONE hash walk
      plus a field store where it was `getOrElse` then `update`, twice
      per pane per element. Written, gated, and then priced on a quiet
      box by `WindowsBenchmark` (which this lane also lands), 16
      iterations per side, minutes apart:
        tumbling 426.8 +-23.0 us with it, 431.4 +-18.6 without
        sliding  911.5 +-124.2 us with it, 874.5 +-69.8 without
      1% better on one lane, 4% WORSE on the other, both inside the
      bars: the change buys nothing. `LongMap`'s get and update are
      cheap next to the rest of `add`, and the Cell's indirection and
      its allocation per pane pay back whatever the second walk cost.
      The code is reverted; the instrument stays.
- [ ] windows-int-key-panes — the OTHER third of the same gap
      (docs/benchmarks.md §20, "Why one core loses"): `okay.Windows`
      keys panes by `HashMap[K, LongMap[Acc]]`, which boxes an `Int`
      key and hashes twice where the packed benchmark form does one
      `LongMap` lookup — 8.5-11% measured. A `PaneKey[K]` seam with a
      packed store for `Int` keys and today's store for everything
      else would take it, IF the window index and the key both fit in
      a `Long` (they do for any `Int` key and a slide over ~2 ms, and
      the fallback must be chosen at construction rather than
      mid-run). Measure before believing: the same 2x2 is in
      `OkayLane` and prices it on every run.
- [x] wroclaw-remeasure-quiet — DONE (2026-09-10): §20's whole table
      is one run of `scripts/wroclaw-bench.sh 8 3 1` on a box under
      load 5, so the daggers are gone and the rows can be read against
      each other. It also PRICED the arithmetic rewrite: Flink +12%
      and a fifth less allocation, Spark's RDD lane +30%.
- [ ] handler-fusion-flat — GATED OFF by stage 0 (the ceiling for pass
      fusion measured 1.13–1.29x); reopen only with a new number. Was:
      `Handler.flat`: Handler.union assembled
      inline so the nested <|> chain unrolls to one match; measured
      on the four-effect agent row, fourth position is the number.
      (was filed under "handler-fusion" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] handler-fusion-step — GATED OFF by stage 0, same reason. Was:
      `Step[F, Acc]` (tail-resumptive by type)
      and `Fused.run` over `F + G` with the row-shaped product state;
      instances for State, Writer (Fold-generic), Reader incl. local;
      abort/choose fall back to a shift with the state captured
      immutably; laws: agrees with nested for both orders, stack-safe
      at 1M, multi-shot and abort survive.
      (was filed under "handler-fusion" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] growing-order-instrumented-repro — REPRODUCE IT WITH EVIDENCE,
      which is not the same as reproducing it. Four sightings of the
      same shape now exist and a fifth adds nothing: what is missing
      is a break that SAYS WHICH ROAD IT TOOK. So the run is
      instrumented first and long second.
      WHAT TO INSTRUMENT, both named by the retraction in BUGS.md as
      the candidates not yet examined:
        1. the PARKING path — `sendersAt(route)` and the resumed
           `pushDecidingAtOnBehalf`, which by design does NOT repair a
           stale route the way `pushDecidingAt` does (`ours`). Count
           resumes whose parked route is not the part the producer
           would own now.
        2. the STALENESS window around `Growing.inner` — `grow()`
           sets `grown` by CAS, builds the `AdaptiveFifo`, and only
           then assigns `inner`, so a reader seeing `grown == true`
           can still get the ring. Safe for routing (a stale `inner`
           is the ring, and pushing there is correct); not obviously
           safe for everything else.
           CORRECTED 2026-09-18: this said "a plain `var` ... has no
           happens-before edge". `inner` is `@volatile` and has been
           since the file was born (3f3adca1) — there is no
           publication hole, only the window above. The counter is
           shaped by the correction: a test cannot see `grown`, and
           should not be given a back door to it, so the probe counts
           the OBSERVABLE CONSEQUENCE instead — a push landing in
           part 0 after some push had already landed in a part above
           it. Part 0 is the adopted part and is drained first, so
           that is precisely the damage.
      THE PROBE EXISTS: `src/test/scala-jvm/ProbeGrowingOrder.scala`
      (growing-order-probe, 2026-09-18), ignored by default, with a
      `Traced` Buffer decorator injected into the SHIPPED construction
      (`SentinelChannel(Growing(Ring(4), 8, () => Ring(4)))`) so the
      production code carries no instrumentation at all. It prints,
      on a break, the offending producer's every push with the part it
      landed in, the route it asked for, whether it was resumed on
      another thread, and both counters. Its header states what it
      COSTS: one map lookup and one atomic per push, which can mask a
      race this narrow — so a quiet run with tracing on is evidence
      about the traced build, not about the bug. RUN IT under
      `OKAY_PROBE_ROUNDS=40000 OKAY_PROBE_BURNERS=20` — environment, not
      `-D`, because this module forks its tests and the property never
      reached the test JVM.
      RUN, AND THE MECHANISM IS NAMED — 2026-09-18. It is NEITHER
      candidate (BUGS.md, "THE MECHANISM, NAMED"). The parking path
      fired 0 times on every break, nothing went back into the adopted
      part, and no route was stale. A producer's elements are SPLIT
      across the swap — 1/3/5 in part 0, 7 onward in part 1 — and the
      CONSUMER passed part 0 before 5 landed there, drained part 1,
      then came back for 5. "Part 0 is read first" holds per SCAN, not
      globally. Closed as a question; reopened as a design one,
      `growing-order-drain-guarantee` below.
      TWO METHOD FINDINGS worth as much as the answer: the full trace
      MASKS the race (off, the break came at round 1959 of 2000; on,
      6000 rounds found nothing), so the instrument is one byte per
      element written once by its pusher; and the probe counts the
      rounds in which the buffer actually GREW, because "no break in N
      rounds" says nothing until the swap happened (40 000 of 40 000).
- [ ] growing-order-drain-guarantee — WHAT THE PROBE LEFT, with the
      window NAMED (2026-09-18, adopted-window). The first cut of this
      entry said "two candidate fixes, both hot path" and left it
      there; that was a hand-wave, and reading `AdaptiveFifo` replaced
      it with something exact.
      THE WINDOW, NAMED. The fix this needs is already THERE and is
      already a rule rather than a phase: `popManyAdoptedFirst` reads
      the adopted part 0 first whenever it has anything in it, and its
      comment claimed the rule "has no window at all". It has one, and
      the code shows it in three lines:

          if took > 0 then ...
          else if open.get == 1 then 0
          else popManyScanning(max)(sink)   // part 0 was EMPTY

      THE RULE HOLDS PER CALL AND THE CALL IS NOT ATOMIC. Part 0 comes
      up empty, the consumer goes to scan other parts, and a straggler
      whose route was read before the swap lands in part 0 during that
      scan. Its element is then delivered behind its own successors —
      the very shape the rule was written to fix. The comment is
      corrected in place rather than deleted: the rule is the
      improvement it claims to be, only its last clause was false.
      THE CANDIDATE, and why it is not taken here: SEAL part 0 to
      pushes at the moment of adoption. A straggler is then refused
      and reroutes to its own part, landing AFTER its predecessors
      instead of before them, and the refusal path already exists
      (`push` false -> `refused()` -> a part of its own). Two other
      candidates are REFUTED by the trace and should not be re-tried:
      merging `grown` and `inner` into one atomic does NOT fix it (the
      straggler's push into the ring is legitimate until the swap, so
      atomicity changes nothing), and "drain part 0 to empty before
      reading others" does NOT fix it either (the consumer DID see it
      empty — that is the window).
      WHAT IT COSTS is the open question: sealing puts a check on the
      ring's push path, in a class whose header carries four benchmark
      tables. `ChannelGuaranteeBenchmark`, `ManyProducersBenchmark`
      and `ChannelGranularityBenchmark` are the three lanes that would
      price it, and a matched pair is the bar.
      THE THIRD OPTION, and it belongs to the operator rather than to
      an agent: weaken the promise. `Growing` says per-producer FIFO;
      it could say "per-producer FIFO except across the one-shot
      swap", and `TestGrowing`'s law would then assert what is true.
      Cheaper than any fix, and it gives something up.
      WHO STANDS ON THE PROMISE, surveyed 2026-09-18 so the choice is
      made with the consumers in hand rather than in the abstract:
        - `TestChannelLaws` states it as a LAW — "one producer's
          elements arrive in the order it sent them".
        - `Channel.scala` already gave up EXACT FIFO ACROSS producers
          deliberately, and names the escape hatch
          (`Queues.strong[A].fifo(capacity)`); per-producer is the
          half it kept.
        - `Source.merge` RESTATES the promise in its own words to its
          own callers ("each source keeps its own order"), so
          weakening the law means editing that too — and the whole
          streaming stack goes through it. `merge-chunked-order` is
          where the defect showed as `1..16, 49, 50, 17..48`.
        - THE ACTOR MAILBOX IS THE UNGUARDED ONE. A mailbox is a
          `Channel[M]` (default 256) and `Actor.scala` promises
          nothing about order — but "messages from one sender arrive
          in send order" is what every reader of an actor model
          assumes. Weakening the channel law weakens that silently,
          which is the worst shape a weakening can take.
        - NOT AFFECTED: okay-persist. The durable journal writes
          through a JDK `FileChannel`, not this one, so durability is
          not in this decision.
        - The rest (okay-cluster `Remote`, the HTTP transports and
          SSE, `ChatDemo`) are multi-producer but either do not need
          the order or recover it from sequence numbers.
      AND THE SHAPE OF THE DEFECT ARGUES AGAINST WEAKENING: it is ONE
      element displaced across a ONE-SHOT swap, rare enough to need
      thousands of rounds under load. A user cannot reproduce that,
      will not connect it to this, and will look in their own code. A
      promise broken once in thousands, and only while the buffer
      grows, is worse than no promise: people lean on it precisely
      because it is almost always true. `docs/queues.md` already warns
      about the defect with its sightings — weakening would turn "rare
      bug, being fixed" into "by design", which is a different message.
      Reproducer and evidence: `ProbeGrowingOrder`, BUGS.md.
      HOW TO RUN IT: load, not repetition — that is what distinguished
      the green runs from the red ones, and 2 000 rounds on a quiet box
      reproduced nothing. Burners to a load of ~20, the round-based law
      in a loop, every break printed with both counters beside it.
      WHAT WOULD CLOSE IT: a break whose counters are non-zero names
      the road; a break whose counters are both zero refutes both
      candidates and is worth as much.
      RULED OUT ALREADY, so nobody walks it again: a stale route
      surviving into `pushDecidingAt`. Measured 12 crossings of that
      window in 156 578 sends, all in the harmful direction, and
      `Growing.ours` repairs every one (BUGS.md, the retraction).

- [ ] growing-channel-order-under-load — SEEN ONCE, NOT REPRODUCED,
      and recorded because the alternative is forgetting it. A full
      matrix on 2026-09-10 failed `okay.TestGrowing`'s "each
      producer's own order survives the swap"
      (src/test/scala-jvm/TestGrowing.scala:186) at round 34 of 200:
      producer 1's subsequence came back 49, **57**, 51, 55, 59 — its
      own FIFO order broken across a part swap, on the SHIPPED
      `Channel(4)` the test deliberately uses. The box was at load 45
      (an operator VM holding ~11 of 14 cores) and two full matrices
      on the same tree an hour earlier were green, as were 13
      subsequent runs of the suite alone — 2 600 rounds at load 15–29,
      no failure. So it is one of: a real race in the adoption/swap
      path that needs contention to show, or a promise the growing
      channel does not actually make under it. NOT tagged and NOT
      retried: both would hide a real defect, and the suite is the
      only place this guarantee is stated. Reproduce with load, not
      with repetition — that is what distinguished the two runs. The
      gate log is okay-gate.ySVIJbPIg1 (the diff is in it).
      **SEEN A SECOND TIME, 2026-09-11 13:12** (route-headers gate, a
      lane that touches only okay-http and okay-openapi), and the
      SIGNATURE MATCHES, which is what makes this more than a flake:

          round 27: producer 1 came back out of its own order
             5, +13, 7, 11, -13, 15

      Both occurrences are PRODUCER 1, and both are ONE element
      hoisted forward past its own predecessors across a part swap —
      2026-09-10 was 49, **57**, 51, 55, 59 at round 34. Two
      independent trees, two days, the same shape and the same
      producer index. That is the first of the two hypotheses above
      (a real race in the adoption/swap path that needs contention to
      show), not the second.
      Load at the failure: `{ 11.75 20.87 24.93 }` — a box coming down
      off a long busy stretch, which again is contention rather than
      repetition. The run was afterwards SIGTERM-killed at 3089 test
      results; `gate.sh` reported RED rather than KILLED, correctly,
      because a suite that failed and was then killed is red (the
      ordering that branch was given on 2026-09-11 exists for exactly
      this case).
      Still NOT tagged and NOT retried into green by the gate. The
      lane that met it re-ran its matrix after filing this, which is a
      person deciding on an unrelated module — not the script hiding a
      defect. The gate log is okay-gate.dHgXx2UyXr.
      **SEEN AGAIN 2026-09-17 — AND THIS ONE IS AFTER THE FIX, which
      the first framing of this note got wrong.** The two occurrences
      above are 2026-09-10 and 2026-09-11; `growing-stale-route`
      landed 2026-09-14 (3ba1d825) and closed the mechanism they
      share. Calling this one "the third of the same" reads as three
      sightings of one open bug. It is not: it is the FIRST
      RECURRENCE AFTER A FIX, which is a different and worse fact.
      See BUGS.md, where the entry is reopened with the analysis.
      (delim-diagnostics-position, a lane touching only okay-persist
      and prose), round 36:

          round 36: producer 1 came back out of its own order
             29, +37, 31, 35, -37, 39

      PRODUCER 1 again, one element hoisted forward again — and the
      third occurrence is what makes the three MEASURABLE rather than
      merely similar. Each producer emits an arithmetic sequence, so
      the hoist can be counted:

          2026-09-10   49 -> 57   +8   = 4 of its own elements
          2026-09-11    5 -> 13   +8   = 4 of its own elements
          2026-09-17   29 -> 37   +8   = 4 of its own elements

      THE HOISTED ELEMENT IS EXACTLY FOUR OF ITS OWN AHEAD, three
      times out of three — and the test deliberately uses the shipped
      `Channel(4)`. Four is the CAPACITY. That turns "a race in the
      adoption/swap path" into something falsifiable: a producer's
      element one full part ahead becomes readable before the part
      holding its three predecessors is drained. It also gives the
      experiment that was missing — run the same suite at capacity 8
      and see whether the hoist becomes +16. If it does, the bug is
      indexed by capacity and the swap is reading the new part early;
      if it stays +8, the number is a coincidence of three and the
      lead is dead.
      Load at the failure: `{ 8.6 6.6 6.3 }` — NOT a busy box this
      time, which weakens "needs contention to show" as a necessary
      condition, though all three runs were full matrices.
      Still NOT tagged and NOT retried into green by the gate. The
      lane that met it re-gated afterwards, having filed this first —
      a person deciding about an unrelated module, not the script
      hiding a defect. The gate log is okay-gate.UR07J2fMsW.

- [ ] ctx-reader-bridge — `(A ?=> B) <-> B ! Reader % A`, one
      Conversion each way; GATED: no consumer named
      (specs/context-functions.md)

- [ ] logic-named-cut — GATED on a search consumer
      (specs/backtracking.md)

## okay-lex
- [ ] scan-into-the-other-scanners — `Yaml`, `Markdown`, `Xml` and
      okay-rag's `Code` scanner still answer the pair, so they still
      pay the `Tuple2` per character and a `Vector` per token; the
      default `stepInto` keeps them correct, not fast. Json's move
      says the shape of the win (~29% of the allocation on that
      scanner's road), but none of the four has a measured lane, and
      `Code`/`Yaml` recurse into their own `step` — the conversion is
      real work, not a rename. Wants a lane that measures one of them
      first: okay-rag's code chunker over a real file is the honest
      workload.

## Build
- [x] ci-affected — DONE 2026-09-16. The numbers: every Actions run in
      the visible history cancelled by the next push, two at the
      six-hour limit, while the same `sbt test` is two minutes warm on
      the box. `affected <ref|a..b> [task]` and `family <platform>` as
      sbt commands in project/ (Scala, no plugin): a file belongs to a
      project by its source and resource directories, dependents are
      closed over the classpath graph, the root aggregate is the
      bound. ci.yml runs `affected` per push with a `target/` cache
      restored from the nearest previous key, and the family nightly,
      one job per platform. Measured: see CHANGELOG.
- [ ] ci-affected-tests-only — a lane that changed only a module's
      TESTS re-tests its dependents too, which is conservative and
      wrong; the closure should skip dependents when nothing under
      `src/main` moved. Cheap once the numbers say it matters.
- [ ] ci-native-flake — the nightly Native job is where the
      native-runner-error recurrences will now show up; wire
      `scripts/gate.sh`'s rerun-alone logic into it rather than
      re-teaching a workflow the same lesson.
- [ ] jdk-internal-bad-symbolic-reference — a COLD `okayJVM/compile`
      can fail with no source position and one error:

          [error] Bad symbolic reference. A signature
          [error] refers to StackableScope/T in package jdk.internal.vm
          [error] which is not available.

      `StackableScope` is loom's own internal class behind
      `StructuredTaskScope`; nothing of ours names `jdk.internal.vm`,
      and the grep says so. MEASURED 2026-09-11
      (optics-outside-remaining, a docs-only lane, so the tree could
      not be the cause): the gate died at 1205 of ~4081 test results,
      and an UNCHANGED `okayJVM/compile` immediately after recompiled
      the same 69 sources clean in 9 s. The same shape as
      `dotty-classfile-crash-transient` with different text: fails
      cold, passes unchanged.
      Three things before anyone teaches `scripts/gate.sh` to re-run
      on it, which is the obvious next step and the dangerous one:
      the signature must require ZERO `==> X` AND that the only
      `[error]` lines are this one plus `one error found`; the re-run
      must be scoped to the failing project, as the Native branch
      already is; and the rate belongs in a ledger here, because a
      re-run that hides a real compile failure is worse than a red
      gate. Not done: seen once, and once is not a signature.
- [ ] dotty-e198-renamed-import-false-positive — RECURRENCE LEDGER.
      `import okay.Row.{at as liftAt, plus}` warned "unused
      import" while `liftAt` was used as an extension method;
      deleting it failed with E008, which is the proof it was used.
      Dropping the RENAME compiles clean. Recorded rather than
      suppressed, because `scripts/gate.sh` is now red on warnings and
      the next person must not delete the line the compiler points at.
      Occurrences: 2026-09-11, src/test/scala/TestInstances.scala.

## okay-codec

- ANSWERED, and it was not the question it looked like
  (form-errors-on-validate, 2026-09-18). The entry read the difference
  between `Form.errors` and `Validate.errors` as WORDING, and proposed
  a `Wording` parameter on Validate. Running the two side by side on
  one schema (`FormErrorsProbe`) found two places where the form was
  simply WRONG and the accumulating decoder right, and both are fixed
  in the form: a field the SCHEMA DEFAULTS was reported "required", so
  the form held a submit that `Form.decode` accepts; and an error
  inside a present `Option` landed at the option's own key, which is
  not a key the form renders under, so the one thing the user needed
  to read rendered NOWHERE. The wording question is withdrawn — the
  form's "required" is the right word for a person and the switch was
  never worth its cost. `SIso` still hands its refinement whole to the
  decoder, which is correct (the refinement is the wrapper's, not a
  field's) and now says so in the code.
- [ ] schema-typed-paths — `Schema.path[A].field("address").field("city")`
      → a checked `Lens[A, String]`: the field NAME verified at compile
      time against the Mirror, the focus typed. The operator's third
      ask in the schema-fold conversation (2026-09-11); `JsonOptic.path`
      is most of it over `Json`, over `A` it is a macro over the
      Mirror's labels. Filed by specs/schema-fold.md (Out of scope),
      to be taken after stage 3; a different risk (macro) from the fold.
      STALE BY TWO DAYS when written (found 2026-09-18): one level of
      this exists — `Lens.field[S]("name")`, by name, the name checked
      against the Mirror at compile time, the focus typed, no macro —
      since optics-core on 2026-09-09 (c2ff5cfe). What is still asked
      for is the CHAIN with the intermediate type inferred, and its
      cost question is `optics-field-fuse` (the planner cannot read
      the by-name constructor, so it pays the interpreter today).

- [ ] native-runner-error, RECURRENCE LEDGER (the entry itself is
      closed in BACKLOG-ARCHIVE.md — the cause is settled: the test
      binary's connection ends and it exits 0 while sbt still has a
      call in flight, so the module reports no tests and sbt reports a
      lost process). Recorded here only so the rate stays visible, as
      `scripts/gate.sh` asks on every occurrence:
      2026-09-10, okayCodecNative, one lost process in a full gate,
      GREEN on the rerun of that module alone (bench-native-lanes).
      2026-09-10 23:12, okayCrdtNative, same shape, GREEN on the rerun
      of that module alone (optics-outside-routes-query). Second
      module to show it, which is consistent with the settled cause
      being the runner rather than any one suite.
      2026-09-15, okayLexNative, same shape, GREEN on the rerun of that
      module alone (handle-decompose, a benchmark-and-prose lane that
      changed no Native source at all). Third module, and the first
      occurrence on a lane that could not have caused it.
      2026-09-16, okayActorNative, same shape, GREEN on the rerun of
      that module alone (producer-drains, which touched no Actor
      source and no Native platform). Fourth module.
      2026-09-17, okayActorNative AND okayConfNative in ONE gate, same
      shape, GREEN on the rerun of both alone (wf-durable-journal,
      which touched okay-persist and okay core only). First time TWO
      modules lost a process in the same run, which fits the settled
      cause — the runner, under a box that was also carrying a
      1-minute load of 12-14 when the gate started.
      2026-09-17, okayCodecNative AND okayObsNative, again two in one
      run, GREEN on the rerun of both (workflow-docs, a documentation
      lane plus one Worker method). SIXTH occurrence, and the second
      PAIR in a single day of heavy gating — which is the first
      evidence that the rate rises with how many gates run per hour
      rather than with what any lane changed. Worth measuring before
      anyone tries to fix it: a gate every ten minutes is the new
      condition, and it arrived with gate-quiet-realistic.
      2026-09-17, okayPersistNative, ONE module, GREEN on the rerun of
      that module alone (dialogue-continue-as). SEVENTH occurrence,
      and two things in it cut against yesterday's hypothesis rather
      than for it: a single module on the heaviest gating day so far
      (the pair, not the rate, may be the coincidence), and the FIRST
      time the module that lost its process is one the lane actually
      changed — okay-persist. With seven occurrences across six
      modules one such coincidence is unremarkable, and it is recorded
      because the ledger is worth nothing if only the fitting
      observations go in it.
      2026-09-17, okayNative, ONE module, GREEN on the rerun (a book
      lane whose diff is markdown only). NINTH occurrence, load
      `{ 13.16 26.00 28.81 }` — a busy box this time, where the eighth
      was quiet. Across nine there is no pattern in the load and none
      in what the lane changed; the only constant is the full matrix,
      which is what the settled cause already says.
      2026-09-17, okayCodecNative, ONE module, GREEN on the rerun of
      that module alone (worker-oracle-attempt). EIGHTH occurrence,
      load `{ 5.95 10.43 11.04 }`. Second single-module sighting in a
      row on a quiet-ish box, which continues to weaken the
      "gates-per-hour" hypothesis filed on the sixth: two of the last
      three were singles, not pairs, and neither box was busy. The
      only thing that has held across all eight is the settled cause
      itself — the runner, not any suite.
      2026-09-18, okayCodecNative, ONE module, GREEN on the rerun of
      that module alone (script-storefront-look, whose diff is a
      markdown fixture, a stylesheet and a browser test — no Scala on
      any Native path). TENTH occurrence, load `{ 4.12 6.86 8.90 }`, a
      quiet box. Nothing new: a module the lane could not have
      touched, on a box under no pressure, which is the tenth reading
      that the full matrix itself is the condition.
      2026-09-18, okayCodecNative, same shape, GREEN on the rerun of
      that module alone (proc-notation-branches, whose diff is a core
      macro, its tests and prose — no Native source and no codec
      source). ELEVENTH occurrence, hours after the tenth and on the
      same module, again on a lane that could not have caused it.
- [ ] json-strict-is-now-the-slow-door — `Json.readStrict` reads 1104
      ns against `Json.read`'s 1004. The strict door was built to
      avoid the lossless road's cost, and 131cedc2 + b4172242 removed
      that cost. Either make the strict walk cheaper than the CST road
      it was meant to replace, or leave it and keep it for its
      REFUSAL — docs/benchmarks.md §10 already says the latter.
      DISQUALIFYING: if the strict walk's extra 100 ns is the field
      map and `make` (the breakdown says it is ~3.3x the bare parse),
      there is no cheap win and this closes as wontfix.
      (was filed under "bench-known-prices" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] elements-door-cursor — the `.elements` door reads 23.8 against
      the chunk transformers' 10.78 (§5), 2.2x for the per-element
      cursor. Known mechanism, stated in the doc.
      (was filed under "bench-known-prices" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] codec-two-roads-audit — the json-parse-fast-road shape as a
      QUESTION rather than a fix: a module had two roads to the same
      value, 37x apart, and the DEFAULT was the slow one for long
      enough that a separate feature (py-arrow) got filed to work
      around the symptom. Where else does this repository have a fast
      path that nothing takes by default? Named suspects: the CBOR
      pair beside `Json`/`JsonValue`, and the staging seam's
      interpreter-vs-installed choice (`Codecs.current`), which is a
      runtime switch rather than a road but has the same failure mode
      — measured once, then assumed. Cheap to check, and the last
      check of this kind was worth 37x.

## okay-agent / okay-intent — intent, autonomy, dialogue
- [ ] spans-static-tokens — `Static` already holds a vector per token
      unit, which would give `okay.intent.Spans` a JS road with no
      runtime and no model file. The measurement says context is worth
      0.2–0.3 of cosine (specs/intent-spans.md), so this is a
      compromise to MEASURE against the contextual encoder on the same
      317 turns, not a default. Trigger: a consumer that needs the slot
      layer where `okay-onnx` cannot follow.

- [ ] tod-schema-guided-retrieval — Labruna, Bonetta, Magnini (RANLP
      2025, "Task-Oriented Dialogue Systems through Function
      Calling", MultiWOZ 2.3): let the model call a schema-guided
      query that fetches only the needed KB entries, instead of
      putting the whole KB in the prompt; accuracy up, tokens and
      time down, the gap widening as the KB grows. Their BASELINE is
      not ours — we never put a KB in a prompt, and the demo's
      central claim is that nothing reaches the projection except
      through a tool. What IS new for us: deriving the RETRIEVAL tool
      from the store's own `Schema` instead of hand-writing one tool
      per query shape (okay-sql's typed layer, okay-match's
      registry), so a new domain field becomes a queryable slot with
      no new tool code. Pairs with a KB-size sweep — tokens per turn
      and latency, full-KB against schema-guided — in
      docs/benchmarks.md, because at the demo's current KB size the
      effect is invisible by construction.
      (was filed under "Task-oriented dialogue: the literature the operator " — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] tod-single-sequence — SimpleTOD (Hosseini-Asl, McCann, Wu,
      Yavuz, Socher, 2020, arxiv 2005.00796): belief state, actions
      and response as ONE delimited sequence rather than three
      models. Filed LAST, and the reason is the useful part. Its
      result is a fine-tuning result (GPT-2 on MultiWOZ) that we
      cannot reproduce without training; the 2025 paper above argues
      the opposite architecture on ground that suits us better; and a
      single delimited sequence WEAKENS the invariant the demo is
      built on, since a belief cut out of raw text has not gone
      through a tool (recoverable only by decoding it through
      `Schema` before it touches the store — intent-classify's own
      rule). What stays attractive is the SHAPE: SimpleTOD's
      inference suspends after the belief state, queries the KB,
      appends the result and resumes the SAME generation — a
      coroutine that yields exactly once, which is `Stage` over
      `Cont` and something okay expresses better than a framework
      would. Worth building only if the items above leave a reason to.
      (was filed under "Task-oriented dialogue: the literature the operator " — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] tod-multiwoz-harness — OPTIONAL and honestly expensive: a
      loader for MultiWOZ 2.3/2.4 plus the inform/success/joint-goal
      metrics, so our dialogue lane has numbers comparable with the
      outside world instead of only with itself. Keep separate from
      the items above; it is a benchmark harness, not a feature.
      (was filed under "Task-oriented dialogue: the literature the operator " — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-fasttext-subword — subword embeddings TRAINED on the
      corpus plus a linear head, i.e. fastText's actual algorithm in
      plain Scala. Bridges chargrams (language-agnostic, no network,
      60%) and the probe (86.7%, needs a server): a trained
      representation that still ships as an array. Only worth it if
      `intent-embedding-choice` says the server is the problem.
      GATED 2026-09-07, the gate measured: "only worth it if the
      server is the problem" — intent-4b-with-more-data found both
      embedders flat from 32 examples with the same slope, and every
      no-network tier since (chargrams 65, TF-IDF 61.7, the static
      table at 68.3 with triples and PCA) meets the same ceiling from
      a different road: the limit is register and context, not the
      server. A trained subword head would be a fourth road to it.
      Opens if a fixture at least twice this size shows a late slope.

- [ ] intent-grammar-parse — intent by GRAMMAR over `okay-lex` and
      `okay-parse`, the way `Temporal` does dates: deterministic,
      explainable, and refusing rather than guessing. Expensive in
      rules, and the honest reason to want it is a domain where a wrong
      answer is worse than no answer.
      GATED 2026-09-07: no consumer has named a domain where a wrong
      answer is worse than no answer; the model tier at 0.909 macro
      F1 with zero undecodable replies, the slot parsers (when,
      duration, people, amount) already refusing rather than guessing
      where a wrong value would act. Opens with that consumer.

- [ ] intent-crf-slots — sequence labelling for the frame's SLOTS
      (who, when, where) rather than its class. `Temporal` fills one
      slot with a parser; the general case is a tagger, and a CRF is
      the classical one. Only after the class problem is settled.
      GATED 2026-09-07: "only after the class problem is settled" —
      it is settled for the model tier (0.909, deterministic, the
      decoder reading every reply) and the four parsed slots cover
      what the meeting frame asks; the slots still open (who, places)
      are named entities, which is what a tagger is for, and which no
      frame in the fixture yet asks a question about. Opens with a
      frame that does.

- [ ] intent-ensemble-weights — `NoModel` blends the probe with the
      pattern tier using ONE fitted weight from a six-point grid,
      because sixty rows cannot support a fitted second-level model.
      When the corpus grows (see distillation), replace the grid with a
      real stacking model and measure whether it beats the blend.
      GATED, and the gate is now measured (2026-09-07): the corpus did
      not grow honestly — intent-distil-dose found the distilled rows'
      gain to be one split's, intent-distil-static found them worth
      nothing to the static table, intent-distil-diversity found them
      a third as diverse as the fixture — so a second-level model
      trained on them would learn the generator's register. Opens when
      the human fixture passes ~200 rows (the review queue is the
      source); the six-point grid stays until then.

- [ ] intent-language-fixture-growth — SHARPER NOW (2026-09-05): the
      fixture is eight languages wide (uk and pl added) and every
      non-English language has at least one class at F1 0.00 when
      fitted on fifteen rows of it. The construction is
      language-agnostic; the DATA is what is missing. Original entry:
      the per-language arms train on
      FIFTEEN examples each, where the learning curve put the probe's
      stabilisation at about thirty-two, and the numbers swing from
      46.7% to 86.7% accordingly. No per-language claim about
      embedders or classifiers is defensible until the parallel set has
      at least 30 messages per language, which means growing it from 30
      meanings to 120. That is a translation job, and the
      author-written-translation limitation grows with it.
      (was filed under "the original entry" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-annotate-log — the model reads the LOGS and proposes
      labels for REAL messages (operator's direction; specs/intent-classify.md,
      "The harvest programme"). Not the distillation that failed: the
      model writes nothing, the messages are real traffic, only the
      label is proposed — the practice the literature supports
      (arxiv 2406.17633, 2503.17336). A row is kept only if the
      reading grounds in the message, conf >= Medium, k samples agree,
      and no deterministic tier contradicts at high margin; provenance
      per row (model, prompt fingerprint, date, filters passed).
      Criterion: 100+ kept rows, and a refit on them moves the
      autonomy rate without breaking the per-class law.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-coannotate-queue — CoAnnotating (arxiv 2310.15638):
      route by UNCERTAINTY, so a person arbitrates only what the
      filters could not settle, ranked by tier disagreement. Our
      active-learning lane already measured the shape (28 labels
      against 36 random for the same gain). Criterion: human effort
      per point of autonomy, not accuracy alone.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-discover-classes — Other is several real classes nobody
      named; cluster what lands there and name the clusters with the
      model (Dial-In LLM, ACL 2025.emnlp-main.300, >95% agreement with
      human judgement on 100k real calls; NILC, arxiv 2511.05913,
      WSDM 2026), then a person accepts or rejects each proposal.
      Criterion: Other's recall after the split, and how many
      proposals survive review.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-noise-aware-refit — if harvested labels prove noisy
      enough to bind, the noise-aware refinement the literature
      reports (arxiv 2505.19675, ~7% recovered). GATED on a
      measurement showing noise is the limit.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-label-queue — which rows to ask a person about: small
      margin plus disagreement between tiers (the shape the
      active-learning lane measured at 28 labels against 36 for the
      same gain). Ships as a queue the admin flow can drain.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-induce-on-harvest — re-run cue induction whenever the
      corpus grows and ship the induced cues beside the hand-written
      ones. Cues need nothing at run time, so their coverage is pure
      autonomy: 85.7% precision at 11.7% coverage on 60 rows today,
      against hand-written 90.6% at 53.3%.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-per-language-models — one artifact per language once
      rows exist; the shipped one is English-only and scores chance
      (23-30%) elsewhere. GATED on intent-language-fixture-growth.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-other-more-rows — SHARPENED 2026-09-07 by
      intent-offline-other, which turned this from "more data would
      presumably help" into a measured blocker: an out-of-domain
      detector over the same rows already RANKS at AUC 0.843, and
      every decision rule built on it is starved by 15 training rows
      (by argmax it cannot fire at all; balanced it destroys the
      tier). 40-60 real out-of-domain English rows — the operator's,
      or harvested from the service's own traffic into
      okay-chat/corpus/harvested.json — and TestOfflineGate re-runs
      unchanged to settle it. Still needs human rows: the distillation
      lanes measured generated ones to be worth nothing (they carry
      the generator's register).
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-second-author — PARTLY ANSWERED 2026-09-04 by measuring
      the gap instead of the corpus: 66.7% on the least-familiar half
      against 86.7% on the most, 65-67% under mechanical register
      shifts, and every shipped quote corrected to 65-70% for a
      message somebody else wrote. What remains is the part no
      measurement replaces — a corpus this repository did not write.
      Original entry follows.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-second-author (original) — the provenance problem the review could
      not fix: the rows are still one hand's Russian, rewritten by the
      same hand that wrote them. A gap measured against my own language
      is a joint measurement of the model and me. The consumer offered
      REVIEW, which is what was available and is now spent; what is
      missing is a second AUTHOR, for Russian and for whatever
      languages the fixture keeps.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] intent-extract-more-slots — people DONE 2026-09-07
      (intent-extract-people): `People.parse`/`find` — a count beside a
      people-word (`four people`, `six of us`, `a team of 5`, `vier
      Personen`, `dla czterech osób`, `4人用`), the Slavic collective
      numerals counting by themselves (`на четверых`, `на чотирьох`),
      1..1000, `None` otherwise; `Slots.people`; the fixture's
      book-room row counts four in all eight languages; number words
      shared with `Duration` through `Numbers`. Left here: named
      entities (who) and places — neither is a parser. Durations DONE
      2026-09-07
      (intent-extract-duration): `Duration.parse`/`find` (minutes; a
      number and a unit, `1h30`, `90m`, the spoken fractions, number
      words, `and a half`; total and deterministic like `Temporal`)
      and `Slots.duration`, asked in `when`'s six languages, showing
      `1h30`/`2h`/`45min` back. English phrases; the other languages'
      number-and-unit words are filed as
      intent-duration-multilingual. Amounts DONE 2026-09-07
      (intent-extract-amount): `Amount.parse`/`find` — a number
      beside a symbol, a code or an unambiguous currency name in
      the eight languages, separators told apart by the digits that
      follow, composed number words, the nearest number wins;
      `Amount(value, currency)` with an ISO code; `Slots.amount`.
      Still open here: named entities (who) and places — neither a
      parser. Original: only
      `when` and whole-message text have extractors. Named entities
      (who), durations, places and amounts are the obvious next ones,
      and each is a `Slot.extract` rather than a design.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## okay-rag
- [ ] named-pairs-rest — the SURVEY behind named-pairs-security, kept
      whole because the operator asked for everything found. Seven
      functions in main sources return a SAME-TYPED pair, so a
      positional swap compiles and no test can catch it by type. Two
      landed (`OAuth2.pkce` -> `(verifier, challenge)`,
      `ApiKey.issue` -> `(key, digest)`), where a swap is a security
      defect. These five remain, each with what a swap would do:

      | where | returns | a swap gives you |
      |---|---|---|
      | `Secrets.scheme` (okay-conf) | `(String, String)` | the scheme and the rest of the reference exchanged, so a `vault:` ref reads as a literal |
      | `KafkaStore.range` (okay-kafka) | `(Long, Long)` | begin and end offsets exchanged, an empty or backwards range |
      | `Smtp.stamp` (okay-mail) | `(String, String)` | the Date header holding a Message-ID and the reverse |
      | ~~`Site.splitUrl` (okay-script)~~ | DONE 2026-09-12 | it was PUBLIC, see the correction below |
      | `FileStore.readHeader` (okay-persist) | a pair read off a buffer | (unread — check before taking) |

      "All are private or module-local, so the blast radius is small
      and so is the value" — THAT SENTENCE WAS WRONG, and it is left
      standing rather than edited away because it is the reason the
      row above was not taken with the security two. `Site.splitUrl`
      carries no modifier at all: it is public API, its caller may not
      be in this repository, and a swap hands them the query string as
      the path. Corrected and done by `split-url-named` (2026-09-12)
      when the operator asked "so nothing needs doing?" and the
      question sent me back to check the claim instead of repeating
      it. The remaining FOUR really are private or module-local
      (`private[conf]`, `private[KafkaStore]`, `private`, `private`),
      checked one at a time this time. Naming them costs nothing at runtime — a named
      tuple IS the plain tuple, measured in named-tuples-stage0 — and
      the call sites need no change, since a positional destructure
      still works. Take them when touching those files for another
      reason rather than as a sweep.

      METHOD NOTE, which is the reusable part: `val (a, b) = named()`
      still binds BY POSITION, so naming a return does not by itself
      stop a caller swapping. The protection is at the call site,
      through `p.verifier` or a named destructure
      (`val (challenge = c, verifier = v) = pkce()`, which binds by
      name whatever order it is written in). named-pairs-security
      moved its call sites for exactly that reason.

- [ ] wroclaw-pipeline-named — `Gtfs.departures` in okay-spark's
      TestWroclawAlgebra still writes the shape of every join step in
      a trailing comment (`// trip -> (time, (route, service))`),
      because its tuples cannot. `GtfsNamed.departures` beside it is
      the same pipeline with the payloads named and those comments
      deleted, proven equal on the real feed (4 593 288 departures,
      identical summary) — it exists as the measurement from
      named-tuples-stage0 and as the regression test for
      named-tuple-unblock. Merging the two into one named pipeline is
      the obvious follow-on and was deliberately NOT done in either
      lane: both had a rule that nothing migrates. Whoever takes it
      keeps the equality test by comparing against a recorded summary
      rather than against a twin that no longer exists.

- [ ] twonode-fixed-ports — `okay.demo.TestTwoNode` spawns two REAL
      JVMs on HARDCODED ports 18091/18092 and is not `Live`-tagged, so
      two agents gating at once collide on them. Seen 2026-09-11 in
      generalized-method-syntax's first matrix: the test failed in
      **0.337 s** with `java.io.IOException: HTTP/1.1 header parser
      received no bytes`, and passed alone a minute later in 3.763 s.
      The timing is the evidence, not a guess: two JVMs cannot boot in
      300 ms, and the suite's own `whoami` swallows every exception
      and waits up to 15 s for both to answer — so for the run to get
      PAST readiness and then die on a later request, something was
      already listening on those ports, and it was not this run's
      children. A sibling's matrix running the same suite is the only
      candidate left; nothing else in the repository uses 1809x.
      nio-port-scope's survey could not have caught this one: it greps
      the test tree for `new ServerSocket`/`serve(0)`/`listen(0)`, and
      here the port is bound by a CHILD process, passed in through
      `OKAY_CHAT_PORT`. Two fixes, and the choice is the point: an
      ephemeral port pair (the test would have to read the port back
      from the child, which is real work), or the `Live` tag that its
      two neighbours in okay-demo already carry (TestChatDemo,
      TestRepoAgent) — which keeps it out of `sbt test` and costs the
      default gate a real distributed-failover test. Price both before
      taking either.
- [ ] vector-search-dominates — `searchVectors` 379 us dominates §11's
      per-query table, where everything else is under 20. Not a
      defect (240 segments x 1536 dims is real work), filed because it
      is where retrieval's time actually goes.
      (was filed under "bench-known-prices" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] rag-langchain4j — their EmbeddingStore as a Retrieve handler
      (the other half of the interop sentence; when a consumer
      names a store)
      PARTIAL 2026-09-02: the EmbeddingModel half landed as
      okay-langchain4j-embed (a local ONNX embedder, String =>
      Embedding + Handler[Embed] — MemoryMatch's exact `embed` seam,
      no okay-rag pipeline needed). Deliberately OUT of okay-demo's
      build and the root aggregate (a real ~90MB model download).
      The EmbeddingStore/VectorStore half named in the title is
      still open — this box stays unchecked for that.

## okay-r
- [ ] r-rserve — stage 1: the served engine (Java client behind a
      trait; own QAP1 over Async later if named); two-engine
      acceptance

- [ ] r-arrow — frames as Arrow files/streams once the JSON-frame
      road hurts. MEASURED 2026-09-09 (r-measure-harden,
      `MeasureRFrame`, medians of five against the dockerized R 4.4.1,
      `identity` on a 3-column frame):

      | rows | payload | our encode | round trip | our decode | typed rows | OUR share |
      |---|---|---|---|---|---|---|
      | 10 000 | 0.30 MB | 6.5 ms | 1 546 ms | 7.3 ms | 2.2 ms | 0.9% |
      | 100 000 | 3.21 MB | 20.4 ms | 13 686 ms | 18.3 ms | 6.0 ms | 0.3% |

      The number says the opposite of the Python twin's. There, 60% of
      the trip was OUR parser; here our two halves are 0.3% and the
      other 99.7% is R. Nor is it the pipe: 3.21 MB in 13.7 s is
      ~230 KB/s, and a pipe does that in milliseconds — we encode and
      decode the same bytes at ~83 MB/s. So the cost is jsonlite
      walking the STRUCTURE we hand it, and the structure is the
      suspect below. Arrow would still remove it, at the price of the
      `arrow` package (native, heavy) on R's side and an Arrow reader
      on ours — a big dependency for a module whose only dependency
      today is jsonlite. Try the cheap shape change first.

- [ ] r-restarts — GATED twice: on r-subprocess and on a restart
      consumer; the one resumable-capture case (specs/r.md)

## optics-outside — optics as an API, not an implementation (specs/optics-outside.md)

The arc the operator opened 2026-09-10, after `specs/optics.md` closed.
Optics INSIDE okay are done; this is optics, profunctors, arrows and
categories as the vocabulary a USER of the library writes. The spec's
criterion decides what belongs here and what is decoration: a
declaration earns an optic only when it must be given to more than one
interpreter and at least one of them DESCRIBES it instead of running
it — which a plain `S => A` cannot do. Read the spec's Overview before
taking any of these; it also records where the run-time tax lands (on
the USER's data path now, so a per-element optic ships behind `Fuse`
or not at all).

- [x] optics-outside-routes — DONE (2026-09-10, dcc1d76e). A route is a prism. One declaration
      answers three questions: does this path match (server), what is
      the path for these parameters (client, reverse routing), and
      what does it look like (OpenAPI, an MCP tool). The prism law
      `unapply(url(a)) == Some(a)` is the feature, not a nicety.
      Motivated by what is in the tree now: every `routes` in the
      repository is `case r if r.method == Get && r.url == "/healthz"`
      (okay-ops, okay-admin, okay-acme, okay-demo) — no route has a
      typed parameter at all, and the one router that does have
      parameters (okay-script's `Site.resolve`) hands them back as an
      untyped `Map[String, String]` and splits on `/` before decoding,
      so an encoded `%2F` inside a parameter becomes a segment
      boundary. Stage 1 of the spec. Landed WITH a caller:
      `Acceptance.routes` became a `Router`, and the conversion found
      two live defects — `startsWith("/person")` answered `/personal`,
      and every route answered every verb.
- [x] optics-outside-routes-adopt — DONE (2026-09-11). Every
      hand-written router in the tree is a `Router` now: okay-ops and
      okay-script (stage 4), okay-http's acceptance fixture (stage 1),
      okay-demo (stage 8's lane), okay-chat, and finally okay-admin's
      `/admin/replay` and okay-demo's `/whoami`. Each conversion found
      something; the list is in the CHANGELOG entries.

      TWO WERE LEFT ALONE ON PURPOSE, so nobody converts them later by
      matching on shape. okay-acme is already correct — it has its own
      `path(url)` cutting the query before it compares, the only module
      that did. okay-security's `McpAuth` matches
      `startsWith("/.well-known/oauth-protected-resource")`, which
      looks like the `/person` → `/personal` sloppiness and is not:
      RFC 9728 allows the resource's path as a suffix, so the prefix
      is probably deliberate and converting it would break spec
      compliance.
- [x] optics-outside-routes-query — DONE (2026-09-10, 6468e90a).
      Stage 3 of the spec: the query string, as its own `Query` type
      composed with `&` and handed over by `?`. The finding was a rule
      attached to the wrong layer — `Param.string` refused the empty
      string for the PATH's sake and broke `?tag=` the day the query
      arrived. `Route.Concat` also became `Route.Split` in the same
      lane, after the operator read the old name as the standard
      library's.
- [x] optics-outside-ops-routes — DONE (2026-09-11, cbb1000f). Stage 4:
      the DESCRIBE interpreter's first consumer. A probe path lived in
      three independent literals (okay-ops served one, okay-deploy's
      `Health` defaulted to another, okay-script's ScriptDeploy wrote a
      third); the paths are values now and two tests hold the ends
      together. Found on the way: `Ops` compared the whole url while
      `Site` compared only the path, so `/healthz?probe=1` worked in
      one module and missed in the other. Also fixed `TestSignals`,
      which used a spin budget as a timeout and failed 2 of 3 runs on
      untouched master.
## optics-arrows-effects — the questions the closed arc leaves (specs/optics.md stage 12, operator's ask 2026-09-18)

The operator asked what optics, profunctors and arrows can do together
with the monads, applicatives, effects and continuations already here,
and what is missing for that to be convenient. Stage 12 of the spec
records what the tree ALREADY answers (one traversal runs at
`Validated`, `Par`, `Static` and an effect row with no code in the
optics; `PState.zoom` is the lens-meets-continuation seam; `Arrow` has
one instance, `Mealy`), and leaves these. Each is one lane with one
test; none is promoted until its spec item is read first.

- [x] gate-bound-test-fanout — DONE 2026-09-18, one line and a
      measurement: `ThisBuild / Test / parallelExecution := false`.
      MEASURED rather than guessed: `show concurrentRestrictions` says
      `Limit all to 14` (TASKS) and `Limit forked-test-group to 1`,
      but `Test / parallelExecution` was TRUE on all three platforms —
      and on JS/Native a test CLASS is an OS PROCESS. One task fans
      out into all of its classes; 14 x ~10 is the 145-165 both dumps
      caught.
      THE PROOF IS THE CONDITION, not the clock: the same `affected
      master` scope that STALLED TWICE (module 77, then 78) ran to
      completion — 92 modules, 303 s — while a sibling's unbounded
      gate held 152 node processes at load 76. Peak children 104.
      NOT CLAIMED: 104 is not the ~14 one-task-per-core predicts, and
      the remainder is unmeasured (`gate-fanout-what-is-left`); and
      303 s is not comparable to the 179 s baseline, which was a quiet
      box. Only failure: `hedge-start-timing-flake`, 4th sighting,
      3 of 3 green alone.
- [x] gate-fanout-what-is-left — ANSWERED 2026-09-18, and the answer
      is `node`. Sampled every 4 s over one full `affected master`
      gate, counting ONLY the descendants of that gate's own sbt (the
      first attempt matched `^node$` box-wide and reported a sibling's
      build):
        node (Scala.js runners)   684 of 944 samples — 72%
        Native test binaries      ~170 together
        java (forked JVM tests)   24
      So after the per-module bound, the weight of a full matrix is
      almost entirely the Scala.js runners. WHAT FOLLOWS, and it is
      the cheap one: a core change makes every module affected on
      THREE platforms, and the JVM run is the one that tests the
      logic — JS and Native mostly re-check that the same suites
      compile and run cross-platform. A gate that ran JVM FIRST and
      only paid for the other two once it was green would fail fast
      on the common case (a logic error) and cost a third as much to
      find it. Filed as `gate-jvm-first`; it needs a measurement of
      how often a lane's first failure is JVM-only before it lands.
- [ ] gate-jvm-first — order the gate by platform, JVM first, and pay
      for JS/Native only after it is green. The measurement that
      decides it: over the gate logs already kept, how many red gates
      failed FIRST on the JVM? If it is most of them, this turns the
      common failure from a full matrix into a third of one. If lanes
      mostly fail on Native (the lost-process shape has its own
      ledger), it buys nothing and should not land. `gate-fanout-what-
      is-left` is where the 72% comes from.
- [ ] gate-fanout-what-is-left (superseded, kept for the method) — the bound took the peak from 165 to
      104, not to the ~14 one task per core would predict. What the
      rest IS, is unmeasured: link steps, the gtk test, whatever else
      spawns beside the runners. Sample the tree BY COMMAND during one
      quiet full gate and name what makes up the 104. Key the sampler
      on the descendants of ONE pid — the first cut matched `^node$`
      and reported a peak that was mostly a sibling's build.
- [ ] gate-bound-test-fanout (superseded; kept for its measurement) — THE OTHER HALF OF THE 57-MINUTE STALL
      (gate-watchdog, 2026-09-18). The watchdog now catches a hung
      gate; nothing yet stops it happening. MEASURED at the stall: 165
      test-runner processes alive at once — 100 `node` (Scala.js) and
      65 Scala Native binaries — all spawned inside the first 20
      seconds, on a 14-core box, with `Tags`/`concurrentRestrictions`
      set NOWHERE in build.sbt or project/. sbt's own default limits
      TASKS, not the processes a single test task spawns, which is why
      the fan-out is what it is. WHAT TO DO, in order: (1) `show
      concurrentRestrictions` and record what the default actually is
      here — do not guess it; (2) count the fan-out per platform on a
      quiet box; (3) bound it with `Tags.limit`, and measure the wall
      clock before and after, because the whole point of the fan-out
      is speed. TRIGGER TO DO IT NOW: a second stall whose dump shows
      the same shape. The dump that opened this is the one named in
      the CHANGELOG entry.
- [x] optics-typepedia — DONE 2026-09-18. The reference nobody could
      grep: `docs/typepedia.md` is 911 lines of "every core type and
      typeclass" and contained neither `Lens` nor `Optic`. It has an
      Optics section now — the constraint lattice, the seven families,
      the five interpretations, the two roads through `Fuse`, the two
      field constructors, and the gotchas that were only in source
      comments (`.compiled` is slower and why; a traversal cannot be
      compiled at all; `Aggregating` is deliberately not `Strong`;
      `idApplicative`/`zipLazy` are not givens; a bottom-up rewrite is
      a catamorphism, not a traversal). Also: docs/optics.md gained
      the program-zooming section, the tutorial's "where to go next"
      links the page, and both theory indexes carry the sharper
      chapter-10 result instead of the old one-line summary.
- [x] optic-law-rewrites — THE CONTAINER HALF IS DONE, 2026-09-18,
      2.7x collected and measured (`fuseTwiceByLaw` 1524 ns against
      `traversalTwice` 4063, within 7% of the hand-written control at
      1421; 3 forks, 24 measurements a lane). `Fuse` rewrites
      `modify(g) . modify(f)` through one optic into
      `modify(f andThen g)`.
      THE SHAPE CAME FROM A PROBE: the outer macro does NOT see a
      nested macro call, it sees the interpretation the inner already
      emitted (`o.apply[Function1](g)(fn).apply(s0)`), because the
      planner cannot read a traversal. Three theories died first, and
      one probe LIED — `report.info` did not print for the outer
      expansion while a file written from the same macro showed it
      running.
      WHERE IT CANNOT REACH, so nobody re-tries: the extension form.
      `o.modify(f)(s)` is `(o.modify(f))(s)` — the application is
      outside the macro, and `traversalTwice` stays at 4063.
      STILL OPEN, the product half: `set . set` into one `copy` is
      ~2.5 ns and NOTHING in bytes on the JVM (escape analysis already
      scalar-replaces the intermediate). Native and JS have no such
      analysis and were never measured, which is the only reason left
      to take it. The entry as originally filed:
- [ ] optic-law-rewrites (the product half) — CITED AS FILED IN TWO PLACES AND FILED IN
      NEITHER (found 2026-09-18 by optics-guide-page, which wanted to
      link it): docs/benchmarks.md §9b ends "Filed as
      `optic-law-rewrites` with these numbers attached" and the
      CHANGELOG entry says the same, and no board ever got the entry.
      The work: teach `Fuse` the two rewrites its own measurement
      priced. `map(f) . map(g) == map(f . g)` on a container is the
      big one — one pass is 1570 ns / 19 672 B against two at 4006 /
      38 320, and the JIT cannot do it because it does not know the
      law. `set ∘ set` into one `copy` on a product is the small one:
      1.48 ns against 3.94 for two fused sets, with allocation equal
      at 24 B because escape analysis already scalar-replaces the
      intermediate — so the prize there is ~2.5 ns of field traffic
      and nothing in bytes, ON THE JVM. Native and JS have no escape
      analysis of that quality, so the allocation half is unmeasured
      there and a lane should measure before claiming it.
- [x] optics-guide-page — DONE 2026-09-18. `docs/optics.md` with five
      pairs, each RUN by `TestOpticsGuide` (src/test/scala-cross), the
      price table from the verdict, the two roads named, and the pair
      where a `copy` still wins (with benchmarks §9b's numbers, which
      correct the obvious guess: the allocation is equal, the cost is
      field traffic). Linked from docs/README.md and guide.md §10.
      It also found `optic-law-rewrites` unfiled, above. The entry as
      written: `docs/guide.md` does not mention `Lens`, `Prism` or
- [ ] optics-guide-page — the convenience item, and the first to
      pick: `docs/guide.md` does not mention `Lens`, `Prism` or
      `Traversal`; tutorial §23 and theory ch. 10 are all the prose.
      A page of PAIRS — nested `copy`, an `Option.map` chain, a
      hand-written walk, each beside the optic that replaces it and
      the verdict table's price for both — and the two roads named:
      a path written in code is free, an optic chosen at run time
      pays the interpreter. Docs-only; the guide's index guard is the
      gate.
- [ ] optics-arrow-instances — `Arrow[Function1]` and
      `Arrow[[A, B] =>> A => B ! R]`, so `split`/`fanout` exist on
      plain and effectful functions and the "one Profunctor" sentence
      in Optic.scala is a fact. Deliverable is the laws' tests
      (`TestMealy` states them over an input), not a capability: with
      a monad in hand `fanout` adds little over a for-comprehension,
      and the entry says so before anyone measures it.
      RE-CHECKED 2026-09-18: specs/static-workflow.md stage 1 wants
      the same category/arrow/choice laws as a property at `Proc`.
      ONE law suite parameterised by the carrier and an observation
      (`TestMealy` observes over an input) serves both; whichever
      lane lands first writes it reusable, the other reuses it.
      Neither lane adds the other's instance (that spec's Design).
- [x] optics-cont-profunctor — DONE 2026-09-18, HALF LANDED AND HALF
      REFUTED, which is the useful shape. `Strong` exists
      (`PState.Zooming` + top-level `opticZooming`), `PState.zoom` is
      now `l[Zooming[X, R]](m)` with TestZoom's six passing unchanged,
      and the instance bought three roads the hand-written shift never
      had: an iso zooms, `first` zooms a pair state, a composed optic
      zooms. `Choice` CANNOT EXIST: on the absent case the zoomed
      program must still answer the inner program's `X`, and `X` is
      universally quantified — parametricity, not a type error, and
      the spec's predicted mechanism ("the indices will not line up")
      was wrong. The door that does exist prices itself in its type:
      `PState.zoomCase` answers `Option[X]`. TestContProfunctor pins
      the absence with the `Strong` summon beside it as the control.
      Not measured and not claimed: `zoom` now summons a fieldless
      instance per call; its four callers are all tests and no JMH
      lane covers it, so a lane that gives it a production caller
      prices that first.
- [ ] optics-prism-selective — a second `Star` interpretation over
      `Selective[F]` whose `right` lifts the preview into `F` and
      `branch`es, so `Static` through a prism reports BOTH arms where
      the applicative road reports the one taken. One test with the
      matched control beside it.
      WAITS on static-workflow stage 3 (2026-09-18): for an arrow
      that is a TERM the question is already answered — `Proc.leaves`
      reports both sides of a `Left`, and stage 3 runs a prism's step
      on the matching variant. What is left is the `Star[F]` road
      alone; take it only if a consumer wants the applicative `Static`
      through a sum rather than `Proc`. Close it when stage 3 lands
      and nobody has.
- [ ] optics-field-fuse — `Lens.field[S]("name")` is the one
      constructor the planner cannot read (verdict, 2026-09-10), so it
      pays the interpreter while `Lens[S](_.f)` is free. Measure it
      first (it has no benchmark row), then either teach `Fuse` the
      `FieldOf.apply` shape or say the price on the guide page.
- ANSWERED, not a lane (ui-path-two-walks, 2026-09-18): the two walks
  STAY two, and the measurement is why — `PathWalkProbe` at depths 4,
  16 and 64 puts the affine at 2.9-7.9x the time and a steady ~5.5x
  the allocation of the hand walk, because it is built from a runtime
  `List[Int]` and pays the interpreter per step. What the question
  found instead was worth more than the tidy-up would have been: the
  two walks DISAGREED about totality. `Ui.path` answers an Option;
  `Ui.patch` indexed its Vectors and threw on a path naming nothing —
  and a Patch arrives over a WIRE, so a well-formed message with a path
  that is not on the client's tree killed the session inside
  `Wire.client`'s receive loop. Guarded now, with the law in
  TestUiOptic extended to cover it.
- NOT a lane, a record (indexed optics, re-checked 2026-09-18): the
  spec's "nobody has asked" was wrong. Three walks carry an index by
  hand — `Validate`'s `At` path through `Schema.Step`, `Ui.diff`'s
  `path: List[Int]` through `go`, and ui-direct-example's field key
  IN the error for `Ui.key(k)` to aim at. None is handed to a second
  interpreter, so the family stays out. TRIGGER, restated with the
  seats named: a fourth path-carrying walk, or two of these wanting
  one walk.

## openapi — the document as a rendering (specs/openapi.md, operator's ask 2026-09-11)

The spec is written and names the consumer first, as
`optics-outside-describe` demands. What it needs from okay-http before
a document is worth serving belongs to THAT arc and is listed here so
its owner can price it:

- [x] openapi-responses — DONE (`Router.out`/`outAt`/`jsonOut`/
      `jsonOutAt`, `Entry.answers`); the box was left unticked after
      the work landed and is corrected here.
- [x] openapi-queries — DONE 2026-09-11 as openapi-parameters, and it
      was two gaps rather than one: the query declarations AND the
      path parameters' kinds, both for the same reason. `Router.Entry`
      took `route.describe` — a STRING — so the renderer re-parsed
      `{name}` out of the template and called every path parameter a
      string; `Route[Int]("id")` was published as text. The entry now
      carries `Route.Described` (template + params + queries), and the
      value exists so the two can never be passed apart again. A
      `Param` carries its own JSON Schema, through okay-codec's
      `JsonSchema.of` rather than a second mapping, and a custom
      `Param` may override it (`format: uuid`) — tested.
- [x] openapi-render — DONE (`okay-openapi`, the law included); box
      corrected 2026-09-11 alongside openapi-responses.
- [x] openapi-serve — DONE 2026-09-11 (4758e8f7). `/openapi.json` and
      a page rendered on the server with no network, plus okay-demo's
      committed document and its drift test — the shape
      okay-demo/deploy already has. The drift test earned its keep on
      its first run: `/app.js` is served only where the linked bundle
      exists, so the document describes the PACKAGED surface.
      What it exposed for the next lane: all six demo operations still
      render `undeclared`, because HTML, server-sent events and a byte
      bundle have no `Schema` to declare them. `out`/`jsonOut` cover
      JSON only, so the answer is an `Answer` that carries a MEDIA
      TYPE and combinators that encode it — same declaration-by-
      construction, wider than JSON.
- [x] openapi-media — DONE 2026-09-11. `Router.Answer` carries a media
      type; `html`/`bytes`/`events`/`media` declare content that has
      no schema the same way `out` declares a value — the router
      writes the content-type, so the declaration cannot drift. The
      demo declares all six operations and its document says
      `undeclared` zero times.
- [x] openapi-prose — DONE 2026-09-11. `Router.summarised(text)` on
      the entry just declared; the document renders `summary` and the
      page shows it; `out`/`jsonOut` took the answer `description`
      too. okay-demo summarises all six, with a test that refuses a
      published operation without one. Tags, a long description and an
      operationId override stay undeclared — each wants a consumer.
- [ ] openapi-ops — okay-ops builds all four of its responses by hand,
      so its operations are the `undeclared` ones left in the
      repository. `/stats` is a straight `out` over `Store.Stats`,
      whose `Schema` it already summons to encode. `/metrics` is
      `media` with Prometheus's own content-type. `/healthz` and
      `/readyz` are the interesting pair and the shape of the next
      decision: their STATUS is chosen per request (200 or 503), and
      every declaring combinator fixes the status at declaration
      time — so they need either two declared answers and a handler
      that picks between them, or an answer type that carries its
      status. Worth deciding on that case rather than widening the
      whole surface for it.

- [x] optics-outside-describe — CLOSED 2026-09-11. The consumer
      arrived and was not built by this arc: a sibling wrote
      `okay-openapi`, quoting this entry's own rule back at it. The
      description then had to be made worth reading — see
      openapi-queries above — and the shape of that work is the
      lesson: the renderer had ALL the names it needed and none of the
      kinds, because the router flattened `Routed` to a String at its
      door. A DESCRIBE interpreter is only as good as what survives
      the boundary it is read across.
      What remains undeclared is HEADERS, and that is a route
      declaration, not a renderer feature: a handler reads a header
      off the `Request` with nothing declared anywhere. Not started —
      no consumer has asked.
- [x] optics-outside-routes-body — request bodies DONE (2026-09-11,
      stage 7, `Router.json[B]`); headers and RESPONSE bodies remain. `Request`/`Response` already carry a `Body` and okay-codec
      has `Schema`, so a body declaration is the `Toolbox` shape
      (stage 2) rather than the `Query` shape. Wanted BEFORE the
      renderer if the renderer is to describe anything but paths.
- [x] optics-outside-tools — DONE (2026-09-10, 21bc8a5c), and it was not
      on this list. Stage 2 of the spec: a tool was declared three
      times (hand-written JSON Schema, a dispatch map re-reading the
      same field names as string literals, and the name written
      twice). `Toolbox` gives `specs` and `table` from one vector.
      The finding: the hand-written schemas never declared `required`,
      so no model was ever told `board_add` needs both its fields.
- [ ] optics-outside-tools-effectful — `Toolbox` handlers are
      `A => String`, because that is the seam `Mcp.Server`,
      `Handlers.tools` and `Stepper` already take. A tool that must do
      I/O has to close over its own runner today. Widening to
      `A => String ! Rest` is a separate decision with those three
      callers to carry; see the spec's Out of scope.
      RE-VERIFIED 2026-09-11: `Persist.append(partition, key, value,
      ack): Long` returns a value directly — okay-persist is
      synchronous by design, so nothing in the tree gives a tool a
      reason to suspend. TRIGGER: the first tool that must do I/O its
      caller cannot do for it.
- [x] optics-outside-tools-adopt — DONE (2026-09-11). The entry was
      wrong that only tests were left: okay-demo's `RepoAgent` is an
      application and held two `ToolSpec` vals beside a `Map` under
      the same names, handed to `RepoMcp`'s server as two arguments.
      It and the five test tables are one `Toolbox` each now. The
      finding is a BEHAVIOUR one the drift argument had not predicted:
      the hand-written decode answered `bad args: ...` as prose while
      `Toolbox` answers `{"error": ...}`, so the same agent reported
      failure in two shapes depending on which module declared the
      tool. `TestRepoTools` pins it, in the default gate — the
      existing `TestRepoAgent` is `Live`-tagged and indexes the whole
      repository, so the tools' contract had no fast test.
- [ ] optics-outside-policy — a projection policy is a traversal:
      which fields of a record may be seen, embedded, logged. The
      interpreter that earns it is the AUDIT — "name the fields this
      policy touches", with no document in hand — and the law that
      couples the two interpreters is that the audit names exactly
      the fields the redaction changes. This repository has already
      paid for not having it: price and contact in a summary's text
      sank a priced offer from 0.63 to 0.13
      (`weighed-not-read-out-of-the-embedding`). Cheapest of the six.
      RE-MEASURED 2026-09-11 against okay-leads, the closest seat this
      repository has ever had: a `Lead` never travels redacted.
      `Demand.Report` is aggregates and cannot carry a contact BY
      CONSTRUCTION; `Demand.deliverable` is a row filter, not a field
      projection; `TestNoCredentialLogs` is still a regex over
      committed source with no value to audit. TRIGGER: the first
      place that hands a record onward with SOME fields removed AND
      must answer "which fields does this policy touch" with no
      document in hand. Redaction alone is a function; the AUDIT is
      what earns the traversal.
- [ ] optics-outside-live — subscribe to a lens. The lens compiles to
      a wire path, the server pushes only the focused part of the
      document, and a client write comes back as `set`. The most
      valuable of the six to a user and the most work: the optic must
      be reifiable and must survive serialisation. okay-live,
      okay-persist, okay-crdt.
      MEASURED 2026-09-11 at the only live consumer in the tree, and
      it is already minimal: `ChatDemo` publishes a KIND
      (`feed.publish("board")`) and the client re-fetches
      `/board.json`. A lens-addressed delta would replace a cheap
      re-fetch of a handful of tasks. TRIGGER: a document large enough
      that re-fetching it on every change is the measured cost — with
      the measurement, not the intuition.
- [ ] optics-outside-query — a query is an optic. `Forget[Sql]`
      compiles it to SQL, `Function1` runs the SAME predicate over a
      `Vector` in a test, `set` compiles to UPDATE. The seat is empty
      — okay-sql is strings plus `Schema` for rows — but typed query
      DSLs are a swamp, which is why the spec ranks this fourth and
      not first. Do not start it before routes and policy have
      measured the shape. STILL BLOCKED 2026-09-11: routes did; policy
      has no seat (above), so the shape is half-measured.
- [ ] optics-outside-topology — a dataflow is an arrow. The ONLY one
      of the six that meets the staticness condition the spec sets for
      reaching for `Arrow` at all: the graph must exist as a value
      before it runs, to be drawn, fused, or shipped to a cluster.
      Wants `ArrowChoice` or branches fall out of the static picture.
      okay-flink / okay-kafka / okay-reactive, and dataflow's own plan
      value is the incumbent to compare against.
      MEASURED 2026-09-11, then CORRECTED the same day. The first
      measurement said "nothing here meets the condition" and was
      about `Stage`, not about the tree: `Stage[I, O, A] = A ! (Take %
      I + Writer % O)` is a PROGRAM, everything past the first effect
      lives in a continuation, and a monadic pipeline's shape
      legitimately depends on its values. All true, and not an answer
      to the question asked.
      THE CONDITION IS MET, by `okay.Tables.Plan[A]` — a GADT of the
      plan, whose own comment says why it is not a `Free`: "its
      continuations are functions; the tree can". It has the three
      interpreters an `Arrow` would have been reached for: `show`
      draws it (okay-spark's TestWroclawAlgebra collects rendered
      plans), `optimize` rewrites it — a projection into its `Read`,
      the small join side to the right — under the law "the turned
      join answers exactly what the written one does", and
      `compile(B)` runs it over any `Bulk[D]`, local `Chunks` and
      Spark alike. Drawn, fused, shipped to a cluster: all three.
      SO THE CANDIDATE IS ANSWERED RATHER THAN WAITING. What it asked
      for exists, and it got there with an ordinary GADT and no
      profunctor — which is the same verdict `optics-outside-conf`
      reached by a different road, and the reason this arc keeps
      asking for a consumer before an abstraction. What an `Arrow`
      would add over `Plan` is `ArrowChoice`-shaped branching and
      composition laws nothing has asked for; reopen it if a plan
      needs to branch on a value and still be drawn.
- [x] optics-outside-conf — CLOSED 2026-09-11, with one test and no
      new abstraction. The PATH half is refused by a decision already
      in the tree: `Serve.Config` is "flat and scalar on purpose" and
      `Conf.fromEnv` refuses a nested field by name, so a lens into
      `server.tls.port` would be machinery for a shape nothing here
      has. The REFERENCE-TABLE half was already built — `envName` is
      one derivation for the reader and the renderer — and was
      under-consumed: the list was held against the deployment and
      against nothing a person opens, so `OKAY_ACME_EAB` was
      declared, deployable, read at boot and named in no guide.
      `TestScriptConfig` now asks that question of
      docs/okay-script-guide.md. The law runs one way only: a program
      reads variables its config does not declare (`OKAY_CONF` names
      the config FILE; `OKAY_STAGING` is okay-staging's switch), so
      "every OKAY_ in the guide is a setting" is false.

## okay-blob — from a consumer (2026-09-16, okay-watch)

These four come from OUTSIDE: okay-watch backs its case log off the
volume through `Blob`, and hit one real defect doing it. Each entry
carries what produced it rather than a wish.

The defect, because all four point at it. `Blob.put` takes
`Chunk[Byte] ! (Produce + Async)`. `Produce` is the identity
signature, so the element type sits in the ANSWER position, and
`pure(chunk)` therefore type-checks — and emits nothing, because
`Stream[Producer, Pure]` reads `Free.Pure(_)` as the END of the stream
and discards its value. The result was a zero-byte object under the
right key. Two symptoms from one cause: the caller's size test never
matched so every backup pass re-copied, and the restore answered
`refused: no header`. Neither compiler nor runtime said a word; a
round-trip test found both. Nothing here is a bug report — the
algebra does exactly what it documents — but the wrong thing was the
one that type-checked, which is a shape worth removing.

- [x] producer-drains — DONE 2026-09-16: `Producer.fold` and
      `Producer.concat` in core; the ten drains are one call each, and
      tail-recursive across chunks where they recursed through `map`.
      Was: the survey behind blob-source-road counted
      TEN hand-rolled `uncons` loops draining a
      `Chunk[X] ! (Produce + Async)` into a `Vector[X]` — Backup and
      Offload `drainList`, S3 `drainBytes`, Fs `sink`, jdbc `Poll`,
      `SqlStore`, `Migrate`, `Writes`, outbox `Rows`, rag `PgVector`
      — each summoning the same `Stream` instance and writing the
      same `go`. One `Producer.toVector` (or `each` folded into an
      accumulator) beside `Producer.each` retires all of them, and
      each is covered by its module's own suite. Six modules, so a
      full gate.
- [x] offload-getbytes — DONE 2026-09-16 (producer-drains). Was:
      `Offload.fetchBytes` is `Blob.getBytes` with
      a throw on the Left: a fourth copy of the walk `Producer.each`
      replaced in Backup, twenty lines that are now one call. Lands
      with producer-drains.
- [x] emptychunk-public — DONE 2026-09-16 (producer-drains): public,
      with its doc saying what it is for. Was: `Chunks.emptyChunk` is `private[okay]`, so
      a consumer writing a byte producer's terminator by hand, or
      passing `Source.toProducer`'s `end` for chunks, spells
      `ArraySeq.empty[Byte]` and hopes it is the same thing (it is —
      an empty `ArraySeq` is what `emptyChunk` casts to). Either make
      it public or give `Bytes` an `empty`; `putSource` already hides
      it for the common case.
- [x] blob-byte-source — DONE 2026-09-16 (blob-source-road):
      `Bytes.file`, `Bytes.stream`, `Bytes.fileSource` and `putFile`
      on the jvm; `Backup.stream` is now one line. Was: a byte stream from a `Path` or an
      `InputStream`, in the library. The 64 KB read loop that
      `Backup.stream` has (private, `okay-blob/.../Backup.scala:65`)
      now exists a SECOND time, copied verbatim into okay-watch,
      because there was nothing public to call. Anyone else putting a
      file into a Blob writes it a third. Cheapest of the four, no
      breakage, and it removes the hand-written `effect` from every
      caller — which is where the defect above lives.
- [x] blob-put-bytes — DONE 2026-09-16 (blob-source-road):
      `putBytes`, `putChunk`, `getBytes`, concrete on the trait, and
      `Producer.each` in core for the walk that keeps the answer. Was: `put` over what callers actually hold: an
      `Array[Byte]`, a `Chunk[Byte]`, a `Path`. Today storing a file
      requires learning the Produce algebra first, and the streaming
      form is the only form. Independent of the above and smaller;
      together they would have made the defect unwritable without
      touching the trait.
- [x] produce-at-a-wider-row — DONE 2026-09-16 (blob-source-road): the
      answer was already in Row — `produce(a).plus[Async]` is a
      zero-cost coerce, not the walk `!.widen` makes — so `produce`'s
      doc says so and names the trap beside it. Was: `produce(a)` is the named injector and
      is typed `A ! Produce` precisely, so a program in
      `Produce + Async` cannot call it. `!.widen` does lift it (that
      is what `Source.of` uses), at the price of a tree-rewriting
      pass a multi-chunk stream should not pay per element, so the
      idiom in practice is the wide `effect[Produce + Async, A](a)` —
      okay's own benchmark spells it that way too
      (`effect[Ask + Produce, Int]`). The point is that at the wide
      row the named safe call is unavailable and `pure` is not:
      reaching for `pure` is exactly the mistake above. A
      `produce[F[+_], A](a): A ! (Produce + F)` would close it; one
      sentence at `produce`'s doc pointing at `!.widen` and at this
      trap would close most of it for nothing.
- [x] blob-source-seam — DONE 2026-09-16 (blob-source-road), ADDITIVELY:
      `Source.fromProducer`/`ofProducer`/`toProducer` in core,
      `putSource`/`getSource` concrete on the trait, the primitives
      untouched, no engine changed. A test asserts the asymmetry that
      justified it: `pure(x)` a silent nothing at Produce, a type error
      at Source. Re-typing the primitives themselves stays declined
      until something needs `Flush.now` at the engine. Was: the real fix, and the expensive one: re-type
      the seam on `Source[Chunk[Byte]]` (`Unit ! (Writer % W +
      Async)`) instead of `Chunk[Byte] ! (Produce + Async)`. The
      answer becomes `Unit` and the element type moves into the
      SIGNATURE, so `pure(x)` can no longer be mistaken for an emit —
      the defect stops being expressible rather than being documented.
      It also buys `Source.of`, `Source.unfold` and `Source.apply` as
      ready constructors, every Writer/Stream combinator, `merge`, and
      `Flush.now` — an explicit chunk boundary, which is precisely
      what `S3.put`'s own comment wants when streaming bodies and
      multipart arrive. COST, stated: `Blob.put`, `Blob.get`,
      `Blob.list`, `Blob.Counted`, `Fs`, `S3` and `Backup` all move,
      and `get`'s `Either[String, Unit] ! (Produce + Async)` needs a
      shape that carries an outcome beside a Source. Worth pricing
      before taking, and worth taking only if the seam is going to be
      touched for streaming puts anyway.

## okay-http
- [x] route-headers-a — DONE 2026-09-11. A request header is a
      `Named[T]` in a third place: `:@`, the same spellings
      (`as`/`opt`/`all`), rendered `in: header`. The design fact worth
      keeping: a header CANNOT join the route's `A` without breaking
      `unapply(url(a)) == Some(a)`, so `Routed[A]` stays a prism on the
      url and `Headed[A, H]` is the request-shaped declaration. One
      builder serves both query and header because a header block IS a
      `Map[String, Vector[String]]`.
- [x] route-headers-b — DONE 2026-09-11. `.secured(scopes*)` on the
      request-shaped declaration, `Router.enforcing(verify)` on the
      table, 401/403 in `answers` without the author writing them, and
      `securitySchemes` + a per-operation `security` in the document.
      okay-admin converted: its seven existing ladder tests pass
      through the new road unchanged, which is the evidence the
      conversion preserved behaviour.
      Two decisions worth keeping. The module boundary shaped the
      interface — okay-security depends on okay-http, so a route
      declares DATA (scheme, scopes, realm) and `Router.Verify` is
      `String => Either[String, Set[String]]`, with `Secure.verifier`
      adapting; a `Policy` that reads the action or resource stays
      with `Secure.granted`. And FAIL CLOSED: a secured entry whose
      table never got a verifier answers 401 `no_verifier`, because
      declaring a requirement and forgetting to enforce it would open
      a hole the document swears is shut.
      The law is asserted twice, as set equalities: `enforcing`
      refuses exactly the secured entries, and the document marks
      exactly those operations.
- [x] route-headers-c — DONE 2026-09-11. `Answer.headers` and
      `Router.answering(status, names*)`, rendered as
      `responses[*].headers`. The distinction the stage exists to
      state: a secured route's `www-authenticate` is declared AND
      written from one value, so it is true by construction; an
      author's declaration is DESCRIPTION, and enforcing it would turn
      a documentation slip into a 500. Saying which half is
      load-bearing beats pretending both are.
- [x] route-secured-with-a-value — DONE 2026-09-11, in two steps the
      same day. `media`/`html` came first, because okay-demo's document
      rendered `/admin/replay` with a 401, a 403 and NO SUCCESS CASE
      and the demo's own guard caught it; the rest — `htmlAt`,
      `bytes`, `bytesAt`, `events`, `eventsAt`, `json`, `jsonAt`,
      `out`, `outAt`, `jsonOut`, `jsonOutAt`, on the class and the
      companion — came when the operator asked. Each carries
      `securityAnswers` beside whatever it declares itself.
      The wrinkle a caller meets: the `Headed` forms carry NO default
      arguments (Scala allows them on one overload of a name), and
      `status`/`description` must be passed POSITIONALLY, because a
      named argument narrows overload resolution before the argument
      types are read.
- [ ] route-headers-adopt — REDUCED 2026-09-11 after looking properly.
      `McpHttp.route` is a TOTAL `Request => Response` by design —
      `McpAuth` depends on that totality, and its comment says so — and
      it answers any verb on one path, which a `Router` entry cannot
      say. okay-script's header reads are all inside `Site`, a page
      server rather than a table. So there is no clean seat for stage
      A beyond the tests; the stage-B half found its consumer instead
      (okay-admin, then okay-demo). Left open in case McpHttp ever
      becomes a table, not as work waiting to be done.
- [x] route-arity-one-tuple — DONE 2026-09-11. The entry asked for a
      second sighting; there were four, three by authors other than
      the one who wrote the entry: the sibling who wrote `TestOpenApi`
      reached for `t.head`, `TestRouterOut` did the same, okay-demo's
      `/events/{email}` carried a comment saying `.head` is "the
      spelling until it has a better one", and I wrote `Tuple1(id)` to
      build a url. That settled it.
      Neither remedy on paper was taken. An `on1` overload doubles the
      surface, and an `Extract[A]` MATCH TYPE says what the parameter
      is and leaves the router to cast into it — which AGENTS.md
      forbids. `Route.Arity[A] { type Out }` is a witness that CARRIES
      the conversion, the same reason `Split` is a witness and not
      `Tuple.Concat`. It collapses at the HANDLER only: `unapply` and
      `url` still speak in tuples, because the optic's laws are stated
      over `A`.
      Measured: 8 signatures on `Router`, 4 on its companion, and the
      whole repository needed FOUR call-site edits. Arity 2 still
      untuples as `(id, slug) => ...` and arity 0 is still `_ => ...`.
- [x] optics-outside-route-of-labels — DONE (2026-09-11, a450ff85). The
      operator settled the open question ("it will be needed"), and
      the entry's own doubt was half wrong: the check catches more
      than a documentation typo, since `describe` publishes those
      names to OpenAPI and to MCP tool schemas. The cost the entry
      worried about is real and stands — a field deliberately named
      differently from the url is now refused, and must be renamed on
      one side. `of[C]` is inline, reads `MirroredElemLabels`, and
      compares path parameters then query parameters against the
      fields; the refusal was verified by removing it and watching
      both tests fail.
- [ ] compile-time-only-is-not-a-guarantee — measured 2026-09-11 in
      optics-outside-route-syntax. `@compileTimeOnly` was tried as the
      carrier of a nicer refusal message (a poisoned `/` on
      `Route.Named`, since precedence sends the mistake there). It
      fired in five isolated variants — dotted, infix, overloaded
      method, overloaded caller, extension receiver, parenthesised and
      not — and silently did NOT fire in the real expression, even
      after a clean rebuild; worse, its presence made two invalid
      declarations compile, because the poisoned method returned a
      usable type where its ABSENCE had produced an error. The trigger
      was never isolated. Anyone reaching for `@compileTimeOnly` in
      `specs/error-messages.md` should read this first: it is fine for
      a message, and must not be the thing that makes an invalid
      program invalid.
- [ ] flaky-port-roulette — the full-matrix port/readiness family,
      one ledger: TestMcpHttp 503 (2026-09-01), TestResumable first
      subscribe, TestHttp first GET 404, and TestWire reading
      literal "HTTP" bytes at its handshake (a foreign server
      answered on the expected port) — all green alone, all under
      parallel suites in one sbt JVM; suspect ephemeral-port reuse
      between a closing listener and a dialing client

- [ ] http-flaky-mcphttp — TestMcpHttp "one Serving, three wires"
      answered 503 once in a full-matrix run (2026-09-01); green
      alone and on suite rerun — likely a port/readiness race
      (second sighting, same family: okay-jetty TestResumable
      failed its first subscribe once in a full-matrix run
      2026-09-01, green twice alone — port/readiness race shape)

- [ ] http-streaming-responses — incremental bodies on the NIO and
      Netty backends (Jetty has it); unblocks MCP push there

## okay-ui
- [ ] ui-terminal-v2 — what is LEFT after ui-terminal-keys read the
      escape sequences and ui-terminal-width gave the layout a budget
      (both 2026-09-18): an `Input` edits by append and backspace with
      no CURSOR, `Scroll` renders its child whole and clips nothing,
      and there is no mouse.
      WHAT EACH NEEDS NOW, since the size question is answered:
      a CARET is host state (Left/Right are reserved for it already,
      and Home/End would have to mean line-start/line-end while
      focused on an `Input` rather than the ends of the tab order — a
      conflict to decide, not just code); SCROLL needs the HEIGHT
      budget and an offset per keyed Scroll, which is the same shape
      the width took and is the cheapest of the three now; the MOUSE
      needs SGR reporting (`ESC [ < b ; x ; y M`, which `Frame.feed`
      would decode) AND hit-testing a rendered frame back to a widget,
      which no part of this host does — a rendered line does not
      remember which node made it. TRIGGER unchanged: a product that
      runs on the terminal host for a PERSON.
- [ ] ui-native-hosts-unread — Swing, GTK, Compose, SwiftUI and the
      Android APK have each drawn the conformance script and a
      counter; none has drawn a product page. NARROWED 2026-09-18
      (native-tokens-tested): the tokens ui-text-intent gave them are
      no longer a claim — TestSwing asserts a monospaced identifier, a
      right-aligned `Align.End` label and the ABSENCE of tabular
      figures Swing cannot draw; TestGtk asserts the `monospace` and
      `numeric` style classes against real GTK widgets (a
      `gtk_widget_has_css_class` binding was added to read them back).
      What is still unread is a SCREEN: layout under a product's
      density, not a token. The recorded gaps
      (Swing: gap not drawn, weights as natural sizes; GTK: no
      weights, multiline as a plain entry, images as labels,
      `Gtk.window` never run with an app; Compose claims nothing;
      the iOS app bundle and the emulator run are still `[ ]` in
      specs/frontend.md) are honest and unranked, because nothing
      with a reader has ranked them. okay-watch found four things in
      two days on the browser; a native client will find its own. Not
      a lane — a record with a TRIGGER: the first product screen on a
      native host, which becomes that host's ui-product.
- [ ] ui-windows-terminal — raw mode beyond stty. WRITTEN OUT
      2026-09-18, because a one-line stub told the next reader
      nothing, and because ui-terminal-keys made the remaining job
      smaller than it looks.
      WHAT BREAKS: `Terminal.raw` shells out to `stty raw -echo`, and
      Windows has no stty outside a POSIX shell. Nothing else in the
      host is POSIX — painting is ANSI, which Windows 10+ draws once
      `ENABLE_VIRTUAL_TERMINAL_PROCESSING` is on.
      WHAT NO LONGER NEEDS DOING: the KEYS. With
      `ENABLE_VIRTUAL_TERMINAL_INPUT` set, a Windows console sends the
      same `ESC [ A` sequences a POSIX terminal does, and
      `Frame.feed` already decodes those — so this is now only about
      the MODE, not about a second input vocabulary.
      THE SHAPE, dependency-free (okay-ui takes none): the same trick
      `stty` is — a CHILD PROCESS configuring the console its parent
      shares. `powershell -c` with an `Add-Type` P/Invoke of
      `GetStdHandle`/`GetConsoleMode`/`SetConsoleMode` does it without
      JNA and without a native image step; the bracket shape of
      `Terminal.raw` (on, run, off) is unchanged.
      TRIGGER, and it is a hard one: a Windows box to VERIFY on. This
      is platform code whose whole content is a side effect on a
      terminal nobody here has — landing it unverified would put a
      claim in the repository that no test and no person has ever
      seen hold, which is worse than the honest gap.

## okay-script
- [ ] script-tls: ALPN/HTTP2, OCSP stapling, cipher policy — still the
      proxy's, and named as such in the spec. A Site behind Caddy/nginx/an ingress needs
      three things from the operator: pass Upgrade for EVERY path
      (a live page's socket is on the page's own path),
      `OKAY_FORWARDED=1`, and to treat `X-Forwarded-For` as a claim.

## okay-py
- [ ] py-arrow — frames via pyarrow (twin of r-arrow). RE-FILED
      2026-09-07 with an honest number: the measurement meant to
      justify it found that 60% of a 500k-row frame's 9.7 s round trip
      was OUR OWN `Json.parse` taking the lossless road
      (json-parse-fast-road). The same frame is now 0.94 s, of which
      the Python side is roughly half and our encode 0.3 s. Arrow
      would still take the serialization hop out, but "the JSON-frame
      road hurts" is ten times less true than when this was filed and
      no consumer has asked. Measure again before building.

## okay-mail
- [ ] mail-consumer-adoption — the consumer who asked for okay-mail
      replaces `Identity.console` with it. Not my lane to do, but the
      one that tells whether the seam is right: their `deliver` is
      `(Channel, String, String) => Unit` and `Mail.Send` has to plug
      in without anything else changing, which was their stated
      requirement.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## okay-parse
- [ ] parse-depth-timer-warns — `TestParseDepth.timeMs(body: => Unit)`
      makes its four callers discard a `Parsed`, which is four E175s
      on every platform and therefore a warning on master that every
      landing inherits. It is NOT a mechanical fix: making the helper
      generic was tried (47dbc639) and reverted, because the test
      then read 27.4x where it asserts under 8 and the honest reading
      is that a TIMING test is the one place AGENTS.md forbids
      rewriting to please a linter. Whoever owns this test should
      change it and re-establish its measurement in the same lane —
      the `: Unit` ascription at the call sites is the candidate that
      generates the same code the implicit discard did.

## okay-cluster / dataflow

- [ ] **shipped-terms — a lambda on the cluster, without shipping a
      closure** (operator, 2026-09-18). `specs/dataflow.md` Claim 3
      says "nothing ships a closure" and states its price honestly:
      *you cannot type a lambda into a REPL and have it run on the
      cluster*; a plan whose leaves are anonymous functions runs
      locally and is refused at submission. The registry is the reason
      the whole class of `Task not serializable` failures does not
      exist here, and that is worth keeping.

      THE PROPOSAL IS TO LIFT THE PRICE WITHOUT LOSING THE CLAIM:
      ship the leaf as **TASTy** (Scala 3's typed AST) plus its
      captured environment as **CBOR** under a `Schema`. That is not
      Java serialization of a JVM lambda — no Kryo, no registration
      list, no synthetic `$anonfun$foo$1` to go stale — so the failure
      class Claim 3 eliminates stays eliminated. It is the Spark
      CAPABILITY without the Spark MECHANISM.

      WHY THE OBJECTIONS THAT KILL THIS ELSEWHERE DO NOT APPLY HERE.
      The book's Appendix A rejects TASTy+CBOR for durable workflows
      on four grounds, and a cluster inverts every one of them:
      a compiler at restore is AMORTISED (compile once per stage, run
      over millions of rows — seconds against minutes, where the
      workflow case spent seconds to save microseconds); symbols
      resolving against a classpath COSTS NOTHING because "every
      worker runs the same artifact" is already this engine's stated
      bargain; an executable payload is a different threat model on an
      authenticated internal control plane than in a persisted
      journal; and pinned code is a FEATURE — version skew inside one
      job is a bug.

      WHAT IS ACTUALLY HARD, and it is one thing: getting the term.
      TASTy is emitted for definitions at compile time, so a macro at
      the submission site must reify `'{ ... }` and marshal the free
      variables. That is bounded here in a way it is not for
      workflows — only the LEAVES need terms (`map`, `filter`, the
      fold), not a continuation across pauses — and `okay-staging`
      already runs `scala.quoted.staging` in production with the
      compiler isolated in its own module and an off switch.

      KNOWN CONSTRAINTS to design against: `staging.run` refuses a
      second thread, so a worker compiling several stages concurrently
      serialises on it; TASTy is forward- but not backward-compatible,
      which a single-artifact cluster satisfies anyway; and a leaf
      whose captured value is not `Schema`-able must still be refused
      at submission, with the stage named, exactly as today.

      ADDITIVE, NOT A REPLACEMENT. Jobs by name stay the production
      road — no compiler on the worker, fastest start, and the
      operational story the spec already tells. Shipped terms are the
      exploratory road: a REPL, a notebook, a one-off. Two roads, and
      the spec's Claim 3 gains a second sentence rather than losing
      its first.

      DECIDING TEST, before any of this is built: a lambda typed at a
      REPL, submitted, and run on a worker that never had that code —
      answering the same checksum as the registered job that does the
      same thing. If that cannot be made to work in a spike, the entry
      is closed with the reason.

- [x] cluster-testfailure-untagged — DONE 2026-09-16 (operator-followups): the
      suite is `Live`-tagged, out of `sbt test`, in `integrationTest`. As
      found: `okay.cluster.TestFailure` binds a
      real `ServerSocket(0)` ("A CONNECTION THAT BREAKS EVERY TIME") and
      spawns worker JVMs, and it is NOT `Live`-tagged, against the
      nio-port-scope rule that every binding suite tags itself. Seen
      2026-09-15 in split-over-either's first gate: `test timed out after
      30 seconds` reported at 315.018 s — a JVM that stood still for five
      minutes, not a slow test — and the matrix came back with 4314 results
      instead of 4424. Alone on the same tree: 9/9 green. The gate does not
      distinguish "the box paused" from "the test is wrong", so either the
      suite carries the tag or the gate learns the 30 s-vs-300 s signature.
      Not fixed in that lane: its claim did not hold okay-cluster.
- [x] dataflow-reconnect — LANDED, both halves, and the measurement
      the entry asked for says both are needed. TOLERANCE: a worker is
      buried after three CONSECUTIVE failures and any answer clears
      its count, which makes a blip on EVERY worker survivable — with
      a tolerance of one, the new test dies with the same sentence
      stage 5's first seeded test produced, "no workers left (4 were
      given)". That alone is enough for a worker that hiccups and
      cannot be enough for a SOCKET, whose failure is permanent by
      construction, so `Served.reconnecting` dials lazily and drops
      the socket on any failure. Against a server that hangs up after
      every request, `connect` dies and `reconnecting` finishes the
      job — the two roads differing only in which `Serve` the
      coordinator was handed. `Run.failed` (attempts lost) is reported
      beside `Run.retried` (workers buried), because a run can now
      recover from a failure without burying anybody, and two tests
      that asserted the burial were asking the older question.
- [x] dataflow-coordinator — LANDED as stage 8. `Wire.state` makes
      the fold a value, `Checkpoint` is where it goes, and a second
      `Cluster.stream` over the same journal picks the run up. It was
      an assembly: the seam binds to okay-persist's compacted log in
      eight lines, and the reason it is a `save` call rather than a
      barrier protocol is that the epoch loop is lock-step.
- [x] dataflow-coordinator-election — LANDED as stage 10, and it was
      a lane rather than a line for the right reason: the wiring is
      small, and the FENCE it forced is the part that mattered. A
      deposed coordinator now stops at its next commit instead of
      writing over its successor.
- [x] dataflow-fenced-commit — CLOSED, on the READ side rather than
      with a compare-and-set no store here offers. `Folded` carries
      the term, `Checkpoint.newest` takes the highest (term, epoch)
      out of a journal's history, and a stale commit is shadowed for
      ever instead of being read back — which a log can do and a cell
      cannot. The honest half: the rows were right either way, because
      a stale resume costs WORK and not correctness as long as the
      source replays and the writer is keyed. For a source that does
      not replay, the fence is still a check and that window stays
      named.
- [x] dataflow-commit-window — LANDED as stage 9, and the entry above
      was wrong about the roads: both the ones it named are worse than
      the window, and the one it did not name is what every engine
      does. The engine cannot close it (the write left the engine), so
      it hands the writer `committed(epoch)` and `recovered(epoch)` —
      told BEFORE the journal, so the worst case is a repeated epoch
      rather than a lost one, and a writer that records the epoch
      beside its rows is exactly-once.
- [x] dataflow-durable-stage — CLOSED by being answered smaller than
      it was asked. The staging contract already requires the writer
      to record the epoch in ONE write with its rows; a writer that
      cannot be atomic cannot have exactly-once, and that is a
      property of the store rather than of the engine. Nothing was
      built to justify the lane. What the lane DID produce is the
      defect it turned up: a resume of a run that was already over
      retired the tail panes a second time out of one partition's
      half — 29 of 3 204 wrong — because a finished run did not record
      that it was finished. It does now (`Folded.done`).
- [x] dataflow-exchange — LANDED. The crossover is ~100 000
      accumulators and the Wrocław job is three orders of magnitude
      under it, so `Auto` declines the exchange there.
- [x] dataflow-auto-for-a-real-accumulator — CLOSED by measurement,
      and the prediction it rested on is REFUTED. §20's tuple tree
      (`count zip sum zip max`, six objects per add) crosses in the
      SAME 80 000-to-160 000 band as a Long count, in two runs — the
      accumulator's weight changes the slope past the crossing, not
      the crossing. One constant serves both and 100 000 is it.
      A methodological finding came with it: the first version of the
      table used a millisecond clock over lanes of 1-7 ms and reported
      the two crossovers four-fold apart. That was quantisation, not
      physics; the ratio is computed from nanoseconds now.
      The other half of the old comment — that fewer partitions move
      the bound up — is still unmeasured and is now labelled as such
      rather than asserted.
- [x] dataflow-onepass — LANDED. And it produced the number the
      engine had been unable to quote: 5.5x the hand-written lane.
- [x] dataflow-complete-panes — LANDED. 5.50x -> 1.14x of the
      hand-written lane; 122 679 accumulators reach the coordinator
      where ~2.9 million did.
- [x] dataflow-run-complete-panes — LANDED. The windowed `Wide` node
      has the rule: a partition that can finish a pane alone PRESENTS
      it into a per-bucket buffer, and only the boundary panes go into
      the maps a reducer merges. The buffer road, not the threaded
      terminal — the node never learns what it is folded into.
      Measured back to back on one box: the three-plan road goes from
      647 ms (8.19x the hand-written lane) to 182 (2.25x), 3.6x, with
      every other lane unmoved. And the check is a COUNT, not a clock:
      `Run.merged` is reported by the single-stage road now and
      `TestFlow` asserts it equals the fan's exactly, at 2, 4 and 8
      partitions. Two things fell out — `prepass` answers an `Extent`
      rather than one Long (the road had been computing half of what
      the rule needs), and the LAST partition has no upper bound at
      all, since the two bounds guard two different neighbours and it
      has no later one. That last is why a run at ONE partition now
      merges nothing, asserted.
- [x] dataflow-fan-overhead — CLOSED by measurement, and the third
      is not there (MeasureFanOverhead, Live). Re-measured lane for
      lane: the fan is 101-111 ms and its three sinks, each run as its
      own fan, sum to 90-92 — a gap of 9-20%, not a third. The 50 ms
      came from arithmetic across differently-shaped lanes (the
      bunching sink has NO pre-pass; the fan's has two columns) on a
      loaded box, and the parts have changed under it since (topK
      stopped sorting). Of the three candidates: a pre-pass column is
      5-6 ms over 1.25M events and a second column in the same pass
      4-5; an `and` arm that does nothing is 0-3 ms, which is below
      this instrument. And the finding worth more than the entry: THE
      FAN IS NOT FASTER THAN THREE SEPARATE FANS on this feed
      (89-96 against 101-111) — one pass saves ~2 ms of source reads,
      because the source is an in-memory array, and pays more than
      that for three operators' state being live at once. What a fan
      buys is a source read ONCE, which matters when the source is a
      file, a topic or a socket, and no shuffle. Pricing the residue
      needs JMH; nothing has asked.
- [ ] (SUPERSEDED, kept for the reasoning) dataflow-complete-panes: 84%
      of the Wrocław run was the sliding stop-window sink, and not
      because the windowing is slow: the job makes 362 983 stop panes,
      every partition holds most of them, and the coordinator merges
      ~2.9 million accumulators on ONE thread.
      `OkayLane.parallel` does not parallelise that merge, it avoids
      it — it emits at the slice every pane no other slice can touch
      (`p.start > hi(i-1) && p.end <= hi(i) - back`) and hands back
      only the handful that span a boundary. The engine already
      computes `hi`: it is the prefix-maximum array gathered for the
      watermark seeding. What is missing is `back`, the greatest
      backwardness, which is two more columns in the same pre-pass
      (each partition's local backwardness and its minimum event
      time). The bar is MeasureWroclawFlow's table: 121 ms against
      the hand-written 22.
- [x] dataflow-fan-exchange — CLOSED by its own condition. It said to
      consider this only after dataflow-complete-panes, because that
      lane might remove the merge instead of parallelising it. It did:
      122 679 accumulators reach the coordinator where ~2.9 million
      did, three orders of magnitude under the exchange's crossover.
      There is nothing left for an exchange to buy here, and a stage
      that wants one can still say `Finish.Shuffle`.
- [x] dataflow-processes — LANDED as stage 4. Jobs by NAME with
      Schema'd parameters, a four-byte length and CBOR, partials back;
      `Job`, `Jobs`, `Req`/`Resp`, `Served`, `WorkerMain`. The
      acceptance ran twice: the synthetic feed across four real OS
      processes in stage 4b (TestDistributed), and the full Wrocław
      Result across four of them in stage 7 (MeasureWroclawCluster),
      which is the one this entry asked for.
- [x] dataflow-recovery — LANDED as stage 5. A lost partition is
      recomputed on a survivor, and it is nearly free exactly as the
      entry guessed: a partition is a thunk and its partial is a pure
      function of (parameters, index, count, bounds). Under SEEDED
      schedules rather than luck — forty of them — plus a real worker
      process killed mid-run. Not under `Sim`: the seeds are the
      suite's own, because what varies is which worker dies at which
      request and that needs no virtual clock.
- [x] dataflow-streaming — LANDED as stage 6, except one half that
      was REFUSED rather than forgotten, and the difference matters to
      whoever reads this next. Landed: the epoch loop, the watermark
      as the minimum over the partitions minus the declared lateness
      (6a), a dying worker's partition replayed on a survivor (6b),
      and exactly-once OUTCOME at a keyed sink with the offers counted
      (6c). NOT landed, and argued against in 6b: keyed state in an
      okay-persist backend. A replacement worker REPLAYS rather than
      restores, because a partition is a recipe and snapshotting an
      operator's insides would make every one of them a wire format
      that has to survive a version change. The COORDINATOR's state
      does go to okay-persist — stage 8 — because that one cannot be
      replayed from anywhere.
- [x] windows-packed-key — WRITTEN, MEASURED AND REVERTED
      (2026-09-11). The entry said to measure before writing it; the
      ceiling justified writing it and the engine refused it.
      THE CEILING, on the two map shapes alone at Wrocław's own
      boundary count (122 679 entries, each inserted by a partition
      and merged by the coordinator, carrying the accumulator the job
      really uses): tuple-keyed HashMap 21 991 us and 32 503 104
      bytes, packed LongMap 12 253 us and 27 921 208 — 1.79x, about
      9 ms of a fan that runs in 111. Worth a seam, so one was
      written: a `Store` packing (window index, Int key) into one
      `Long` with a per-ENTRY fallback, because whether the packing
      fits depends on the data and not the types.
      IN PLACE IT LOST, and not narrowly. On the Wrocław job the fan
      allocated 1 506 199 512 bytes against 1 377 755 184 — 9.3%
      WORSE — and the sliding stage 13.1% worse, with the control (the
      source alone) unmoved at 0.0005%. The ceiling was measured on
      the wrong SHAPE: one map with 122 679 entries, where the engine
      has eighteen maps that grow into that total. An open-addressing
      `LongMap` copies its whole table on every growth where a
      `HashMap` allocates a node once and never moves it, and against
      accumulators that are objects anyway, the trade loses.
      The mechanism is reverted. WHAT STAYS IS THE INSTRUMENTS:
      `MeasurePaneStore` (the two shapes, the ceiling) and
      `MeasureWroclawBytes` (what the real job allocates, per road) —
      the second is the one that decided it, because allocation is
      deterministic where this lane's wall clock moves 10% between
      runs. Anyone reopening this needs a number from the second.

## okay-spark
- [ ] spark-4-2 — bump `spark-sql` 4.0.0 -> 4.2.0 and move
      `scala-reflect`/`legacyStdlib` from 2.13.16 to 2.13.18 with it
      (4.2.0 resolves 2.13.18; the two must stay a matched pair, and
      build.sbt says so beside the pins). The suite passed on 4.2.0
      during the migration, so this is a read of the release notes and
      a gate, not an investigation. Spark is still 2.13-only at 4.2.0,
      so nothing about the `for3Use2_13` arrangement changes.
      (was filed under "spark-4-2" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] §20's Spark row is a BATCH job over RDDs: no event time, a window
      is a key, the arrival index carried through the shuffle. Spark's
      answer to an event-time question is Structured Streaming —
      `.withWatermark("ts", "30 seconds").groupBy(window($"ts", "5
      minutes"), $"route")` — and that is the lane that would be
      like-for-like with Flink rather than with `groupingBy`. What it
      needs, all of it known: a `SparkSession` (so the two-stdlib
      classpath hack, which means the lane lives in okay-spark's tests
      and prints its own table), encoders for the row type, a memory or
      rate source with `Trigger.AvailableNow`, a checkpoint directory,
      and reading the result back from the sink. `SparkInterop.toSpark`
      already gives the typed aggregator for the Dataset side, so the
      "one Aggregator, every engine" line holds there too. Trigger: a
      reader who asks what Spark's watermark costs against Flink's.

## okay-deploy
- [ ] deploy-cli-native — a GraalVM/Scala Native binary needing no
      JRE. The renderers are pure string builders and would port; the
      question is whether a second build toolchain is worth paying
      for, and nobody has asked yet.
      (was filed under "deploy-everywhere" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

- [ ] deploy-host-verified — put the rendered systemd unit in front of
      a real `systemd-analyze verify` and the install script in front
      of a real rented box. Needs a Linux host; the unit is currently
      proven by its text, which is not the same thing.
      (was filed under "deploy-everywhere" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## dataflow — direction (2026-09-11; specs/dataflow.md stages 11-13, specs/federation.md)
- [ ] dataflow-source-log — stage 11: `Flow.topic` over okay-persist
      partitions that SEEK by epoch, and `Sink.stagingTo(topic)` whose
      append is the commit. Exactly-once from log to log on the
      repository's own primitive; verifiable on one machine. FIRST.
- [x] dataflow-netem — LANDED (TestNetem, default gate). Tolerance 3
      carries 20% loss with certainty, the knee is at 30%, half the
      runs die at 50%. And the finding: with burial OFF a 70% wire
      still finishes every run — on a lossy wire the loss never ends
      a run, the burial policy does, because three lost packets are
      read as a dead machine. `tolerance` is a parameter of
      `Cluster.run`/`stream` now; telling the two failures apart BY
      THE ENGINE needs a real wire (stage 12).
- [x] dataflow-rescale — stage 13: change the partition count between
      two epochs. LANDED (box 1 + box 3, TestRescale): a striped source
      (`Flow.striped`, `Job.rescalable`) into a keyed/fold sink resumes
      at a new width with the batch answer; the workers vector may
      change too. Two conditions the build now ENFORCES: the source
      must be striped (a contiguous cut has no global prefix) and the
      sink must keep its state in the fold (a windowed sink's open
      panes are not journalled). Both refuse rather than fake it.
- [ ] dataflow-rescale-windowed — stage 13 box 2: journal a windowed
      operator's OPEN panes so a re-cut can rescale it, rather than
      relying on replay (which a re-cut cannot do). Found by
      dataflow-rescale: today a windowed rescale is refused, naming
      this. The boundary panes would be re-bucketed under the new
      extents on resume.
- [x] federation-two-parties — specs/federation.md stage 1: two logs,
      two processes, one job, the answer equal to the union's and the
      bytes shown to be accumulators. LANDED: `TestFederation`,
      `Cluster.Refused` (a refusal is a `Resp.Failed` in process too —
      it was already one over a socket, and the split was the
      finding); 0.17% of the records' bytes crossed.
- [ ] federation-refusal — specs/federation.md stage 2: a worker with
      an allow-list of jobs, and a coordinator identity checked before
      the pre-pass; `okay-security` connected. `Cluster.Refused` is
      the answer's shape already.
- [x] one-binary-story — the small-business path, as ONE worked
      example rather than a module: events into a log, a windowed
      report over them, a page that shows it, a backup that leaves the
      machine — all in one process, run end to end in a test. The
      thing the repository is FOR, told once. LANDED: `okay.demo.Ledger`
      + `TestLedger`; every piece existed, the work was the seam and
      the honest sentence about it (a backup is the books up to the
      last roll). Two findings filed below.
- [ ] backup-active-segment — `Backup.copy` leaves the active segment
      home, so a backup is bounded by `segmentBytes` of unsaved books.
      A shop wants "everything up to now": copy the active segment
      under a distinct key (it changes, so it is not incremental) or
      roll on demand before a backup. Found by one-binary-story.
- [ ] dataflow-machines — stage 12 proper. BLOCKED: needs machines
      that are not this one. Not to be pretended at.

## okay-resilience

- [ ] **resilience-timed-tests-measure-the-box** — THE MODULE-LEVEL
      TASK the two entries below have been asking for one sighting at
      a time. It is one disease, not two flakes: this module's timed
      suites run in the DEFAULT gate and assert on wall-clock
      deadlines and exact counters, so under a full matrix they
      measure how busy the machine is and report it as a behaviour.
      THE EVIDENCE IS COMPLETE. Five failures across two suites, and
      EVERY ONE of them from a lane that cannot have caused it —
      dataflow numbers, an optics lane, continuations-audit,
      workflow-suspended-driver, and finally a lane whose entire diff
      is five markdown files with no executable code at all. Each
      passed in isolation on the same tree minutes later, 3-of-3 or
      better. One theory was tested and REFUTED (a yield-spin starving
      the fibre it waits for: 12 CPU burners at load 22 did not
      reproduce it), which is recorded below so nobody spends that
      hour again.
      WHAT IS ACTUALLY WRONG: `TestHedgeStart` waits five seconds by
      wall clock for a fibre to answer, and `TestResilienceTimed`
      needs three attempts to start 10 ms apart with the third winning
      at +5 ms and then asserts `starts == 3, cancelled == 2` exactly.
      Beside ninety other module runs, neither bound is defensible —
      not because the code is wrong but because the assertion is about
      the scheduler, not about hedging.
      TWO WAYS TO CLOSE IT, and the owner picks:
        (a) assert on a CONDITION rather than a deadline, and on
            BOUNDS rather than exact counters — `starts <= 3` and
            "eventually no attempt is running" say what hedging
            promises, and say it on any machine. `Live` exists for
            the parts that genuinely need real time.
        (b) move both suites to `integrationTest`, where a timed test
            is allowed to want a quiet box. Cheaper, and it takes the
            guarantee out of the gate that protects it.
      (a) is better and (b) is honest; what is not acceptable is a
      third year of ledger entries.
      TAKEN 2026-09-18 (hedge-bounds), option (a):
        - `TestHedgeStart.until` keeps the CONDITION as its assertion
          and turns the clock into a TRIPWIRE — 60 s, the line past
          which "slow" is "hung" — and SLEEPS instead of spinning on
          `Thread.yield()`, which on a loaded box can hand the core
          back to the one thread with nothing to do.
        - `TestResilienceTimed`'s hedge counters become the promise:
          a hedge happened (`starts >= 2`) and EVERY LOSER IS
          CANCELLED (`cancelled == starts - 1`), which is the only
          claim there that is about hedging rather than about time.
      ONE CORRECTION TO THIS ENTRY, found by running it: it names both
      suites as if both reached the gate. `TestResilienceTimed` is
      ALREADY `Live`-tagged and excluded from `sbt test` — so all four
      sightings are `TestHedgeStart` alone, and the timed suite's
      change is an improvement rather than a fix.
      SECOND REFUTATION OF THE BURNER THEORY, and it cost an hour
      because this entry already said it: 16 burners at load 93 did
      not reproduce the failure with the OLD code either (0 of 4,
      against 0 of 4 new, detector verified on a known-good run
      first). CPU pressure is not the condition. Every sighting was
      inside a FULL MATRIX — hundreds of processes, four sbt JVMs, GC
      pressure and paging — which is why DONE below says matrix and
      not burners. Do not spend a third hour on burners.
      ACCEPTED 2026-09-18: three full `affected master` matrices back
      to back, all GREEN, with `TestHedgeStart` in every one. WHAT
      THAT DOES AND DOES NOT SHOW, said plainly because the criterion
      above asks for "under load": those three ran while a docker
      image build held the VM and siblings were landing — ordinary
      traffic, not the four-matrix crush every sighting came from —
      and the OLD code also passed 4 of 4 at load 93, so no run of
      this size discriminates. What carries the change is that the
      assertions no longer name the scheduler: a condition with a
      hang tripwire, and `cancelled == starts - 1`. Three green
      matrices say it did not regress. The ledger stops growing or it
      does not, and the next sighting decides — if one comes, it is
      now a claim about hedging that failed, which is worth reading.
      DONE MEANS: a full matrix under load with both suites in it,
      three times, no failure — and the assertions readable as claims
      about hedging rather than about timing.
      COST SO FAR: five gate cycles, each roughly ten minutes, plus
      the investigation hour. Paid by five different lanes, none of
      which owned this module.

- [ ] resilience-timed-under-load — `TestResilienceTimed."hedge: max
      bounds the attempts in flight"` failed once in a full gate
      (2026-09-11 00:0x, optics-outside-ops-routes) and did NOT
      reproduce alone: 4 of 4 green on unmodified master and 3 of 3 in
      the lane's worktree, 7 for 7 in isolation. The suite is timed by
      name — `Hedge.run(10, max = 3)` needs three attempts to start
      10 ms apart and the third to win at +5 ms, then asserts exact
      counters (`starts == 3`, `cancelled == 2`) — so it measures
      whether the scheduler kept up, which under a full matrix it
      sometimes does not. Untagged today, which puts a machine-speed
      question in the default gate; the policy in AGENTS.md
      ("no flaky tests in the default gate") says `Live`. Not tagged
      by that lane on purpose: one sighting is a ledger entry, not a
      verdict, and the owner should decide between tagging it and
      making the assertions bound-based rather than exact.
- [ ] **hedge-start-timing-flake** — `TestHedgeStart."an attempt forked
      while the answer arrives leaves neither a running attempt nor an
      armed timer"` fails a landing gate with "timed out waiting for
      the first attempt to answer" when the box is loaded (1-minute
      average ~19, three sbt matrices), and passes alone on the same
      tree seconds later. Same family as `parse-depth-tests-out-of-the-
      gate`, just landed: a wall clock on a shared box. Either the wait
      needs to be a condition rather than a deadline, or the test
      belongs in `integrationTest`. Seen 2026-09-10 by the
      dataflow-numbers gate, which does not touch okay-resilience.
      SECOND SIGHTING 2026-09-17, continuations-audit's gate: same
      test, same message, in a run of 4498 results whose only other
      failure was none — and the lane touches `Delim` and docs, while
      okay-resilience never names `Delim`. 3 of 3 green in isolation
      on the lane's own tree minutes later. Two sightings from two
      lanes that cannot have caused it is no longer one ledger entry:
      the owner's choice between `Live` and bound-based assertions is
      now overdue.
      THIRD SIGHTING 2026-09-17, workflow-suspended-driver's gate —
      same test, same message, another lane that cannot have caused
      it (okay core and okay-persist).
      FOURTH SIGHTING 2026-09-18, gate-bound-test-fanout's gate — same
      test, same message, and the lane changes ONE LINE of build.sbt
      and nothing else. Box at load 76 under a sibling's gate; 3 of 3
      green in isolation on the same tree minutes later. Four lanes,
      none of which can have caused it, is the whole argument: the
      assertion is about the scheduler, not about hedging. Option (a)
      above is the one to take.
      ONE THEORY TESTED AND NOT CONFIRMED, recorded so nobody spends
      the same hour twice: the suite's `until` helper waits by
      spinning on `Thread.yield()` for up to five seconds, and a
      yield-spin BURNS a core rather than waiting — plausibly the core
      the fibre it waits for needs. Changing it to `Thread.sleep(1)`
      is obviously no worse, but the repro DID NOT REPRODUCE: with 12
      CPU burners and a 1-minute load of 22, the ORIGINAL `yield`
      version passed. So the starvation theory is unproven and the
      change was reverted rather than landed on a guess. Whoever picks
      this up: synthetic CPU load is not the shape that breaks it —
      the failures all happened under a full matrix, which is many
      JVMs with many threads and a lot of I/O, not a busy loop.
      FOURTH SIGHTING 2026-09-17, a book lane whose ENTIRE DIFF IS
      PROSE — five markdown files under docs/continuations and not one
      line of executable code. That settles the remaining doubt: no
      lane's code perturbs this, because this lane has no code. What
      the four have in common is only the full matrix, which means the
      test is measuring the box and calling it a behaviour.
      THE DECISION IS OVERDUE AND THE EVIDENCE IS NOW COMPLETE: a
      five-second wall-clock deadline inside a suite that runs beside
      ninety other module runs is not a bound anybody can defend. Move
      it to `integrationTest`, or assert on a CONDITION rather than a
      deadline (`Live` exists for this). Not done here on purpose: a
      documentation lane must not carry a fix to okay-resilience, and
      the owner's choice between the two shapes is a design decision,
      not a patch. Four gates have now paid for it.
- [ ] microservices-next — the audit's remaining gaps, each its own
      spec when picked. DONE 2026-09-09 (service-lifecycle): graceful
      shutdown and RED metrics, both in okay-ops. DONE 2026-09-09
      (outbox): transactional outbox / inbox / dead-letter as
      okay-outbox (specs/outbox.md). DONE 2026-09-09 (discovery):
      service discovery + client-side balancing in okay-resilience
      (specs/discovery.md). DONE 2026-09-09 (schema-compat): Schema
      compatibility between services, `okay.codec.Compat`
      (specs/codecs.md). DONE 2026-09-09 (obs-log): a Log effect with
      trace correlation, `okay.obs.Log` (specs/obs.md, "The third
      leg") — the audit's list is now closed except: saga over `Durable`
      + persist with compensations as values; transactional outbox /
      inbox / dead-letter when the truth is in SQL; service discovery
      + client-side balancing (cluster.md lists it out of scope);
      Schema compatibility checks between services; a `Log` effect
      with trace correlation (0 hits for one today).
      (was filed under "resilience" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## okay-persist
- [ ] the only thing genuinely absent is NUMBERED QUEUES — a ticket
      per waiter, served in order — and the entry filed that as a
      question rather than work. It stays a question: nothing in the
      tree asks for one, and `Channel` already serves waiters in
      order within a partition.
      (was filed under "leases" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## bulk — the loading seam (specs/bulk.md)
- [ ] bulk-plan-next — with a workload that asks: a `Where` whose
      predicate is structural (a column equals a value) pushed under a
      join, and a size estimate for a held table from the count its
      `Cache` already made. Neither is worth a line until something
      measures for it.

- [ ] bulk-parquet — `Bulk.csv` is the only source; the taxi demo
      (TestTaxiAlgebra) still reads its parquet through Spark's API.
      A `source` per format, or `Bulk.read(Format)`, with the local
      instance reading parquet without Spark (okay-delta already
      carries a Delta Kernel road, specs/data.md).

- [ ] bulk-flink — `flink-core` alone carries no DataStream; an
      instance needs flink-streaming-java. The seam's `Any`-element
      choice is what a `DataStream[AnyRef]` instance would do too.
      (2026-09-10: flink-streaming-java and flink-clients are now on
      okay-flink's TEST classpath for the §20 benchmark, so the
      dependency question is answered — a `Bulk` instance would still
      need them in `compile`.)

## okay-actor
- [ ] ~~Say so.~~ Not taken: the operator asked for the actors to
      WORK on JS, which is the second answer.
      (was filed under "actor-on-js" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)

## Refuted, declined or answered — do not retake blind
- **refs-typed-heap — REFUTED 2026-09-11.** Scala 3's generalized
  method syntax (a type clause after a term clause) does NOT remove
  `Refs.handle`'s cast, though its own documented example
  (`def getOrElse(k: Key)[V >: k.Value]`) is that signature to the
  letter. Two roads compiled and refused: a `Ref` with `type Value`
  plus `Map[Ref, Slot]` — `s.value` is `s.ref.Value` and nothing proves
  `s.ref` is `c`; and the heap as a dependent function
  `(c: Ref) => Option[c.Value]`, which expresses the dependence and
  then breaks at `put`, where `k eq c` is a run-time fact the types
  never learn. The rule both share: the feature fixes SIGNATURES, not
  STORAGE — it lets a type depend on a term that is present, and does
  not recover a type erasure has thrown away. Written out beside the
  cast in `Refs.scala`; the probes are in the session scratchpad.
  Where the feature WOULD earn its place is a value with a type MEMBER
  whose dependent type cannot be written today; okay has no such API
  at present (`Ref[S]`, `Fact[V]`, `Tag[K,F,A]` all carry parameters,
  not members).

One line each; the measurement and the reasoning are in
`BACKLOG-ARCHIVE.md` under the same slug.

- writer-test-no-some — REFUTED 2026-09-09, no code landed. Built
  (ClassTag class test in the companion, Typeable fallback by given
  priority) and …
- single-shot-row — PRICED AND REFUTED 2026-09-09: a mutable cell buys
  Writer's reverse and nothing else (-8.9% B/op on the mixed program,
  gate was …
- handler-fusion-eff — DONE 2026-09-09, REFUTED: Eff + composite is
  0.58x of the fused Free loop, 2.4x the bytes; the best tree-free road
  0.86x …
- default-scheduler-shape — REFUTED 2026-09-08 by its own disqualifying
  evidence, and the entry was built on a mismatched pair besides.
  Matched by …
- growing-adopted-part0 — REFUTED 2026-09-08, and there was never a
  defect. The entry rested on a MISMATCHED PAIR, filed by me:
- chunked-lexer-bookkeeping — REFUTED 2026-09-09, nothing landed. The
  per-chunk bookkeeping was rewritten away (one traversal into a
  growable array) …
- lexer-buf-without-concat — REFUTED 2026-09-09, nothing landed. The one
  candidate that needs no input (a doubling char array in the state) is
  WORSE by …
- stm-sync-commit-fastpath — MEASURED AND DECLINED 2026-09-07 (§18e).
  Built as filed, with the first attempt inside a `Run` so nothing runs
  at …
- optics-fast — BUILT, MEASURED, DECLINED 2026-09-10 (operator's call;
  specs/optics.md "optics-fast"). The premise was refuted: compiling an
  optic to …
- sql-plan-cells — MEASURED AND DECLINED 2026-09-07 (taken up on the
  operator\'s word despite its own condition). Compiling each field\'s
  Shape into a …
- intent-live-provider — LANDED 2026-09-03, and it REFUTED the claim it
  set out to quantify: the early stop saves 0.0% against a real model,
  under a …
- intent-gate-non-english — LANDED 2026-09-04, and it REFUTED its own
  premise. Re-measured on domain-bearing names, the gate does not pay in
  any of six …
- channel-chunk-batch-size — REFUTED TWICE 2026-09-06: the consumer
  already batches at 62 of 64 Taken as channel-batch-floor on the
  finding that …
- intent-split-other — MEASURED AND DECLINED 2026-09-05. Carving the bin
  into Social/Support/Errand takes `Other` recall from 46.7% to 6.7%
  (composite …
- intent-label-model — MEASURED AND DECLINED 2026-09-07
  (MeasureLabelModel, offline). Six offline labelers combined by
  agreement-estimated weights …
- intent-offline-other — MEASURED AND DECLINED 2026-09-07
  (TestOfflineGate, offline, no network). The offline analogue of the
  model path's binary gate: …
- channel-per-element-effect-cost — CLOSED as an interpreter lane
  2026-09-06, redirected Taken as free-cont-stack on the hypothesis this
  entry invites: …
- actor-receive-offer-first — MEASURED AND DECLINED 2026-09-06: +19% in
  the regime that matters Built: `receiveNow(): Poll[A]` on `Channel`
  (default …
- source-unfold-tuple — DECLINED by design 2026-09-07 `Source.unfold`
  costs 13% over `Source.range` on its lane (section 6c),
- drain-copy-per-element — DECLINED by design 2026-09-07 `Drain` is a
  case class and `Stream[Drain, Async].uncons` answers
- native-interpreter-allocation — DONE 2026-09-06: the collector is not
  it; the count is six objects per bind §18: `bindChain`, N nested
  flatMaps with …
- adaptive-as-default — decided 2026-09-07: NO, and here is the number
  that would change it `Queues.strong.adaptive` wins many-to-many (0.63
  of our …
- jiffy-hole-scan — MEASURED and DECLINED 2026-09-07: a real latency
  hazard with no measurable throughput cost The operator pointed at
  Jiffy (Adas & …

## json-raw-nesting-jmh-pending (2026-09-10, found by json-raw-nesting-threshold-trampoline)

- [ ] `json-raw-nesting-threshold-trampoline` landed with correctness
      fully proven (184 tests, three platforms, an A/B at 100 000
      levels via a scratch `Codecs.maxDepth`/`NativeThreshold`) but NO
      trustworthy JMH number: system load climbed from ~20 to 88 while
      measuring (a shared box, unrelated to this lane), and
      `parseOnly`/`parseValueOnly` — two benchmarks with IDENTICAL
      bodies — read 282±26 vs 600±237 ns/op in the SAME run, proof the
      box could not answer the question regardless of fork count.
      Re-run `compare/Jmh/run -i 5 -wi 3 -f 5 .*parseOnly.*|.*parseValueOnly.*`
      once `uptime`'s load average is near the core count, and update
      `specs/iterative-recursive-decode.md`'s "Targets 1+2 done" entry
      with the real delta.
