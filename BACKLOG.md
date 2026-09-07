# Backlog

## matrix-kill-by-process-group — SETTLED 2026-09-06, and the name is wrong: it is a PKILL, not a group

The trap fired. `scripts/gate-sentinels.sh` was watching a full matrix
when it died at 1635 tests, and the three sentinels answer the
question this entry has carried since 4 September:

```
GATE EXIT -> 143
  group  ALIVE     -- same process group as the gate, nothing matchable in its argv
  named  DIED      -- its OWN session, "sbt-launch" in its command line
  plain  ALIVE     -- its own session, nothing matchable
```

A process-group kill cannot do that: `group` shares the gate's group
and lived. An indiscriminate sweep cannot do that: `plain` sat beside
`named` in its own session and lived. The only thing that kills a
process in a foreign session because of what its COMMAND LINE says is
a **pkill matching "sbt"**. `named` stopped heartbeating one second
before the gate returned.

The `ps` recorder makes it worse and clearer: **no other sbt was
running on the box at the time**. Only the gate (pgid 69895), its test
JVMs and the sentinels. So this was not a contended box being tidied
by someone who needed it — it was an agent with no sbt of its own
running `pkill -f sbt` to clean up, and taking a sibling's matrix with
it. That is exactly the cause CHANGELOG:8155 recorded as admitted, and
it is NOT the mechanism this entry named.

**So the prescribed work below is void.** `TestTwoNode` already kills
by pid; giving spawned workers their own session with `setsid` would
have changed nothing, because nothing was killing by group. Do not
build it.

THE MECHANISM, IDENTIFIED AND NAMED (late 2026-09-06). The sentinel
run's `ps` recorder caught it two seconds before the build died at
1642 tests: `grep -E 'sbt-launch|xsbt\.boot|sbt\.script|sbt/standalone|
bloop|scala-cli|scalacli|…|org\.openjdk\.jmh'` in a foreign process
group, then a kill of the matches. The adversarial-lanes session traced
the pattern string to its source, and it is NOT an agent and NOT
anyone's pkill: two launchd agents from the operator's scalascript
project. `io.scalascript.build-ram-guard` runs every 20 s and, under
memory pressure (available < 3 GB with pageouts), kills the HEAVIEST
JVM matching the regex — a full matrix crosses that line 60–90 s in,
which is why three matrices died at ~1640 tests at +71, +91 and +56 s
after their starts, and why four others got through. `io.scalascript.
kill-stale-builders` runs hourly with `--idle 30 --kill`: a JMH host
waiting on its fork sits at 0 CPU, counts as idle, and dies; the fork
lives on holding `jmh.lock` — the three orphans found that afternoon,
one alive five hours fifty-eight minutes. Its log,
`~/Library/Logs/kill-stale-builders.log`, lists today's kills by pid
and cwd across okay-wt-floor, okay-wt-advbase and others. `sbt.script`
is in every sbt's arguments, so nothing in this repo can dodge it. So:
not a suite, not a process group, not concurrency, and — the
correction this entry owes the sibling it blamed twice today — not a
neighbour's cleanup either. The fix is in scalascript's scripts (a
builder burning CPU is never the one to kill; a host waiting on a fork
is not idle) or in unloading both agents while okay builds run; the
operator has it.

Second firing, 2026-09-06 evening (channel-batch-floor's gate): the
`named` sentinel died 41s before the gate returned — and the gate
itself LIVED, exit 0, 2438 tests. A `pkill -f sbt` takes both, since
the real sbt's command line carries `sbt-launch` too. So this kill
matched the sentinel and spared the build: a first-match kill
(`kill $(pgrep -f sbt-launch | head -1)` takes the OLDEST pid, which
was the sentinel), or a pattern that fits `sbt-launch-sentinel` and
not `sbt-launch.jar`. Not solved — recorded so the next firing can be
read against it. Three of four sentinel verdicts today were name
kills; the fourth was a two-hour sbt hang at "set current project".

**The fix is a rule, not code, and it is now in AGENTS.md:** never
`pkill -f sbt` / `killall java`. Kill by pid, and check whose the pid
is first. The serialisation this entry imposed — "a green matrix is
obtainable only by running ALONE" — is also unfounded: a full matrix
passed earlier the same day with another agent's sbt alive throughout.

## the original entry — one suite takes down every sbt on the box

Found 2026-09-04 while gating an unrelated four-line change. The full
matrix dies with SIGTERM (exit 143) immediately after
`okay.cluster.TestCluster`, and it is not the change, not memory and
not a flaky test:

- a feature branch died three times at exactly 1427 tests;
- CLEAN MASTER, run from the main checkout with no changes at all,
  died the same way at 1897;
- different counts, the SAME suite boundary every time;
- 53% of 36GB free, 5.7GB of java resident — nothing is being
  OOM-killed, and 143 is a TERM rather than a KILL.

`TestCluster`'s two tests are "a killed in-process worker's chunks
recompute" and "a socket worker dies mid-stream". A kill issued to a
process GROUP rather than to a pid does exactly what is observed: it
reaches every sbt on the machine, including a sibling worktree's
matrix minutes from finishing.

This is the mechanism behind the folklore that two concurrent sbt runs
SIGTERM each other here. It is not contention — it is one suite
killing processes it does not own, and the practical cost is that a
green matrix is currently obtainable only by running ALONE, which for
several agents on one box is a serialisation nobody agreed to.

Second observation, 2026-09-06 (backlog-audit's gate, `clean; test`):
exit 143 again, but at a DIFFERENT boundary — the log ends on
`okay.demo.TestTwoNode`, "two real processes over one shared log: one
leader, the follower serves reads, a kill fails over", which is also a
suite that kills a process it spawned. No sibling sbt was alive when
it died (the next one started 19s later), so this was not contention.
One observation is not the pattern the `TestCluster` finding rests on
— sbt interleaves suite output, so the last line printed need not be
the killer — but if the fix is written against `TestCluster` alone it
may not be enough. Recorded so the next 143 has something to match.

MECHANISM NOT ESTABLISHED — read this before building the fix below
(gate-143-mechanism, 2026-09-06). Two of this entry's own facts point
away from a process-group kill:

- `TestTwoNode` kills with `procA.destroyForcibly()` — that is BY PID
  and it is SIGKILL. The victim gets SIGTERM, which is 143. A by-pid
  SIGKILL does not produce the signature this entry is about.
- `TestCluster` spawns no OS process at all. Its two tests are "a
  killed in-process worker" and "a socket worker dies mid-stream" —
  an in-process kill and a dropped connection. There is no group for
  it to signal.

And there is a competing explanation with an ADMITTED instance:
CHANGELOG:8155 records "three full-matrix runs were SIGTERM-killed
externally (a sibling pkill, admitted in the room)". Same signature,
same box, a cause that is a person rather than a suite.

So the prescribed work below may fix nothing: `destroyForcibly` is
already by pid. Do not write the setsid fix until the mechanism is
established.

`scripts/gate-sentinels.sh` establishes it. Three sentinels heartbeat
beside the gate: `group` shares the gate's process group and carries
nothing matchable; `named` sits in its own session with "sbt-launch"
in its command line; `plain` sits in its own session with neither.
Which ones die names the blast radius — group only means a
process-group kill, `named` means somebody ran a pkill matching "sbt",
all three mean an indiscriminate sweep, none means the gate was
signalled directly. The discriminator is verified: `group` shares the
runner's pgid, the other two get their own via `os.setsid`, and
`pgrep -f sbt-launch` matches `named` alone.

Third run with the trap set, 2026-09-06 (free-cont-stack's gate):
passed again — 2438 tests, 0 warnings, all sentinels alive — and this
one ran with **another agent's sbt alive on the box the whole time**.
That is one observation against the folklore this entry records
("two concurrent sbt runs SIGTERM each other here") and against the
serialisation it imposes. Two concurrent runs are not sufficient to
cause a 143.

First run with the trap set, 2026-09-06: **the matrix passed** — exit
0, 2438 tests, 0 warnings, all three sentinels alive. That is one
clean run, so it neither confirms nor refutes anything; it does show
the instrument does not perturb the gate. The trap wants to be set on
the next 143, which is why the script takes the sbt command as an
argument and why `OKAY_SENTINEL_TAG` exists — narrow the tag and it
stays out of a sibling's `pgrep -f sbt-launch` quiet check.

The work: kill by pid, and if a test genuinely needs to signal a
group, give the spawned worker a group of its own
(`setsid`/`ProcessBuilder` with its own session) so the blast radius
stops at the thing under test. Wants a law that a worker-killing test
leaves other JVMs alone — hard to assert directly, but a sentinel
child process that must still be alive at the end of the suite would
catch a regression.

## Benchmarks — after kyo-fair-lanes (2026-09-02, docs/benchmarks.md §2/§5/§7)
- [x] test-login-tamper-flake — `TestLogin`'s "a tampered token is
      refused" builds its tamper as `token.dropRight(2) + "xx"`, which
      is the SAME token whenever the JWT happens to end in `xx`. Base64
      url alphabet, so roughly 1 in 4096 runs fails a merge gate for
      nobody's mistake. FOUND 2026-09-03 during http-peer-address's
      gate; reproduced by running the suite alone (3/3 clean), so the
      failure is the data and not the code. Fix: tamper by flipping a
      character to one it is not, e.g. `init :+ (if last == 'x' then
      'y' else 'x')`.
      LANDED 2026-09-03 — and the suggested recipe above was ITSELF
      flawed: flipping the LAST char flaked worse (~40% of runs, not
      1-in-4096), because a 64-byte ES256 signature's base64url tail
      char carries only 2 significant bits (4 are decoder-ignored
      padding) — many flips there decode to the SAME bytes and still
      verify. Fixed by flipping a MIDDLE character instead (always
      inside a fully-significant 6-bit block); 0/50 stress runs clean.

- [x] okay-script-scalac-classpath — `okayScript/test` fails 5/7 on
      master itself, unrelated to any branch: `summonFrom` not found in
      `scala.compiletime`, and `NoSymbol cannot be cast to ClassSymbol`
      in dotc's Namer/Typer. Reproduces identically on master and on a
      fresh worktree, so it is an environment/toolchain break (a JDK or
      dotty version drift most likely), not a code regression. FOUND
      2026-09-03 while gating json-unicode-escape; that claim's own
      suites (okayCodecJVM, okayHttp*, okayMatch*, TestTelegram-style
      consumers) were unaffected and green.
      LANDED 2026-09-03 (okay-script-runtime) — not a toolchain drift:
      `okay-script`'s own `build.sbt` block never set `Test / fork :=
      true` (every other project in this build does), so its tests ran
      INSIDE SBT'S OWN JVM. `ScalaScript.run` built the compile
      classpath from `System.getProperty("java.class.path")`, and in
      that un-forked JVM that property is just `sbt-launch.jar`'s own
      path — sbt manages its real classpath through its own layered
      classloaders, invisible to that property — so dotc compiled
      against a classpath with no scala-library on it at all and
      crashed resolving `scala.Int`. Confirmed by printing the
      property from inside the failing test JVM. Fixed by adding
      `Test / fork := true`; `okayScript/test` went 5 failures -> 0
      before any further change. specs/okay-script.md.

- [x] chunked-source-sweep — DONE 2026-09-07: `zioStreamRange`,
      `fs2StreamEmits` (pure) and `okayStaged` joined
      `StreamOpsBenchmark`, all thirteen lanes ran in one session with
      `-prof gc`, and §5 is that one table — floor 13.96, Staged 1.55
      (0.11x), Okay chunked 9.52 (0.68x), Okay elements 22.5, fs2 emits
      21.5, ZStream.range 31.4, kyo range 60.8, the per-element rows
      243 / 618 / 1364. The old caveat's ratios held. (fs2's chunked
      spelling is `emits`, not `range`: its `range` is a singleton
      chunk per element, fs2-chunked-merge-lanes.) Original: one
      same-session StreamOps run with every library's CHUNKED source
      next to the per-element lanes; today only kyo's chunked lane
      exists and the §5 table mixes sessions with a ratio-to-floor
      caveat.
- [x] shape-check-new-lanes — DONE 2026-09-07: a rule, now written
      where a lane's author reads it — docs/benchmarks.md "Lane rules"
      under "Where the numbers are honest about limits", and AGENTS.md
      beside the Jmh warnings policy — with `ReaderBenchmark`'s
      left-nested / right-nested pair as the worked example and the
      pairing rule (granularity, memoisation, source) beside it.
      Original: every new competitor lane built by
      foldLeft gets a right-nested twin before its number is quoted
      (the kyo Env/Emit/Resource lesson: the foldLeft shape is O(N²) in
      kyo, ~1000x, and read as the library's price for a week).

## Casts — the audit of 2026-09-02 (operator's rule: no cast without a real necessity)
185 `asInstanceOf` + 28 non-`resume` `@unchecked` in src/main, in five
groups; the recipe for the first two is the one that made Stm.scala
cast-free (stm-typed-interpreter: `perform[X](op: Op[X]): X`, GADT
matching on `Bind(Effect(e), k)`, typed helper classes, a decision at
construction instead of a type test per value).
- [x-landed] cast-free-condition — landed: ops carry their answer
      type, the policy's Any crosses one checked door (accept), the
      run loop is GADT-typed; one stated claim left (a Within's body
      re-typed in the machine's row). See specs/condition.md.
- [x-landed] cast-free-delim — landed: a typed chain Segs[F, A, Z],
      the cut at a prompt through Same[Prompt]'s witness, Next(prog,
      kont) between steps; two stated claims left (Push.body,
      Capture.f). TestDelim unchanged.
- [x-landed] cast-free-sim — landed: Chan[A]/Send[A]/Receive[A]/
      Close[A], the wait queues typed on the channel itself,
      perform[Y] by GADT; zero casts; traces unchanged by seed.
- [x-landed] cast-free-effects — landed (casts-encapsulated):
      Handler.union splits through `<|>` (the one claim), translate's
      cont typed by the Bind node; typeableK's class-test kernel
      stays, stated.
- [x-landed] cast-free-codec — Json (9 → 0), Cbor (9 → 0): Schema
      WAS a GADT already; the codecs cast out of habit. Two kernels
      in Schema state the Mirror's erasure once (`eachField`: parts
      is productIterator in field order; `theCase`: caseOf is the
      ordinal), sum cases are `Schema[? <: A]`, and the codecs are
      written by GADT matching.
- [x-landed] cast-free-typed — landed: Shape[A] is a GADT (Prim
      carries its typed decode/encode with the column widenings,
      Opt/Iso/Arr carry their types, Row carries its Schema), decode
      and encode by matching, a row encodes through eachField; 11 → 0.
- [x-landed] typed-js-facades — landed: okay.Web (core scala-js)
      states fetch/Response/Headers/the body reader/WebSocket and its
      events as js.native facades; both transports rewritten on them,
      17 casts → 0 (a text-vs-binary frame is a type TEST on `Any`).
- [x-landed] casts-encapsulated — landed: ChunkBuf's array kernel
      is one `wrap` (7 → 2, in it) and `sized` replaced the Vector
      casts; Eager's encoding dispatch is one `fold` (6 → 2, in it);
      Pipe.unreachable throws instead of handing out a null; Same's
      two witnesses stay as the axioms.
- [x-landed] direct-upcast-ascription — landed: the macro summons
      `V <:< T` at expansion time and splices it (`upcast`), so the
      generated code carries the compiler's evidence, not a cast;
      found on the way: one of the four was NOT an upcast — a
      statement-position loop's `F[Any]` cast to Unit — now an
      explicit discard `(_: V) => ()`, Scala's own value-discard rule
      said in the macro.
- [x-landed] unchecked-audit — landed: the five `case c: Chunk[Byte]
      @unchecked` over a `Chunk[Byte] | Null` scrutinee are
      null-first matches (flow typing types `c`). The rest are the
      stated kernels: Chunks/Writer's Fold specialization dispatch
      (8: a runtime class test on the Fold instance, the type
      argument erased — commented at Chunks) and Throws' union
      dispatch (12: `A | E | Either | Try` told apart at runtime,
      commented at Throws). Casts in src/main: 185 → 97; what is
      left is kernels with their reason (ChunkBuf, Eager, Pipe,
      Same, Schema, Effects), JVM interop (blob S3/Offload/Backup,
      java Streams, CryptoJvm, kyo) and small ones in ui/rag — the
      next audit's list.

## Casts, round two (2026-09-02, after the audit's 185 → 97)
- [x] casts-recount — RE-COUNTED 2026-09-07, after a day of core, channel,
      STM, intent and frame lanes: 33 `asInstanceOf` in src/main across
      the modules (code lines; the round-two close said 36 and counted
      comments that mention the word too) and 27 non-`resume`
      `@unchecked`. Every site is one of the argued ones — `Same`'s
      `=:=` witness, the erased `Chunk` array (`Chunks`, `ChunkBuf`),
      `SentinelChannel`'s one cast with its argument, `Pipe`'s
      `resumeWith`/`reinject`, `Effects`' `unapply` and the F-or-G
      split, `Writer`'s `Say`, `Generate.produced`, `Delim`'s two,
      `Agent.stateAs`, `Schema`'s kernel, `Frame.valueOf` by identity,
      `Http`'s `ArraySeq` narrowing, the Java and JS interop in
      okay-java / okay-crypto. Nothing crept in; the day's new code
      (`Ring`'s single-consumer pop, `Stm`'s log, `Reading.grounded`,
      `Temporal`/`Duration`/`People` lexicons, `Slot.lookup`) has none.
- [x-landed] cast-free-agent — Provider/Grounded/Handlers/Memory/
      Large/Durable/ToolSpec (10 → 1): interpreters built at the
      GADT-bound X (a covariant row gives X >: the answer, `!` is
      invariant, so `pure[F, X]` / `map[X]`), a Tool[String] asked
      as such answers a String, defaults through a Schema kernel
      (`defaultAt`), the snapshot's erased state through ONE kernel
      (`Snapshot.stateAs`, the Context row names no S).
- [x-landed] cast-free-blob — landed: the Backup/Offload walkers are
      typed by the tree (an Async[X] or a produced X — Produce is the
      identity signature, the op IS its answer; that the values are
      chunks is `produced`'s one claim), S3's row re-associations are
      ascriptions (a row is a union); 17 → 0.
- [x-landed] cast-free-rag-llm-kyo — landed: rag's rows by
      ascription, `fair` built in Choose + Pure, Cut's "cut" frame
      typed through `frame[…, Violation]` (the ClassTag door), kyo's
      Throws matched at its E and the continuations uncast (kyo's
      types line up); 10 → 0.
- [x-landed] cast-free-small — landed: Rx's queue is a typed message
      ADT, Async's handshake cell is `Got[X] | Moved | Null`, Native's
      placeholder an Option, the Java API downcasts are type tests
      with a named refusal (Nio, Jetty, Netty, Tls; CryptoJvm through
      privateKeyOf/publicKeyOf), Node facades for process.argv and the
      Buffer callbacks (Web.Process, NetNode, both CryptoJs — the
      require-based one keeps ONE claim at the module boundary),
      Form decodes fields at their type, Screen finds a boundary by
      Same's witness, Collect always calls the finisher, jdbc/r2dbc
      walk any array by the runtime. Casts in src/main: 97 → 36.
      Left in this group: Dom.scala's js.Dynamic (a ui-js facade
      lane; decided for now — the backend takes a real document or a
      test's fake, and js.Dynamic is what fits both). Screen's
      `Nav | S` split is SOUND since tidy-warnings-screen-dom: a
      `NotGiven[S <:< Nav]` evidence refuses an S that is a Nav at
      compile time, so the runtime test on Nav decides the union.
- kernels that stay, each with its reason at its line: Same (2),
  Eager (2), Pipe (2), Condition (1), Delim (2), Schema (5), Effects
  (2), Writer (1), Http (1), Chunks (2), ChunkBuf (1), Generate (1),
  java Streams (5, array specialization).

## STM — after stm (2026-09-02, specs/stm.md)
- [x] stm-js-direct-bench — DONE 2026-09-07 (§18d, specs/stm.md
      Results): `BenchStmCross` on three platforms + `StmBenchmark`
      under JMH. A one-`Modify` transaction is the same handler
      twice (the structural fast path; JMH identical to the byte,
      9 ns / 55 B per transaction). On a Read-then-Write `direct` is
      ahead of `tl2` by 23% on Node and 19% on Native; the JVM pair
      is inside its noise. The cost is the transaction (~800 B,
      70–90 ns JVM / ~300 ns Node / ~1.5 us Native), not the handler.
      Original: the direct handler is the JS given by construction;
      price it against tl2 on Node once a JS benchmark harness exists.
- [x] stm-sync-commit-fastpath — MEASURED AND DECLINED 2026-09-07
      (§18e). Built as filed, with the first attempt inside a `Run`
      so nothing runs at construction: `async(tryNow(tx)).flatMap {
      Right(a) => pure(a); Left(log) => Async.await(park…) }` in
      both handlers; 17 STM tests green. JMH `-prof gc`:
      `directReadWrite` 398 → 451 us (+13%) and 3.64 → 4.25 MB/op
      (+152 B per transaction), `tl2ReadWrite` 298 → 318 (+6%),
      +124 B; JS flat, Native −3–6% inside its bars. The `Run` +
      thunk + `Bind` + closure + `Either` cost MORE than the `Await`'s
      registration closure, exchange cell and `Got` — which the
      Drive's synchronous-answer path handles in one CAS and a
      `getAndSet`, evidently well. Reverted in full; the lanes stay.
      What the log costs (~800 B per Read-then-Write) is untouched
      by this and is the number, if anyone wants it: the persistent
      write map updated per write, the `(TRef, Long)` tuple per read,
      the installed cell. Original: an attempt that COMMITS never
      parks, yet every non-fast-path `atomically` is staged as an
      `Async.await`; run the attempt first, answer `pure(a)` on
      commit, reserve the `Await` for `RetryNow`.
- [x] stm-log-cost — DONE 2026-09-07 (§18f): the read set as two
      parallel arrays (−55/−63 B per transaction, time flat) and the
      commit as one typed walk of the write set — no reversed copy,
      no iterators, no lookup per install — `directReadWrite` 398 →
      167.5 us (−58%), `tl2ReadWrite` 298 → 229.3 (−23%); a
      Read-then-Write is 35 ns / 439 B (`direct`), 51 ns / 695 B
      (`tl2`) over the bind now. Not taken: the small-array write set
      (the `TMap` walk is no longer the cost), the log reuse across
      retries (a retry is rare and parks). Original: a Read-then-Write
      transaction allocates ~800 B
      and costs 70–90 ns on the JVM, ~300 ns on Node, ~1.5 us on
      Native (§18d), in the `Log`: a `TMap` write set rebuilt with
      `updated` per write, a `(TRef, Long)` tuple with a boxed
      version per read in an `ArrayBuffer`, the `Slot` installed per
      written cell, `written`'s iterator and closures at commit.
      Candidates, measured one at a time on `StmBenchmark
      *ReadWrite` (`-prof gc`) with the `Modify` lanes as control: a
      small-array write set for the one-to-few-refs case (the map for
      more), reads kept as two parallel arrays (no tuple, no box), the
      log reused across an attempt's retries. Laws: every TestStm* on
      every platform; `orElse`'s branch logs (`parent`, `absorb`)
      must keep their semantics.

## Async — after channel-callback (2026-09-02)
- [x] native-scheduler-pool — the Native Scheduler forks one OS
      thread per fiber (src/main/scala-native/Platform.scala). With
      the callback channel nobody waits in a thread anymore, so a
      fixed-size pool with a task queue (the JVM's Schedulers.pool
      shape) is safe: fibers become cheap on Native. Blocking forms
      (CanBlock) on a pool thread still park it — document, or size
      the pool for it.
      LANDED 2026-09-03: Schedulers.pool(size), hand-rolled queue (no
      java.util.concurrent assumed on Native's javalib);
      Schedulers.threads keeps today's behavior and stays the
      DEFAULT — a blocking workload on a shared pool can starve it,
      so pool is opt-in, sized per workload. cancel() best-effort,
      tracked per-task so a stale cancel never hits a later task on
      the same worker.

## Direct style — the roads named by the 2026-09-01 survey (docs/direct-style.md)
- [x-landed] direct-try-ctx — `try` inside direct[[X] =>> E ?=> X] (the
      Reader-elimination monad) CRASHES dotty 3.7.4 at erasure
      ("bad adapt for M$proxy2.pure(a)") when a CanTry instance for
      context functions exists (found by the 2026-09-02 audit;
      the instance was withheld so the case is a clean "no CanTry"
      compile error instead). Minimize, report upstream, or emit
      the try's pure branch differently for context-function F.
      LANDED 2026-09-03: the crashing shape reused CanTry.strict
      verbatim, which was also the WRONG semantics (a context
      function is a closure — constructing it never runs the body,
      only applying it does, so a strict try never sees a throw
      from inside). `ctxFn` defers the try to APPLICATION time
      instead (the honest counterpart to the Free row's per-step
      guard) — different generated code, no crash, no version bump.
- [x-landed] condition-typed-reconcile — landed in audit-fixes: Of[A]
      derives the Answers instance (Answers.fromOf), so the two typed
      doors are one door with two spellings (specs/condition.md,
      Typed signals, Reconciled). The two frame overloads verified
      complementary: Restart[V]-capability body + typed recover vs
      the inline direct body + Any recover; unifying them would need
      the macro to accept a beta-redex body — not worth a road.

- [x-resolved] direct-choice-ambiguity — resolved in practice by
      ui-direct's landing: explicit direct[[A] =>> A ! AgentRow]
      with .reflect compiles (ChatDemo.agentTurn on master); Choice
      documents the Monad/MonadPlus overlap in its header. Reopen
      only if the inference form (no type argument) bites a consumer.

## Flakes observed (record → fix loop when they recur)

- [ ] native-runner-137 — the Native test binary
      (`.native/target/scala-3.7.4/okay-test`) "finished with non-zero
      value 137" in a full matrix (2026-09-07 01:25, the
      frame-language-tag-fallback gate): the sbt-scala-native runner
      logged `Force close java.net.SocketTimeoutException: Accept timed
      out` and then "Test runner interrupted by fatal signal 9", and
      `okay.TestChannelFailureCross` was the suite in flight. Neither
      launchd guard killed anything (build-ram-guard.log: `killed=0`
      every tick, okay worktrees `SPARE`d; kill-stale-builders' last
      kill 19:22 the day before). The box was paging through the whole
      run (pageouts 240–590/tick, "available" 8–10 GB, a sibling's
      idle 601 MB sbt beside the gate), so the reading is the
      adapter's own accept timeout under paging, followed by its
      SIGKILL of the binary it could not reach. The timeout is a
      constant: `scala.scalanative.testinterface.ComRunner` calls
      `ServerSocket.setSoTimeout(40000)` (test-runner 0.5.12,
      bytecode), so a Native test binary that takes more than 40 s to
      start and connect — on a paging box, with the gate's own JVMs
      beside it — is force-closed by its own runner. Nothing in
      `nativeConfig` reaches it. Until upstream makes it a setting, a
      137 with `Accept timed out` above it is the box, not the tree —
      re-run, and keep the box quiet (one sbt) for the Native leg.
- [x] chat-demo-sessions-flake — CLOSED 2026-09-07: the nine
      `withServer` tests of `TestChatDemo` are `portTest` now (the
      `Live` tag through a per-test helper, as `liveTest` already
      was); the one test that never opens a port stays in the gate.
      Original: `okay.demo.TestChatDemo` "demo-sessions:
      a verified session is the identity of record" failed once in the
      full matrix (2026-09-07, intent-typo-robustness' second gate, load
      ~6): `java.io.IOException: HTTP/1.1 header parser received no
      bytes` after 30.1 s, from `postJson` against a real Jetty port
      (`withServer`, `Jetty.serve(0)`), the request thread's EOF. It
      passed in the same lane's first gate forty minutes earlier and in
      every gate of the day before it, and 3/3 in isolation right
      after — the port/readiness family (flaky-port-roulette), not the
      intent change beside it. Per the standing rule, the demo-sessions
      tests over a real socket move to `Live` (`sbt integrationTest`);
      the scripted-chat tests that never open a port stay in the gate.
- [x] script-temp-snapshot-crosstalk — FIXED 2026-09-05 (same defect as
      the since-removed `script-temp-tests-watch-a-shared-directory`
      entry, filed separately and merged here): `TestScalaScript`'s
      "run: leaves no temp file/directory behind" and `TestPage`'s
      "close() deletes the cached compiled program's temp output
      directory" both snapshotted the SHARED `java.io.tmpdir` for
      `okay-script-*` entries before/after, so either failed whenever
      another concurrent suite (a sibling worktree's own okay-script
      tests, in a parallel matrix) created a matching entry between
      the two snapshots — nothing to do with either test's own
      cleanup. `ScalaScript` now takes an explicit `tempRoot: Path`
      (threaded through `run`/`render`/`compileRender`/`check`/
      `compileOnly` and `Page`), defaulting to the old shared lookup;
      both tests now point it at a private directory and snapshot
      that instead. `TestScalaScript`'s test is back in the default
      gate (dropped the `Live` tag). See specs/okay-script.md
      "okay-script-tests-watch-a-shared-directory".
- [x] demo-live-judgment-flake — FIXED 2026-09-02: `judged` retries
      the whole turn once before asserting in LIVE UNGATED and LIVE
      SEEKER (stochastic judgment — one retry is a quadratic flake
      cut; a consistent failure still fails). 15/15 with the retry.
- [x] persist-election-replicated-flake — SETTLED by exclusion
      (flakes-integration, 2026-09-03, OPERATOR CALL). History:
      okay.persist.TestElectionReplicated errored at suite level on
      one platform under the full matrix (2026-09-01, Errors 1 with 0
      failures; JVM tests of the same suite green in the same run);
      the second platform run printed the header with no tests.
      TRIAGED 2026-09-01: alone on JS (3/3) and Native (3/3) — did
      NOT reproduce. The suite is pure and deterministic (MemoryStore
      + Election + Replicated, a manual clock, no threads, no IO), so
      what failed was the RUNNER under parallel matrix load, the same
      family as the Native-SIGKILL-under-load incidents, not a code
      defect. Now `Live`-tagged with the rest of the recorded flake
      family and run by `sbt integrationTest`. Noted honestly at the
      suite and in specs/integration-test-gate.md: this one is
      excluded by DECISION, not by evidence against the suite — it is
      the only member of the family that touches nothing outside the
      JVM. Re-triage only if it recurs with a non-environmental
      signature, where it will now show up in the integration run.
- [x] mcp-auth-matrix-flake — SETTLED by exclusion (nio-port-scope
      tagged the suite, 2026-09-03; closed here with the rest of the
      family, flakes-integration). okay.security.TestMcpAuth "the
      metadata documents are servable without any token" failed once
      under the full matrix (2026-09-02, `java.io.IOException:
      HTTP/1.1 header parser received no bytes` — the client read an
      empty reply from a server the suite had just started); ran
      alone right after: 4/4 green. It BINDS A REAL PORT, so its red
      can be the machine's rather than the code's: `Live`-tagged, out
      of the default gate, run by `sbt integrationTest`.
- [x] chunk-size-representation — SETTLED, and the premise was wrong
      (2026-09-03). The suspected cause (Vector-then-ArraySeq per
      chunk) was tried: filling a `ChunkBuf` in `Stage.chunked`
      measured 11% better at chunk 256 and 8% at 1024 while 2.2%
      WORSE at the default 16, bars non-overlapping — helping only
      sizes nobody uses. Declined and reverted. The real reading is
      that the curve was never a chunking defect: it compared our
      per-element `Source`, chunked after the fact, against a stream
      chunked by construction. Against the like-for-like pair — okay
      `Chunks.merge` 23.2us against ZIO `ZStream.merge` 58.6 on
      2x2000 — okay is 2.5x AHEAD. docs/benchmarks.md §6b.

- [x] nio-port-scope-flake — SETTLED, and the timing was not what the
      name says (nio-port-scope, 2026-09-03). The assertion took the
      ephemeral port its listener had been given, closed the scope,
      and required a connect to it to FAIL. Under the full matrix that
      is not a fact about our Resource: the port returns to the
      ephemeral pool the instant we release it, a sibling suite binds
      it, and the connect reaches THEIR listener and succeeds — so the
      test reported "the listener outlived its Resource scope" about a
      listener that closed exactly on time. The claim is about the
      listener, so it is now asked of the listener: `Nio.listen`'s
      resource value IS the ServerSocketChannel, and `isOpen` answers
      deterministically, with no port and no neighbours in it. Every
      suite that BINDS a real port (14 of them, found by survey rather
      than by waiting for each to flake) is also Live-tagged now.

- [x] channel-impls — CLOSED 2026-09-07 (channel-entries-audit): every
      bullet below landed under another name, and the harness it asks
      for exists. RingChannel → `SentinelChannel` (d7c69167, the ring
      plus a mark in the FIFO stream), the default `Channel.apply`,
      and since receive-blocking-path-length its head moves by a store
      for a single consumer (§17g: 152 us elementwise against
      `StmChannel`'s 250). UNBOUNDED → `Segments` behind
      `Queues.strong[A].unbounded` (channel-ring-unbounded). Relaxed /
      MultiFifo → `AdaptiveFifo` behind `Queues.relaxed` / `adaptive`
      (cb51748c), measured at sixteen producers, order between
      producers documented as the price. The comparison harness,
      parameterised over the implementation: `ChannelGuaranteeBenchmark`
      (a lane per mechanism at both granularities, zio beside them)
      and `TestChannelLaws` (an `impls` table every mechanism must
      answer for). Original:
      Implementations behind the `Channel` seam
      (channel-seam landed the interface; `StmChannel` is the default
      and unchanged). Each is its own lane, each measured against the
      others AND against `zio.Queue` on the same harness, because the
      point of the seam is that none of them is simply better:

      * RingChannel (bounded, mutable) — `Ring` is already landed and
        measured at 3.4x the rebuild model. Needs the claim-not-remove
        waiter protocol from channel-ring-integration; that lane's
        diagnosis is the starting point.
      * an UNBOUNDED implementation, for the capacity
        `Channel.merge` actually defaults to — see
        channel-ring-unbounded, which is the same work: linked ring
        SEGMENTS. Not a separate Michael-Scott lane, because a
        segmented ring dominates MS on every axis (allocation
        amortised over a segment rather than a node per element,
        cache-friendly, no per-element node) and is the Segmented
        Queue construction Koch-Sanders-Williams 2025 SS3 surveys.
        How it differs from `StmChannel`, so the lane need not
        re-derive it: `StmChannel` costs THREE allocations per send
        (a cons cell, a `Queue`, a `State`) and CASes the whole
        six-field state, so a concurrent operation on an unrelated
        field forces the entire transition to re-run — measured at
        28-49% CAS-failure rates in channel-cas-contention. A
        segmented ring allocates once per segment and contends only
        on the head and tail positions.
      * RelaxedChannel / MultiFifoChannel — Koch, Sanders & Williams
        (arXiv:2507.22764). An order of magnitude at p=32..192, at
        the price of bounded rank error, so ONLY for a channel whose
        consumer accepts relaxed order — not for `merge`, whose
        per-source order `TestChunkEdges` asserts. See
        channel-multififo-many-producers for when it applies.

      The comparison harness belongs to the first of these to land,
      parameterised over the implementation rather than written per
      lane.

- [x] raft-wire-election-flake — CLOSED 2026-09-07 (raft-wire-flake): Live-tagged, budgets and a retrying cluster; see its own entry below. Original: okay.persist.TestRaftWire "killing
      the leader: the survivors elect a new one and keep committing"
      failed once under the full sbt matrix (2026-09-03, one gate in
      three), green 3/3 in isolation immediately after. Leader
      election is timeout-driven, so this is the netty-ws-matrix-flake
      / nio-port-scope-flake family: a schedule-sensitive assertion
      under matrix load. Established as NOT caused by the lane that
      observed it (ring-channel): that lane's only edit to existing
      code is an added `Ring.isFull`, everything else is new files
      nothing references, and the Channel factory still returns
      StmChannel -- there is no code path from it to Raft. Settle by
      the survey in AGENTS.md (does it bind ports / depend on
      timeouts?) and either tag it Live or fix the timing.
      SECOND OBSERVATION 2026-09-06 (channel-elementwise-wakeups'
      gate): same test, same assertion -- "the survivors did not
      commit after failover: Vector((1,1), (2,1))" -- under matrix
      load 25-30, then green 3/3 in isolation at load 12-13. The same
      signature as 3 September. Not the lane that saw it either: its
      only edit is `filled` from AtomicBoolean to @volatile in
      CanBlock, and this flake is three days OLDER than that edit.
      Two sightings, both load-shaped, both green in isolation -- that
      is enough to stop surveying and act: tag it Live, or give the
      failover assertion a budget that survives a loaded box.

- [x] channel-impls-correctness — CLOSED 2026-09-07 (board-hygiene): not a flake, and answered by the channel rewrite — `RingChannel` and `CasChannel` no longer exist; the ring channel is `SentinelChannel` (d7c69167), judged by `TestChannelLaws` as this entry asked. Original: bring RingChannel and CasChannel
      back, now that channel-laws exists to judge them. They were
      written and measured (casChannel 143.9 +/-16.3 against
      stmChannelUnbounded 187.7 +/-18.2 and zio.Queue 122.2 +/-9.7;
      ringChannel 249.9 +/-31.2 against stmChannel 418.2 +/-95.3) and
      withdrawn because their accounting failed one full gate in
      three. The code is in 83ff8b23.

      What changed: `TestChannelLaws` now states the contract and is
      parameterised over the implementation, and it was PROVEN to
      catch this class of defect -- CasChannel with its in-flight fix
      reverted fails law 1 in 0.05s, naming the law, where the full
      gate needed roughly three runs to show the same thing. And
      `Channel.finished` now asks implementations for the conclusion
      ("nothing further can ever be delivered") rather than letting
      them derive it from a raw flag plus an emptiness check, which
      is the derivation three of the four defects got wrong.

      So: add each implementation to `impls` in TestChannelLaws, make
      the laws pass, and only then land. A fourth defect remains
      undiagnosed in CasChannel -- suspect a double-invocation of the
      continuation (CompletableFuture.complete silently drops the
      second value, which looks exactly like a lost element) or a
      waiter dropped by wakeOne's CAS-and-claim.

- [x] ring-channel-waiters — CLOSED 2026-09-07 (channel-entries-audit)
      by two counts that refute its premise. The premise: the waiter
      protocol eats more than half the ring's win. The counts: on the
      elementwise saturated regime the consumer never parks and the
      producer parks 12–20 times per 4000 elements (§17f, a probe on
      `okaySentinelElem`), so no waiter code is on the per-element
      path; and the per-element cost that WAS there was the head CAS
      of the pop, a third of the consumer by JFR, taken for a single
      consumer by a store (§17g, 203 → 152 us). `SentinelChannel`
      reads ahead of `StmChannel` on every lane today. What the entry
      names is still literally in the code — a `Waiter` per park
      attempt, `wakeOne` walking a `List` with `last`/`init` — and
      matters only in a regime that parks at scale; the sixteen-
      producer regime already has its answer in per-part waiter
      queues (channel-per-part-waiters, one useful wakeup in k). No
      lane on the board measures a cost there, so nothing stays open.
      Original: (after channel-impls-correctness) the
      ring's waiter protocol measures 1.7x over the bounded default where
      the RING MECHANISM alone measured 3.4x (channel-ring). The
      waiter protocol around it eats more than half the win, and the
      causes are known rather than suspected: the waiter queue is an
      `AtomicReference[List]` walked with `last`/`init` (O(n) per
      wake), a fresh `Waiter` is allocated on every retry iteration
      and purged afterwards, and the close barrier spins. Fix by
      giving the waiters their own lock-free queue (CasChannel's node
      code is right there) and by not re-registering per iteration.
      Measure against the 3.4x mechanism ceiling, not against the
      default.

- [x] channel-ring-integration — SUPERSEDED by channel-seam plus
      ring-channel: the implementation lives behind the interface
      instead of replacing Channel's mechanism in place, which is why
      three real defects could be found without master ever being at
      risk. Original
      diagnosis kept below for the record.

- [x] channel-ring-integration (original) — wire `Ring` into `Channel`. The ring
      itself is landed, tested (MPMC and SPSC on real threads) and
      measured at 3.4x the rebuild-per-operation model it replaces
      (channel-ring), which matches the 3.6x gap to `zio.Queue`. The
      integration was ATTEMPTED in that lane and reverted, with the
      bug found and understood rather than left as "it hung":

      THE BUG — the take-do-put-back window. The first protocol had
      `deliverToReceiver` temporarily REMOVE a waiter from the queue
      to try a `pop`, putting it back if the ring turned out empty.
      A producer pushing during that window sees an empty waiter
      queue, wakes nobody, and the element sits in the ring with the
      receiver parked forever. Reproduced deterministically by the
      existing `TestChannel` "producer/consumer/close accounting"
      test (capacity 4, 200 rounds); the virtual-thread dump shows
      the consumer parked in `receiveBlocking` on a callback that is
      never invoked. `admitOneSender` has the identical hole.

      THE FIX — claim, do not remove. Keep the waiter in the queue
      and give it a one-shot `AtomicBoolean`; a deliverer takes the
      ELEMENT first, then CASes a waiter's claim, retrying the next
      if that one was already claimed. The element is in hand the
      whole time, so nothing can be stranded. Needs the waiter
      representation changed on both sides (`receivers` and
      `senders`), which is why it is its own lane rather than a
      patch. Also still to settle there: `close` must hand the end to
      claimed-but-undelivered waiters, and `receiveManyRing`'s batch
      must admit parked senders without reopening the same window.

- [x] channel-ring-unbounded — CLOSED 2026-09-07 (board-hygiene): not a flake; the unbounded ring is `Segments` behind `Channel.apply` above `MaxRing` (relaxed-queues-builder, cb51748c). Original: channel-ring gives the allocation-free
      fast path to BOUNDED channels only (a ring is a fixed array).
      `Channel.merge`'s own default capacity is `Int.MaxValue`, so it
      does NOT get the fast path; `Source.merge` (64) and
      `Channel.buffer(n)` do. Options, in the order they look
      sensible: linked ring SEGMENTS (covers everything, the
      Segmented-Queue shape from Afek et al. that Koch-Sanders-
      Williams 2025 SS3 surveys); or change `Channel.merge`'s default
      to bounded (an API decision, not a performance one); or leave
      unbounded on the rebuild path permanently. Settle by measuring
      whether the unbounded path matters in practice first — nothing
      in the library defaults to it except `Channel.merge` itself.

- [x] channel-multififo-many-producers — CLOSED 2026-09-07 (board-hygiene): not a flake; `MultiFifo` was folded into `AdaptiveFifo` behind `Queues.relaxed`/`adaptive` (cb51748c), and channel-per-part-waiters measured it at sixteen producers. Original: if a channel ever has MANY
      producer fibers (work distribution to p workers), head/tail
      contention becomes the bottleneck the ring does not solve, and
      the known answer is relaxed multi-subqueue FIFO (MultiFIFO /
      BlockFIFO, Koch, Sanders & Williams, "BlockFIFO & MultiFIFO:
      Scalable Relaxed Queues", arXiv:2507.22764, an order of
      magnitude at p=32..192). NOT applicable today and deliberately
      not taken: their gain needs many threads (at p=1-2 all designs
      are within a small factor, their Fig. 6.2) and it costs RELAXED
      ordering — elements come out with bounded rank error — while
      `Channel` promises FIFO and TestChunkEdges asserts each
      source's own order survives a merge. Revisit only if a
      many-producer channel appears AND its consumer can accept
      relaxed order.

- [x] netty-ws-matrix-flake — SETTLED by moving it out of the gate
      (netty-integration, 2026-09-03, operator decision). It failed
      the default gate a second time with the identical signature
      (jetty StaticException: Closed, one in 12) and was green in
      isolation immediately after, both times — which is the evidence
      the settle-plan asked for, pointing at load/port timing rather
      than code. okay-netty's suites are now Live-tagged and run under
      `sbt integrationTest`, per AGENTS.md's no-flaky-in-the-default-
      gate policy. Investigating the timing itself remains open, but
      no longer at the cost of every landing's gate.

## Task-oriented dialogue: the literature the operator brought (2026-09-05)

Three papers, read together on 2026-09-05. Their common answer to
"a new task must not need retraining" is to put the schema in the
INPUT rather than in the weights — which is structurally where we
already are (`Schema[I]` derives the tool declaration AND the
decoder, specs/intent-classify.md). What follows is only what is NOT
already done here, in the order it is worth doing.

CAVEAT ON ALL THREE, stated once: every reported gain comes from a
FINE-TUNED model (GPT-2, T5). What transfers is the construction of
the input, not the numbers. Each item below is a hypothesis to
measure on our own data, never a predicted result.

- [ ] tod-demonstrations-from-the-log — "Show, Don't Tell" (Zhao &
      Gupta, Google Research 2022; SDT arxiv 2204.04327, D3ST arxiv
      2201.08904; SGD / MultiWOZ 2.4 / SGD-X). Their finding: ONE
      annotated example dialogue in the input beats slot DESCRIPTIONS
      — and descriptions are exactly the half we render today. The
      okay-shaped part is where the example comes from: a recorded
      turn in the durable ChatLog, together with the typed belief it
      produced, IS an annotated example. The log stops being only an
      audit trail and becomes prompt material. Highest value of the
      three and the cheapest; offline-testable with a scripted model.
- [ ] tod-schema-diagnostics — two experiments, not features, both
      from the same pair of papers. (a) D3ST randomizes slot names to
      arbitrary indices so a TRAINED model cannot lean on the names.
      For a PROMPTED model that is not a technique we want — we WANT
      the priors around a word like "email" — but it is an excellent
      DIAGNOSTIC: shuffle the names to indices and measure the drop.
      A collapse says our descriptions are decorative and the model
      was riding the names. (b) SGD-X's robustness method: paraphrase
      the attribute descriptions and measure whether extraction is
      stable. Together they turn "our schemas are good" from an
      assumption into a number, BEFORE we invest more in writing
      them.
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
- [ ] tod-multiwoz-harness — OPTIONAL and honestly expensive: a
      loader for MultiWOZ 2.3/2.4 plus the inform/success/joint-goal
      metrics, so our dialogue lane has numbers comparable with the
      outside world instead of only with itself. Keep separate from
      the items above; it is a benchmark harness, not a feature.

## okay-ui: above v1 (specs/ui.md, "The architecture above v1")
- [ ] ui-native-toolkits — GTK/Cocoa satellites over the Backend seam
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-codec
- [x] json-value-parser — landed: JsonValue.parse, a strict
      recursive-descent parser yielding to the lossless CST parser on
      any doubt; Json.parseValue wires it in. 61x over Json.parse on
      the fixture, 2.0x faster than circe's own parser; end to end
      with the staged decoder, 2.3x faster than circe's fused
      parse+decode. specs/codecs.md, "Value parser".
- [x] staged-cbor — landed: Staged.cbor[A], sharing the Reflect base
      with Staged.json; Cbor.scala's Out/In made public so both the
      fold and the staged generator call the same primitives. Encode
      1.6x, decode 2.0x over the interpreted fold. Named by
      okay-persist's own wire path. specs/codecs.md, "Staged CBOR".
- [ ] staged-runtime — `scala.quoted.staging` for run-time schemas
      (ToolSpec from a model, Pg composites from the catalog);
      JVM-only opt-in module, compiler dependency; only on a named
      workload (specs/codecs.md, Staged fold mode, Out of scope)

## okay-py (specs/py.md — Python as a handler; model = specs/r.md by reference)
- [ ] py-arrow — frames via pyarrow (twin of r-arrow; nearer —
      pyarrow is first-class)

## okay-r (specs/r.md — R as a handler)
- [ ] r-subprocess — stage 0: the module, REval/RValue/RFrame,
      Rscript engine (CBOR/JSON wire, clean env), verify(packages),
      condition-as-data, dead-process-throws; Durable-replay test
- [ ] r-rserve — stage 1: the served engine (Java client behind a
      trait; own QAP1 over Async later if named); two-engine
      acceptance
- [ ] r-arrow — frames as Arrow files/streams once the JSON-frame
      road hurts

## okay-persist (specs/persist.md — staged design; stage 0 landed)
- [x] persist-raft — RaftStore: consensus as one more control-log
      engine under the unchanged Election machinery (specs/
      consensus.md own-Raft notes; typestate per specs/typestate.md)
      STAGE 0 LANDED 2026-09-03 (operator: "start it anyway," a
      months-scale effort taken as a staged climb, not attempted
      whole): okay.persist.Raft — the pure algorithm core, leader
      election + log replication, seven tests proving election
      safety, log matching, the Figure 8 commit trap.
      STAGE 1a LANDED 2026-09-03: okay.persist.RaftWire.Node — a
      real peer-to-peer wire transport, RaftMsg over real sockets
      (the SAME [len:int32][CBOR] framing Wire.scala uses), real
      wall-clock election timeouts/heartbeats. Three real nodes
      elect a leader, replicate and commit a client entry, and fail
      over on a killed leader — all over an actual network. Stage
      1b LANDED 2026-09-07: `okay.persist.RaftStore` — a `Store` over
      the wire node (an append proposed as one log entry, applied on
      every node at commit to its local store; reads from the local
      store, so nothing uncommitted is ever served; a follower's
      append throws `NotLeader(leader)`, a lost majority
      `NotCommitted`) — and `RaftWire.Stable` (term and vote saved
      inside the lock before any send; a file replaced by rename, or
      memory). `TestStable` in the gate, `TestRaftStore` Live beside
      `TestRaftWire`. FORWARDING LANDED 2026-09-07 (persist-raft-
      forward): `RaftMsg.Propose`/`Proposed`, a follower's append
      carried to the leader and applied everywhere; `NotLeader` only
      when no leader is known. SIM HARNESS LANDED 2026-09-07
      (raft-sim-fuzz): `TestRaftSim`, a discrete-event simulator over
      the pure core — five nodes, random timeouts, reordering, 10%
      loss, a minority cut and healed — safety asserted after every
      event on 40 seeds, acked proposals never lost, convergence and
      a late ack on a lossless stretch; replayable by seed. STAGE 2a
      LANDED 2026-09-07 (raft-membership): single-server membership
      changes (thesis §4.1) — a configuration entry in the log, in
      force on append, one change at a time, a removed leader steps
      down at commit, a removed node stops campaigning;
      `reconfigure(cluster)` on the wire node and the store; swept
      by the simulator (a sixth node joins, the leader leaves, 40
      seeds clean). The sweep forced two of the paper's rules the
      core had skipped: the blank no-op at the start of a term (§8)
      and conflict-only truncation in AppendEntries (§5.3) — a real
      safety bug under reordering, fixed. STAGE 2b LANDED 2026-09-07
      (raft-compaction): `Raft.compact` (the engine's bytes, the
      core's index arithmetic), `InstallSnapshot` for a follower
      whose next entry is compacted away, `restored` as the engine's
      cue; `Node.compact`/`onRestore` on the wire; the simulator's
      nodes run state machines they snapshot and compact to — 541
      snapshots, 81 installs on 32 of 40 seeds, safety asserted on
      what the machines saw. STAGE 2c LANDED 2026-09-07
      (raft-store-snapshot): `RaftStore.snapshot()` — the local
      store's full history as the image, refused by name once
      retention has dropped history (offsets must survive a
      restore); restore appends the image's records past the local
      `end`, a gap is `damaged`; `snapshotEvery`; the wire's commit
      callbacks moved inside the node's lock (a real ordering race
      between connections). PRE-VOTE LANDED 2026-09-07
      (raft-prevote), measured: a `PreCandidate` asks at its next
      term without adopting it, a voter grants only on the log AND
      no leader heard within an election timeout (the caller's word:
      `handle(..., leaderFresh)`); terms over the forty partition
      seeds 163 → 67, over the membership seeds 174 → 93, more
      proposals acked. LEFTOVERS DECIDED 2026-09-07 (raft-leftovers):
      the joiner's catch-up phase declined by measurement (a joiner
      is current 25..165 ms after it is added, median 71, on 40 of
      40 seeds, one to three heartbeats, while the cluster commits
      0..1 entries — a learner phase shortens a window already
      shorter than a heartbeat's commits; returns when a snapshot
      takes longer to transfer than an election timeout); chunked
      InstallSnapshot declined until an image near the 2 GB frame
      exists (flow control, not correctness); the commit-wait as an
      `Ack` level declined by the contract (an offset exists only
      once applied, i.e. committed, so every level waits for the
      commit; the timeout is the one honest knob). BOX CLOSED: stages
      0-2c and pre-vote landed, the rest decided on evidence.

## okay-http (sibling's area — coordinate before taking)
- [ ] flaky-port-roulette — the full-matrix port/readiness family,
      one ledger: TestMcpHttp 503 (2026-09-01), TestResumable first
      subscribe, TestHttp first GET 404, and TestWire reading
      literal "HTTP" bytes at its handshake (a foreign server
      answered on the expected port) — all green alone, all under
      parallel suites in one sbt JVM; suspect ephemeral-port reuse
      between a closing listener and a dialing client
- [x] ui-cmd-flaky — FIXED 2026-09-01 by the unprocessed-counter
      close redesign (the runCmd race: close only when upstream done
      AND pending==0 AND unprocessed==0); TestCmd 3x3 green since.
- [x] demo-chat-live-budget — FIXED 2026-09-01: munitTimeout raised
      to 180s in TestChatDemo (the TestRepoAgent precedent, sized
      for a busy local model under a full matrix).
- [ ] http-flaky-mcphttp — TestMcpHttp "one Serving, three wires"
      answered 503 once in a full-matrix run (2026-09-01); green
      alone and on suite rerun — likely a port/readiness race
      (second sighting, same family: okay-jetty TestResumable
      failed its first subscribe once in a full-matrix run
      2026-09-01, green twice alone — port/readiness race shape)
- [ ] http-streaming-responses — incremental bodies on the NIO and
      Netty backends (Jetty has it); unblocks MCP push there
- [ ] http-post-body-audit — Netty/NIO: do POST bodies reach routes?
      (Jetty's did not — found by mcp-push, fixed there)

## okay-demo (the showcase lane — specs/demo-chat.md, specs/match.md) — DONE, all 11 landed
- [x] demo-streaming-cut — LANDED 2026-09-02: `Chat.reply`/`chatRoute`
      gain a `policy: (Int, String) => Option[Cut.Violation]`, checked
      alongside the token budget in the SAME `Cut.checked` — additive,
      defaults to never-violate. The demo wires a banned-word content
      policy; `Chat.scripted` echoes the user's message, so typing the
      banned word is itself the trigger, offline. Closes the Elsewhere
      gate on `llm-streaming-cut`.
- [x] demo-ctx-wiring — LANDED 2026-09-02: ChatDemo.handler(budget)
      is `(Transport, Secrets, MatchStore) ?=> Route`; main wires
      Transports.http() + Secrets.env, the test wires a canned wire +
      memory Secrets and runs the LIVE Anthropic.stream path offline;
      offline suites run over a DEAD wire. Closes the Elsewhere gate.
- [x] demo-market-live — LANDED 2026-09-02: GET /market.json (facts
      with attr names, Public-only), GET /events/market (SSE feed
      pinged from the chainedTable wraps + /admin/replay), the page
      re-renders on every ping with attribute facet chips; rows stay
      server-rendered at load.
- [x] demo-deal-timeline — LANDED 2026-09-02: chainedTable threads
      off: Long; match_inquire/match_respond append a DealEvent(state,
      by, Provenance) per transition — append-only, never rewritten.
      GET /deals/<n> and /deals/<n>.json render the full history with
      provenance; a withdrawn stand-down gets its own event; unknown
      deal is 404.
- [x] demo-mcp-market — expose the market tools (search / assert /
      deal / flow) as an MCP server over okay-http's MCP: any MCP
      client (Claude included) becomes a market participant; the chat
      UI unchanged, the marketplace becomes the shared substrate.
      LANDED 2026-09-02: chainedTable mounted at /mcp via
      McpHttp.route; mcpTable rebuilds it per call for fresh
      offset/period. Caught and fixed a real bug — mcpRoute must be
      built ONCE per server (a def re-evaluated per request built a
      fresh MCP session table each time, dropping every session right
      after initialize).
- [x] demo-two-nodes — two demo processes over one shared durable
      log: Election picks the writer, both serve reads, kill the
      leader and watch the market survive — the persist/Election
      machinery in a consumer-visible showcase. Sized LARGE; take
      only when a distributed demo is named wanted.
      LANDED 2026-09-02 (named wanted by the operator): TwoNode
      polls a shared OKAY_CHAT_LOG directory (FileStore has no live
      cross-process tailing, stated not hidden), Election picks the
      writer, POST is 503-gated to the leader, GET always serves.
      TestTwoNode launches two REAL OS processes, kills the leader,
      proves the survivor takes over and the market holds.
- [x] demo-scenario-editor — scenarios are already data
      (ScenarioDef): a UI page to author one (steps, prompts, deal
      hook), saved through the store, listed by the help command —
      extensibility without touching code, shown not told.
      LANDED 2026-09-02: GET/POST /scenarios edits the plain JSON
      shape of ScenarioDef/Transition directly — "steps"/"prompts"/
      "deal hook" turned out to already BE transitions/notifies, no
      new schema needed. MatchStore gained `scenarios` (no list-all
      method existed); help text now names what's registered instead
      of a static hint.
- [x] demo-en-phrasebook — LANDED 2026-09-02: isEnglish(text) (no
      Cyrillic) picks the reply template per message, no session
      state; every trigger pairs 1:1 (умею/can:, нужен/need:-want:,
      спроси/ask, сценарий/scenario, шаг/step, флоу/flow,
      берусь/accept, отказываюсь/decline, помощь/help); both speak
      the SAME chainedTable.
- [x] demo-e2e-browser — a browser-level test of the React UI
      (today's tests hit the HTTP/SSE seam directly, so the React
      layer itself is untested); smallest honest version: build the
      bundle, drive one chat round through a headless browser.
      LANDED 2026-09-02: okay-demo-e2e-browser (Playwright, real
      headless Chromium) — typed text sends, the scripted reply
      streams in via the SAME fetch+ReadableStream glue Main.scala
      ships. Kept OUT of okay-demo's test sourceset and the root
      aggregate (a real ~450MB one-time browser download); invoke
      via `sbt "okayChatWebJS/fastLinkJS" "okayDemoE2eBrowser/test"`.
- [x] demo-package — one-command run: bundle the React build into
      the jar's static assets (+ optionally a Dockerfile); today the
      demo needs sbt and a node dev server side by side.
      LANDED 2026-09-02: Deploy.extraBuild/extraCopy (both additive,
      empty by default — no drift on any other Deploy value);
      DemoDeploy.spec links okayChatWebJS and copies main.js to
      /app/app.js, wired through Chat.appJs's existing OKAY_CHAT_APP
      env var — okay-chat itself needed no change.
- [x] demo-gate-ui — the platform Gate policy (Allow / AfterMatch /
      Withhold) switchable from an admin page per attribute class;
      today it is set in code — the two-gate visibility model is the
      business story, so let a viewer flip it and watch /market react.
      LANDED 2026-09-02: MatchStore.gate/setGate/gateOverrides — a
      `livePolicy` var replacing the immutable constructor-bound
      PlatformPolicy; POST /admin/gate flips it, admin-token gated
      like /admin/replay; /market gained a panel and /market.json a
      "gates" field.

## Reusable modules extracted from the demo (user ask 2026-09-02) — DONE

All three landed: okay-subscription, okay-admin, okay-chat (specs/
subscription.md, specs/admin.md, specs/chat.md). The demo now
composes three independent modules plus okay-match via `orElse`
route tables instead of holding their logic inline.

- [x] okay-admin — LANDED 2026-09-02: `Admin.routes(verify, policy =
      Policy.scoped("admin"), realm)(replay, onReplayed)` on
      `Secure.granted`, plus `Admin.Issuer` (an ES256 keypair, same
      shape as `okay.demo.Login`) so a consumer has a credential to
      test/use it with. Fixed the real gap named when this was filed:
      the demo's `POST /admin/replay` is no longer reachable without
      an admin-scoped bearer token; the token rides the server
      console at startup (same "no delivery channel yet" precedent
      Login's one-time code already set).
- [x] okay-chat — LANDED 2026-09-02: `Model`/`scripted`/`live`/
      `local`/`model`/`modeName`, `sse`/`obj`/`reply` (Cut-guarded
      SSE, `sse`/`obj` public — a consumer's OTHER streams reuse the
      same framing), `fieldOf`/`messagesOf`/`appJs`, and `chatRoute(
      m, budget, turnOverride: (Request, Seq[Anthropic.Message]) =>
      Option[Source[Chunk[Byte]]] = (_, _) => None)`. Widened from
      the original sketch (`Seq[Anthropic.Message] => ...`) to also
      carry the full `Request` — found while wiring the demo: the
      `/match` override needs the bearer token off the request's
      headers, which parsed messages alone cannot carry. page/
      reactPage HTML stayed OUT of the module as planned (market-
      flavored — a market link, example chips, `/events/<email>`
      inbox JS); the demo keeps its own copy, reusing `Chat.Model`/
      `reply`/`sse`.

## Round two: what else in the demo is reusable (user ask 2026-09-02) — DONE

All three landed: pg-target-in-okay-pg, okay-live, login-in-okay-
security (specs/sql.md, specs/live.md, specs/security.md). The first
three extractions left ~1160 lines in ChatDemo.scala; surveyed for
what else earns a move — not everything does; the condition-based
intake (BadEmail/resolveEmail) and the deal timeline stay demo-local,
named and reasoned in specs/demo-chat.md already. Three did earn it:

- [x] okay-live — LANDED 2026-09-02: `Hub[A]` (broadcast, `subscribe()`
      /`publish(a)`) and `Registry[K, A]` (`apply(key)`, lazy per-key
      channel), a new JVM-only module (same java.util.concurrent
      reasoning as okay-subscription — filed for cross-platform
      unification below). `marketFeed`/`inboxes` in ChatDemo.scala
      now delegate to one `Hub`/`Registry` each.
- [x] pg-target-in-okay-pg — LANDED 2026-09-02: `PgTarget` moved to
      `okay-pg/src/main/scala-jvm` (the JVM leg PgTls.scala already
      lives on). Its own TestPgTarget suite in okay-pg (4 tests,
      3 new: disable/absent plaintext, require carries no CA,
      malformed URL never throws); the demo keeps only the live-
      Postgres integration test (proves marketOf's own wiring).
- [x] login-in-okay-security — LANDED 2026-09-02 (specs/security.md,
      stage 6, security-sessions): `SessionIssuer(ttlSec)(subject,
      scopes)` (the ES256 keypair-plus-issue/verify shape) and
      `OneTimeCode(ttlMs)` (confirm-and-sign), both okay-security/
      scala-jvm. `okay.demo.Login` and `okay.admin.Admin.Issuer` are
      thin wrappers now; a caught bug on the way — a first "expired
      token" test landed inside `Jwt.verify`'s default 60s clock-skew
      tolerance and silently passed, fixed by advancing further.

## Cross-platform concurrent state (operator ask 2026-09-02, filed while landing okay-live)

Two round-two modules made the SAME tradeoff for the SAME reason:
`okay-subscription` (joinedPeriod/paidPeriods) and `okay-live`
(Hub/Registry) both needed a safely-shared, growing collection
(a map, a list) and both reached for `java.util.concurrent`
(ConcurrentHashMap/CopyOnWriteArrayList) — which is JVM-only, so
both modules became JVM-only projects rather than crossProject(JVM,
JS, Native), even though everything ELSE about them (the values
they hold, the operations they expose) has no platform opinion.
`okay` core already carries the machinery this problem wants:
`TRef[A]`/`Stm.atomically` (src/main/scala/Stm.scala) is a
cross-platform (JVM/JS/Native) transactional cell, and the STM
engine's OWN write-set bookkeeping already leans on an internal
`TMap` — proof the pattern the two modules need (a transactional
map, a transactional growing list) is buildable on what exists,
not a new primitive from scratch.

- [x] okay-stm-collections — a small cross-platform layer ON TRef:
      at minimum a `TMap[K, A]`-shaped wrapper (get/put/computeIfAbsent
      equivalent, atomically) and a `TList[A]`/append+snapshot shape
      — public API, not the STM engine's private bookkeeping TMap.
      The real design question to answer before building, not
      assumed: `Hub.subscribe()`/`Registry.apply(key)` are PLAIN
      synchronous methods today; an STM-backed version makes them
      effectful (`... ! F`, run inside a transaction) — decide
      whether that's an acceptable API change for every call site,
      or whether a thin JVM-only synchronous facade stays over a
      cross-platform STM core (facade cost vs. honest effect type).
      LANDED 2026-09-03: `TRef.modify` is ALREADY synchronous, so a
      single-cell dict/list never needs `Tx`/`Stm[F]` at all — no
      facade, the plain shape IS the honest one. Named `TDict` (not
      `TMap`: that name is taken, by exactly the engine bookkeeping
      type this bullet warned about). A 64-thread stress test found
      a real, stated limit: `computeIfAbsent`'s `mk` may run more
      than once under CAS contention (only the winner's value is
      ever stored) — fixed the doc, not hidden.
- [x] Once landed: migrate `okay-subscription`'s two maps and
      `okay-live`'s `Hub`/`Registry` onto it, and reconsider whether
      either module (or okay-demo itself) should become crossProject
      at that point — no JS/Native consumer is named yet, so this is
      NOT urgent; filed so the decision is made once, deliberately,
      not by accretion the next time this exact tradeoff recurs.
      PARTIAL 2026-09-03: `okay-subscription` migrated, a pure swap
      (9/9 existing tests unchanged). DONE 2026-09-07 (live-tdict):
      `okay-live`'s `Registry` over `TDict.computeIfAbsent`, `Hub`
      over `TList` — a pure swap, the module's last
      `java.util.concurrent` gone, tests unchanged. The crossProject
      question, decided: neither module moves — no JS/Native consumer
      is named, and a crossProject with one platform is a promise
      nobody asked for; the swap leaves the door open at the cost of
      one build.sbt line when someone does.

## okay-script (specs/okay-script.md) — markdown ```scala fenced blocks as Scala source
- [x] okay-script-site — LANDED 2026-09-06 (c0b37da2): the container,
      "a new JSP" complete. `Site(root).routes` serves a directory of
      `.md` pages over okay-http/okay-jetty (`/a/b` → `a/b.md`,
      `index.md`, `[param].md`, static files); `okay.script.api`
      (Web with form/cookies/body/params, Response with status/
      headers/redirect/cookie, Session, Error, include, forward) is
      DELEGATED to the host classloader servlet-API style — and so is
      `scala.*`, because delegating the API alone fails the JVM's
      loader-constraint check on `Option`/`Map` in its signatures
      (found on the first run). ```scala declare (JSP `<%! %>`),
      error.md / errorPage:, contentType: front-matter, TTL sessions.
      Per-thread stdout capture (Capture) replaces the JVM-global
      System.setOut, so concurrent requests are correct. Example
      okay-script/examples/site (a store with a session cart).
- [x] okay-script-multipart — LANDED 2026-09-06: multipart/form-data
      uploads reach a page as `Web.parts` / `Web.file(name)`
      (`api.Part`: name, filename, contentType, bytes); a multipart
      request's non-file fields land in `Web.form` too. Byte-level,
      binary-safe parser (`okay.script.Multipart`), no dependency;
      damage yields no parts rather than a 500. specs/okay-script.md
      "Uploads".
- [x] okay-script-secure — LANDED 2026-09-07: declarative page security,
      web.xml's constraint. `secure: <scope>`/`any` and `loginPage:`
      in front-matter; `Site(verify = Some(...))` checks a bearer
      token from the header or the session attribute `okay.token`
      (`api.login(token)`/`logout()`); 302 to the login page with
      `?next=` when one exists, else okay-security's 401/403 ladder;
      no verifier → 500. Forwards checked, includes not; a secure
      page's Live socket checked from its cookie. `Principal.current`.
      specs/okay-script.md "Declarative security".
- [x] okay-script-live — LANDED 2026-09-06 (operator ask): okay-ui as
      the front-end layer. `api.Live(init)(view)(update)` declared at
      object level, `${mount("id", app)}` in prose: SSR of the first
      tree (React.elem → HTML) + script; `Site.ws` runs okay-ui's
      `Wire.serve` over the page's own WebSocket (`?__live=<id>`);
      `/__okay/live.js` is a dependency-free WireJson patch consumer
      served by the container. Classloader: `okay.*` shared with the
      host, and — found by the lifecycle test's ClassCastException on
      jetty's Server — any class on BOTH the page's classpath and the
      host's is the host's. specs/okay-script.md "Live pages".
- [x] okay-script-live: reconnect with state — a socket is one session
      from `init`; a browser that reconnects starts over. okay-ui's
      ui-durable (journal + refold on okay-persist) is the mechanism;
      key by session cookie. DONE 2026-09-07 (script-live-resume) for
      the RECONNECT: `Live[S]` keeps the state each session cookie
      last reached (a `TDict[String, S]` inside the app, no cast),
      `session(key)` starts from it and remembers on `Closed`;
      `Site.ws` reads the cookie, `mount` opens the session so the
      page sets one. In memory for a plain `Live`. DURABLE 2026-09-07
      (script-live-durable): `Live.durable(...)(using Schema[S])`
      keeps the state as a session attribute (`okay.live.<id>`, CBOR
      in base64) through the bound `Sessions.Handle`, so
      `Sessions.persisted`/`shared` make a restart and a second node
      resume too, swept by the session TTL; not a journal + refold —
      one value per session written on close is what a session
      attribute is, and nobody has asked for replay in between.
- [x] okay-script-live: server-pushed updates — a Live app only
      changes on a client event; a source merged in (`Ui.run`'s
      `external`) is what a ticking clock or a shared poll needs.
      DONE 2026-09-07 (script-live-push): `Live(init)(view)(update,
      push = Source[Event])` — the server's own events, a fresh
      instance per session, the same capability rule as the
      browser's; `Jetty.serve(port)(routes)(ws, push)` — an optional
      `push: PartialFunction[Request, Source[Frame]]` the transport
      feeds into the socket's input channel beside the client's, so
      `Wire.serve` is still used verbatim and `ws` keeps its callers;
      `Site.push` beside `Site.ws`. In-JVM and over Jetty: two
      patches arrive with nobody pressing.
- [x] okay-script-cluster-sessions — LANDED 2026-09-06 (operator ask):
      `Sessions.shared(topic)` — the persisted engine over a
      `Replicated` coordinator (leader node) or a `RemoteStore` topic
      over the wire (every other node), index kept current by tailing
      the topic, own offsets skipped. Found by the test: a touch was
      a full-state write, so a stale-cookie read on A racing an
      invalidate on B resurrected the session in log order — a
      logout that did not stick; a touch is now an 8-byte
      update-if-present record. specs/okay-script.md "Clustered
      sessions".
- [x] okay-script-persistent-sessions — LANDED 2026-09-06: `Sessions`
      is a trait; `Sessions.memory` (default, unchanged) and
      `Sessions.persisted(store)` over an okay-persist keyed compacted
      topic (whole state per write, tombstone on invalidate, rebuilt
      on open) so a Site's carts survive a restart. Expiry stays the
      sweep's job on the caller's clock — the first draft dropped
      entries in the rebuild by wall clock, a synthetic-clock test
      caught it. specs/okay-script.md "Persistent sessions".
- [x] okay-script core — LANDED 2026-09-03: `blocks`/`run`, one .md
      file = one compilation unit (blocks concatenate in document
      order, later blocks see earlier ones' val/def), driven through
      dotty.tools.dotc IN-PROCESS (no scala-cli subprocess, no custom
      language/interpreter). Success = compiles + runs without
      throwing; stdout captured. Investigated ../scalascript first —
      unrelated (a full custom markdown-as-syntax language), nothing
      reusable found, recorded as a negative result in the spec.
- [ ] okay-script: sbt-test / CI integration — a task walking
      `specs/*.md` (or a configured dir), failing the build on the
      first `!ok` Result. Deliberately not built with the core
      (operator: "библиотека/API, без интеграции в sbt test пока").
- [x] okay-script-check — LANDED 2026-09-03: mdoc-style literate
      testing — a block's expected stdout, written inline as a NEW
      ```stdout fence, checked against what a real `run` actually
      printed. `ScalaScript.check(markdown, classpath): CheckResult`
      is purely ADDITIVE and host-side — no synthesis changes, no new
      fence recognized by `tokenize`/`withMeta` at all (deliberate,
      right after two landings in a row hit the same re-indentation
      bug shape there). Extracts every ```stdout fence's (trimmed)
      content via a plain line-scanner mirroring `blocks`' own, runs
      the document once via ordinary `run`, then verifies each
      expected chunk appears as an IN-ORDER, non-overlapping substring
      of the actual stdout — proving the right output happened in the
      right relative sequence without injecting a checkpoint into the
      compiled program. All mismatches collected, not just the first;
      a `run` that fails to compile fails `check` immediately with one
      summarizing mismatch. First cut passed all 8 tests on the first
      run — no bug found, unlike the two landings right before it.
      specs/okay-script.md "Output-comparison testing".
- [x] okay-script-line-mapping — LANDED 2026-09-03: a compile error's
      line number now reports the ORIGINAL `.md` file's line, not the
      SYNTHETIC wrapped source dotc actually compiled. `Segment.Code`/
      `Interp` gained a `startLine` (`tokenize` computes it, same
      convention as `Block.startLine`); `withMeta` builds the
      synthesized body PLUS a parallel `Vector[Int]` line-origin map
      (one entry per physical body line, `-1` for injected/synthesized
      lines); `collectingReporter` reads `dia.position()` (confirmed
      0-based empirically, via a throwaway probe, before writing
      anything) and prefixes a mapped diagnostic `"L<n>: "`. A
      multi-line block's error correctly reports ITS OWN line, not
      just the block's first. Found and fixed along the way: the SAME
      bug shape as okay-script-web's `compileOnly` fix, one function
      over — `withMeta`'s first cut indented EVERY physical line of a
      `Text`/`Interp` segment's synthesized `print("""...""")`
      uniformly, corrupting embedded multi-line string DATA the same
      way the earlier fix corrupted it at a different layer;
      `TestScalaScriptRender`'s own test caught it again immediately.
      Fixed with an explicit `isStatement` flag per item (`Text`/
      `Interp` indent only their first physical line; `Code` indents
      every line). specs/okay-script.md "Line-accurate errors".
- [x] okay-script-runtime — LANDED 2026-09-03: the REAL goal named by
      the operator is runtime app generation (generate a `.md`,
      compile+run it AT RUNTIME, come up as a live web app — a
      storefront), not a doc smoke-test; see specs/okay-script.md
      "The real goal". Added `Classpath` (explicit classpath entries,
      `ambient` as a documented-fragile default) and `Deps`
      (`//> using dep "org:artifact:version"`, resolved via the
      `cs`/`coursier` CLI) so a generated script can be handed exactly
      the jars it needs (okay-ui/okay-jetty, an extra library) instead
      of inheriting the host process's own classpath. Also fixed
      okay-script-scalac-classpath above as part of the same pass (the
      bug that made the classpath question concrete, not hypothetical).
- [x] okay-script-lifecycle — LANDED 2026-09-03: the `Server !
      Resource` lifecycle question above is settled, no new
      `ScalaScript` API needed. `Resource.run` releases every acquired
      finalizer on ANY escaping `Throwable` (Resource.scala's `_loop`),
      and `ScalaScript.run` invokes the compiled script synchronously
      on whatever thread called it — so a caller runs `run` on its own
      `Thread` (does not block the generator) and stops the app with
      `Thread.interrupt()` (makes the script's own `Thread.sleep(
      Long.MaxValue)` throw, which `Resource.run` turns into a real
      `server.stop()`, not just an abandoned thread). Proved against a
      REAL `okay-jetty` server in `TestScalaScriptLifecycle` (Live):
      answers HTTP while alive, stops answering after interrupt, the
      returned `Result` carries the `InterruptedException`.
      specs/okay-script.md "Lifecycle".
- [x] okay-script-storefront-example — LANDED 2026-09-03:
      `okay-script/examples/it-consulting-storefront.md` — a real
      `okay-jetty` server (page + `/order/<key>` route), content
      (services/prices) taken verbatim from `../it-consulting/site/
      site.md`, compiled and run end to end through `ScalaScript.run`
      using the proven lifecycle recipe (own `Thread`, `Thread.
      interrupt()` to stop). No `busi`/`scalascript` DSL reused — only
      the data; the page and `/order` handler are ordinary Scala.
      Proved by `TestScalaScriptStorefront` (Live): all five services
      render with prices, `/order/<key>` confirms the right one,
      interrupt stops the server. Found and fixed along the way: the
      example's first cut used a QUERY STRING (`/order?key=<x>`), which
      okay-jetty's `Request.url` silently never carries (`Jetty.scala`'s
      `requestOf` uses `getPathInContext` — path only, no query-string
      field on `okay.http.Request` at all) — see the next entry.
      specs/okay-script.md "Worked example".
- [x] http-request-query — LANDED 2026-09-03: NOT "no query-string
      support at all" as first written — `okay.http.Server` (JDK) and
      `okay-netty` both already carry the query string in `Request.url`
      (`getRequestURI().toString()` / `req.uri` are both full
      request-targets). Only `okay-jetty` was broken: `requestOf` built
      `url` from the static `getPathInContext(req)`, PATH ONLY. Fixed:
      `req.getHttpURI.getPathQuery` instead — a route with no query
      string sees the byte-identical string as before (`HttpURI`'s own
      `getPathQuery` returns the bare path when there is no query), and
      no `ContextHandler` is used anywhere in `Jetty.serve`, so nothing
      about an existing route's matching changes. `TestJetty` gained a
      query-string test; `TestBackends`' cross-backend matrix never
      exercised one either (noted in specs/http-backends.md as the gap
      that let this ship unnoticed). specs/http-backends.md.
- [x] okay-script-classloader-isolation — LANDED 2026-09-03: each
      `run` call already had its OWN `URLClassLoader` (scripts do not
      collide with EACH OTHER), but its parent was
      `getClass.getClassLoader` — `okay-script`'s own defining
      classloader — and `URLClassLoader` is parent-FIRST, so a script
      could silently resolve a class from `okay-script`'s own build
      (munit, in Test scope okay-jetty, ...) regardless of what the
      caller's explicit `Classpath` actually listed — the isolation
      `Classpath`/`Deps` (okay-script-runtime) were built for was not
      actually enforced. Fixed: parent is now
      `ClassLoader.getPlatformClassLoader()` (JDK core only) — a script
      sees exactly its own compiled classes, its own `Classpath`, and
      the JDK. No behavior change for `Classpath.ambient` callers (it
      already lists ~everything). Proved by
      `TestScalaScriptClassloaderIsolation`: a script given a minimal
      `Classpath` can no longer reach `munit.Assertions` (present on
      `okay-script`'s own test classpath, absent from that minimal
      one) — confirmed as a REAL regression check by temporarily
      reverting the fix and watching the test fail before restoring
      it. specs/okay-script.md "Classloader isolation".
- [x] okay-script-interpolation — LANDED 2026-09-03: the operator's
      own framing for `okay-script` — "a new JSP, but Scala+Markdown".
      New `ScalaScript.render(markdown, classpath): Result`, separate
      from `run` (untouched — still for apps/effects like the
      storefront). `render` recognizes `${expr}` in PROSE (outside
      ```scala fences; `$${` escapes to a literal `${`) as a Scala
      expression evaluated in the SAME document-order scope
      ```scala blocks build, `.toString`-printed in place; everything
      else — prose, other-language fences — passes through verbatim.
      Brace-depth- and quote-aware scanner (handles a NESTED real
      `s"${x}"` string interpolation inside an `${...}` marker's own
      expr). The rendered document is `Result.stdout`. Worked example:
      `examples/render-storefront.md`. One design refinement made
      BEFORE any test ran: direct `print(...)` per segment instead of
      a buffer flushed at the end, so a code block's own `println`
      output stays in true document order instead of reordered after
      the whole rendered text. specs/okay-script.md "Interpolation".
- [x] okay-script-page — LANDED 2026-09-03: the HOT-RELOAD half of
      "per-request execution + hot-reload" (the REQUEST-OBJECT
      injection half is still open, see the next entry). New
      `Page(path, classpath)`: compiles a `render`-mode `.md` file
      ONCE, cached by the file's mtime, re-INVOKES (not re-compiles)
      on every `render()` call while the file is unchanged — the
      actual JSP shape (a page's servlet class compiles once, its
      per-request method runs once per request). No new dependency;
      an actual `okay-jetty` route stays glue code a caller writes.
      Split `ScalaScript.compileAndRun` into `compileOnly` (returns an
      invokable `Compiled` handle or a `Result` with compile errors)
      and `Compiled.invoke()` (callable repeatedly). Found and fixed
      along the way: a SECOND `invoke()` on the same compiled program
      silently printed nothing — a real, previously-invisible bug from
      okay-script-classloader-isolation (the isolated script
      classloader loads its OWN separate `scala.Console`, so the
      original host-side `scala.Console.withOut` fix for capturing
      `println` never touched it; it only "worked" for a one-shot call
      by coincidence). Traced to a minimal bare-classloader
      reproduction before writing the fix. Fixed by driving the
      isolated classloader's OWN `Console` via reflection
      (`setOutDirect`) on every `invoke()` — applies to `run`/`render`
      too, though it was invisible there. specs/okay-script.md
      "Hot-reload".
- [x] okay-script-web — LANDED 2026-09-03: the REMAINING half of "a
      new JSP" — a script reading the CURRENT HTTP request (method,
      path, query, headers) the way it already reads `Meta.current`
      for file metadata. Scoped to avoid the dependency this entry
      itself flagged: new `Web` is a plain, dependency-free case class
      (`String`/`Map` only) — no `okay.http.Request` import anywhere
      in `okay-script`'s own code; a caller (an `okay-jetty` route)
      translates its own `Request` into `Web` before calling `render`/
      `Page.render`. `Page.render(web)` sets it FIRST, inside the
      page's existing lock, so concurrent requests never race on which
      request's `Web` a given call sees. Found and fixed along the
      way: `Web` hit the SAME classloader-identity trap
      okay-script-page's Console fix found, one level up, for a
      user-defined type — a host-built `Web` handed directly to the
      isolated script fails reflection's argument-type check (the
      isolated loader compiles its own separate `Web` class). Fixed by
      encoding `Web` into a flat `Array[String]` host-side and
      decoding it back INSIDE the isolated classloader — only
      `String`/`Array[String]` cross the boundary — which meant
      abandoning `@main def okayScriptMain(): Unit` (its generated
      forwarder never hands `args` through when the `@main` method
      itself takes zero parameters, which it always did here) for a
      plain `object OkayScriptMain: def run(args: Array[String]):
      Unit`, confirmed via `javap` before writing the change. That
      wrapper change broke output for EVERY existing example — caught
      immediately by `TestScalaScriptRender`'s own test, not a
      `Web`-specific failure — because the naive fix (re-indent the
      already-built body by prefixing every physical line) corrupted
      DATA inside a `Text` segment's multi-line raw string literal,
      indistinguishable from source formatting to a blind line-prefix
      pass. Fixed by having every body-line producer build lines at
      their FINAL depth directly. Also repeated (and fixed the same
      way as) `hasMeta`'s own self-sufficiency lesson: an unconditional
      `Web` reference broke `TestScalaScriptClassloaderIsolation`'s
      minimal-Classpath case again; `hasWeb` gates it now.
      specs/okay-script.md "Request context".
- [x] okay-script-meta — LANDED 2026-09-03: code inside an .md file
      reads the metadata defined in the markup AROUND it, as its
      current context (operator ask). Front-matter (`---`, file-level)
      plus nested ```yaml fences scoped by heading ancestry — the
      shape `../it-consulting/site/site.md` already uses. New module
      `okay.script.Meta`: a typed AST (`Value`/`Section`/`Doc`) built
      by a minimal YAML-subset parser, plus `Context(doc, path)` with
      untyped `get`/`apply` AND the full typed `doc` — both forms of
      access asked for, through one value, reachable via
      `Meta.current` (a plain always-fresh method, NOT a `given` — see
      the spec's "How code reaches it" for why a `given` genuinely
      does not work for this: confirmed empirically, a plain `given`
      is evaluated once and local re-declaration at the same flat
      scope is a compile error, neither known before this landing
      tested it directly). `run`/`render` emit the `Meta` wiring only
      when a document actually HAS metadata (`hasMeta`), preserving
      self-sufficiency for the common metadata-free case — the first
      cut skipped that check and broke it, caught by
      `TestScalaScriptClassloaderIsolation`'s own minimal-Classpath
      test. A ```yaml fence is now metadata (consumed, not shown in
      `render`'s output) — every other fenced language is unaffected.
      The storefront example now reads its tagline/contact from real
      front-matter instead of a hardcoded second copy.
      specs/okay-script.md "Metadata as context".

## Elsewhere
- [x] ctx-wiring — CLOSED 2026-09-02: the consumer arrived and
      shipped (demo-ctx-wiring — ChatDemo.handler as a
      handlers-awaiting-environment value, genuinely rewired in
      tests) (specs/context-functions.md)
- [ ] ctx-reader-bridge — `(A ?=> B) <-> B ! Reader % A`, one
      Conversion each way; GATED: no consumer named
      (specs/context-functions.md)
- [x] llm-streaming-cut — CLOSED 2026-09-02: the mechanism shipped
      earlier (Cut.guarded/checked/screened) and the consumer landed
      (demo-streaming-cut) — okay-chat's reply/chatRoute take a
      content `policy` alongside the token-budget check
      (specs/llm-agentic.md, Streaming validation)
- [ ] logic-named-cut — GATED on a search consumer
      (specs/backtracking.md)
- [ ] r-restarts — GATED twice: on r-subprocess and on a restart
      consumer; the one resumable-capture case (specs/r.md)
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

## okay-agent: intent classification — after intent-classify (2026-09-03, specs/intent-classify.md)
- [x] intent-other-collapse — LANDED 2026-09-03. Six arms over the same
      24-message fixture, in-repo as `TestClassifyLive` (Live-tagged):
      the answer is examples + a binary in-domain gate, 0.955 macro F1
      over 23/24 decoded replies with `Other` recall 0.83 at precision
      1.00, against 0.587 and recall 0.00 for the prompt as it was.
      Two things fell out that were not the question: the decode rate
      is a PROMPT property (4 -> 23 of 24 decoded, same model, purely
      on how the answer was asked for — a rendered example beats a
      schema), and a harness sentinel must not enter the confusion
      matrix or macro F1 tracks the decode rate instead of the
      classification. Original entry follows.
- [x] intent-other-collapse (original) — the lane's own measurement: declaring an
      `Other` case is NOT enough. On 24 labelled messages the local 4B
      model gave `Other` recall 0.17 with reasoning first and 0.00
      without, absorbing every out-of-domain message into a positive
      class (charged twice -> Request, birthday wishes ->
      Notification). Candidates, in the order I would try them: an
      explicit none-of-the-above instruction in the prompt, `Other`
      examples shown in the prompt, and a separate binary in-domain
      gate ahead of the taxonomy. Measure each against the same
      fixture with `Eval.regressions` — this is exactly the loop that
      rule exists for.
- [x] intent-precedence-rule — LANDED 2026-09-04 as a REFUSAL. The
      design answer (a `Taxonomy[I]` typeclass beside the schema, so a
      tie-break travels as far as the type) was built and measured, and
      stating the rules cost 0.043 macro F1 with every class falling —
      including the class the second rule was aimed at. Not shipped: an
      API whose only measurement says it hurts is an unearned claim in
      code. The four lines are in the lane's history. Original entry
      follows.
- [x] intent-precedence-rule (original) — `Proposal` vs `Request` confused 3 of 6
      in the same run, and it is genuine overlap rather than model
      error ("Can we move Thursday's sync to Friday?" is both). Needs a
      stated precedence rule travelling WITH the taxonomy (a doc
      comment the prompt renders), not a better classifier.
- [x] intent-symbolic-tier — LANDED 2026-09-04, built on the operator's
      word rather than its trigger, and NOT wired in: 112us per message
      (fast enough), but agreement plateaus at 60-64% and does not rise
      with the margin, so the margin is not a confidence signal and
      there is no threshold at which it can safely answer. At margin 0.2
      it would spend ~14 points of end-to-end accuracy to save 55% of
      calls. Original entry follows.
- [x] intent-symbolic-tier (original) — an LU dictionary over `Postings`/BM25 as a
      first pass that answers the easy majority without a model call.
      TRIGGER: measurement shows cost or latency binding on the model
      tier. Linagora's ontology system answers in <150ms with no model
      at all, so the tier is not a rudiment — it is just not yet
      justified here by a number.
- [x] intent-vector-tier — LANDED 2026-09-04, and it EARNS its place,
      unlike the symbolic one. Agreement rises monotonically with the
      margin (80% -> 87% -> 96.3%) where BM25's plateaued at 60-64%: the
      constraint was representation. At margin 0.05 it answers 45% of
      messages at 96.3%, ABOVE the model tier's ~90%, for 12ms of
      embedding plus 90us. Composition is three lines at the call site;
      no wrapper, so the caller sees which call is being paid for.
      Original entry follows.
- [x] intent-vector-tier (original) — class centroids, then a linear probe over
      frozen embeddings, trained from LLM-distilled labels (keep only
      `Conf.High` plus the human confirmations the `Clarify` path
      produces). 18x1024 weights is 72KB; a cosine at 1536 components
      measured 1.04us in `Store.scala`, so ~18us for 18 classes — the
      "sub-millisecond encoder" tier with no dependency and no training
      pipeline. Needs 30-100 examples per class, not the 1k-5k a
      fine-tuned encoder wants. TRIGGER: the symbolic tier starts
      missing paraphrases.
- [x] intent-temporal-slots — LANDED 2026-09-04. `Temporal` parses the
      shapes scheduling mail uses, relative to a reference day passed as
      an argument (a parser that reads the clock cannot be tested), and
      REFUSES everything else rather than guessing — a wrong date is
      acted on, a declined one is asked about. Hinnant's civil algorithm
      rather than month tables, no `java.time`, so the JS build keeps
      it. 13 tests, 3 properties. Original entry follows.
- [x] intent-temporal-slots (original) — a `When` slot takes ISO-8601 and refuses
      anything else through `SIso`, so "next thursday" cannot be
      filled. A Duckling-equivalent over `okay-lex`/`okay-parse` is its
      own lane; until it exists the model does the conversion and the
      schema checks it.
- [x] intent-live-provider — LANDED 2026-09-03, and it REFUTED the
      claim it set out to quantify: the early stop saves 0.0% against a
      real model, under a strict prompt (nothing follows the closing
      brace) and under a prose-inviting one (the value never decodes,
      so the walk runs to the end). The mechanism itself works — proven
      on a counting synthetic stream in the default gate — but a
      classification prompt that says "and nothing else" already buys
      what `cut` would buy. Spec sentence removed rather than softened.
      Original entry follows.
- [x] intent-live-provider (original) — `Classify.prompt`/`read` are tested
      against hand-built and round-tripped values, not a live model.
      The end-to-end run belongs with `TestLive`'s gating, and it is
      what would let `Structured.cut`'s token saving be measured rather
      than reasoned about.
- [x] intent-eval-on-journal — LANDED 2026-09-04. A recording IS a
      journal: `Durable.Entry` + `Rerun.Version` + `FileVersions` held
      it with nothing new invented. 13 minutes of live calls become
      0.046 s in the default gate, reproducing the live report exactly.
      Two guards, both verified by breaking them: the prompt
      fingerprint refuses a stale recording, and `Eval.regressions`
      finally guards something. Original entry follows.
- [x] intent-eval-on-journal (original) — bind `Eval` to a `Rerun` journal so an
      evaluation run is replayable and a regression names the step that
      changed, not just the class that fell. The spec names this as the
      intended fixture; the lane deliberately did not build it, and the
      seam it needs is only that `Eval` takes label pairs from
      anywhere.
- [x] intent-domain-in-names — LANDED 2026-09-03, and it changed the
      recommendation: naming the domain in the case names does the
      gate's work at half the calls (macro F1 0.907, `Other` F1 0.92,
      vs 0.906 / 0.86 for generic names + gate), and the two do NOT
      compose (0.830 together). The gate is now documented as the
      fallback for taxonomies that cannot be renamed. Original entry
      follows.
- [x] intent-domain-in-names (original) — the residue the gate does not catch: one
      of six out-of-domain messages is still absorbed, and the fixture's
      `Other` mixes "not about this at all" (a birthday wish) with
      "another topic in the same register" (a double charge, a
      cancellation). A taxonomy of `Proposal`/`Request`/`Notification`
      with a bare `what: String` never says its domain is meetings — the
      case NAMES carry the domain or nothing does. Try domain-bearing
      case names before adding any prompt machinery.
- [x] intent-fixture-too-small — LANDED 2026-09-03. 120 messages, 30
      per class, domain stated inside the fixture, hard cases marked;
      plus a parallel set of 12 meanings in 6 languages so a language
      effect is attributable instead of anecdotal. The n=24 conclusion
      reproduced (macro F1 0.553 -> 0.906, `Other` F1 0.00 -> 0.86).
      Original entry follows.
- [x] intent-fixture-too-small (original) — at n=24 a difference of one or two
      replies is not a difference, and a mid-lane wording change moved
      an arm by two. Grow `IntentFixture` past the reference's minimum
      (30 per class) before defending any gap in the arms table as real.
- [x] intent-gate-non-english — LANDED 2026-09-04, and it REFUTED its
      own premise. Re-measured on domain-bearing names, the gate does
      not pay in any of six languages: neutral in three, costly in
      three, and its worst damage is in ENGLISH (0.881 -> 0.602), not
      outside it. The "non-English" framing came from having measured
      the gate only against generic names. The language gap itself is
      real and naming does not close it — Russian stays weakest (0.652)
      across two independent runs. Original entry follows.
- [x] intent-gate-non-english (original) — the gate loses PRECISION outside
      English: `Other` precision 1.00 en, 0.75 fr, 0.60 ru, with recall
      1.00 everywhere, so it is pushing genuine in-domain messages out
      rather than failing to catch out-of-domain ones. Opposite
      direction to the English failure. Try stating the domain in the
      gate prompt in the message's own language, or giving the gate the
      same few-shot treatment that fixed the taxonomy prompt; measure
      per language on `IntentFixture.parallel`, not in aggregate.
- [x] intent-decode-rate-residue — LANDED 2026-09-04 as 03cf0da4. Not a
      residue: 9 of 10 failures were ONE malformation, the model closing
      the intent's object a brace too late and swallowing `conf`.
      Declaring `conf` before `intent` took undecodable from 10/120 to
      0/120. The diagnosis was a `groupBy` over failures the harness
      already collected and threw away. Original entry follows.
- [x] intent-decode-rate-residue (original) — 11 of 120 replies still undecodable
      on the best arm (9%). The rendered example took this from 32% to
      9% and then stopped; what remains has not been looked at, and a
      caller cannot tell a hard message from a malformed reply.
- [x] intent-name-sensitivity — LANDED 2026-09-03. The control held: a
      nonsense qualifier (`Zarnic`) is the WORST arm, not a free win, so
      the effect is the domain and not the appearance of deliberate
      naming. A wrong domain (`Shipping`) proved the word is read, by
      damage: it lifts `Other` recall as much as the true domain while
      halving `Proposal` recall, i.e. it pushes meeting messages into
      `NotAboutShipping`. `Other` PRECISION separates right from wrong
      domain (0.92 vs 0.72) where recall does not. Original entry
      follows.
- [x] intent-name-sensitivity (original) — the whole result rests on four
      identifiers, so the obvious question is how much of it is the
      word "Meeting" and how much is any qualifier at all. Try a third
      taxonomy with a DIFFERENT domain word and a fourth with a
      nonsense qualifier: if the nonsense one also lifts `Other`, what
      helps is the model noticing the names were chosen, not the domain
      they name.
- [x] intent-language-gap — LANDED 2026-09-04. Precondition done (12 ->
      30 meanings per language) and it refuted the spec's own ordering
      claim: at n=30 English is best (0.929) and the middle four cluster
      at 0.887-0.895, so "Spanish and French above English" was
      small-sample noise. The gap survives: Russian ~0.19 below English
      across two runs at two sizes. BOTH candidates failed — native case
      names −0.029 on average (helps fr/ru, badly hurts de/es), an
      explicit domain sentence −0.052 (only ru gains). Original entry
      follows.
- [x] intent-language-gap (original) — Russian 0.652 and German 0.813 against
      Spanish 0.914 and French 0.900, with domain-bearing names and no
      gate, so this is not a gate artifact and not a simple
      English-first ordering (English is 0.881, below both Spanish and
      French). Two candidates, worth trying SEPARATELY because they
      cost different things: case names written in the message's
      language, and an explicit domain sentence in the prompt. Measure
      per language on `IntentFixture.parallel`; and grow that set past
      twelve per language first, because twelve supports "there is an
      effect" and not the size of any one number.
- [x] intent-tiebreak-by-example — LANDED 2026-09-04 as a REFUSAL, and
      worse than the prose it was meant to improve on: 0.854 against
      0.866 for prose and 0.909 for neither, with `Request` recall
      collapsing 0.87 -> 0.63. An example of a BOUNDARY generalises
      past the boundary, where an example of a CLASS generalises
      usefully — the first measurement in this line where few-shot
      examples cost anything. Conclusion: the Proposal/Request overlap
      is not fixable from the prompt by either channel; it is a
      labelling question. Original entry follows.
- [x] intent-tiebreak-by-example (original) — the precedence lane's own suggestion
      for what to try before reaching for stated rules again: render a
      tie-break as EXAMPLES of the disputed case rather than as prose
      (few-shot examples are the one lever that has consistently paid
      in this line), and use one rule rather than a list. The prose
      version cost 0.043 macro F1 and diluted every class.

## channel-sentinel-default — DONE 2026-09-04 (d7c69167)

Closed by `SentinelChannel`: the end-marker travels in the channel's
own element slot (`A | Mark`, one documented cast to read it back),
the layer owns `close` so the sentinel cannot be overtaken, and
`Channel.apply` defaults to it — ring under `MaxRing`, `Segments`
above, `StmChannel` only for capacity < 2. The entry sat open for a
day after landing; found by the 2026-09-06 audit. Original entry
follows, as the record of why the layer was chosen over the invariant.

Measured 2026-09-04 (`ChannelGuaranteeBenchmark`, N=4000, cap=1024):

| lane | us/op | contract |
|---|---|---|
| okayStrong (`StmChannel`) | 333.6 | drains on close, termination detected |
| okayWeak (`AbruptChannel`) | 206.0 | close discards, no detection |
| **okayLayered** (`AbruptChannel` + FIFO sentinel) | **177.4** | **same as okayStrong** |
| zioStrong (`Queue[Option]`) | 187.1 | our contract, their queue |
| zioWeak (`Queue` + `take(N)`) | 168.9 | count known up front |

The result that matters is the one that refuted the expectation. The
strong contract costs ZIO 11% (187 vs 169) and costs us 62% (334 vs
206) — so the guarantee was never the whole gap, but the WAY we buy it
is. They express it ABOVE the queue, as one sentinel travelling in
FIFO order behind the buffered elements. We express it INSIDE the
transition, so every send and every receive reads the state that makes
termination derivable, and pays for it whether or not close is ever
called.

Buying it their way and keeping our contract lands at 177.4 — 1.9x
cheaper than `StmChannel` and past `zioStrong`.

The work: an end-marker carried in the channel's own element slot
rather than a boxed `Option` (the benchmark boxes, and still wins),
with the layer owning `close` so the sentinel cannot be overtaken.
Then `Channel.apply` can default to the weak mechanism plus the layer
and lose no promise. Blocked on nothing; wants the two-tier laws
(landed) to hold the line while the default moves.

## channel-bulk-send — DONE 2026-09-06 (feed-offer-first), without the primitive

Closed the other way round from this morning: the caller existed —
`Channel.buffer`'s feed, one `send` per element, 4000 handshakes for
4000 elements — and it did not need `sendManyNow`. `offer` is
synchronous and O(1); the feed now offers in a loop while the ring
takes and parks with one `send` only on the refused element, which
is `sendBlocking`'s own OFFER-FIRST rule applied one level up.

**2 279 774 → 1 411 988 B/op on `elem_effectCallback`, −38%.** `Slot`
and `Async$Await` left the allocation profile's top twelve; the four
`Channel` closure classes collapsed to one. Three laws through a ring
of 2 against thousands of elements: none lost, order kept, a producer
parked on a refusal released by close.

`sendManyNow` stays as landed in d13cfd72 — `private[okay]`, two laws,
and now with the reason it has no caller stated twice: representation
amortises the chunk feed, and offer-first amortises the element feed.

## reopened earlier the same day — the caller exists, and it is the feed

Closed this morning as "no production caller, by measurement". The
measurement that reopens it (channel-batch-floor, counted per side):
on the elementwise channel lane the CONSUMER makes 64 awaits for 4000
elements, and the PRODUCER makes 4000 — `Channel.buffer`'s feed sends
`c.send(it.next())` one element at a time, and each is an
`Async.Await`, a `Slot`, an acceptance callback and the interpreter
steps around them. Every per-element handshake on that lane is on the
send side.

`sendManyNow` is exactly the primitive for a producer holding a batch
of elements for an element channel, and a feed over a strict
collection (`St.iterator`) holds all of them. d13cfd72's caveat still
binds: bulk send LOSES 1.43x against a consumer that is not draining,
because a full ring fails every bulk scan. Here the consumer drains at
62 of 64. So the shape to build is `feed` offering runs through
`sendManyNow` while there is room and falling back to `send` when the
ring is full — and to MEASURE it on both consumer shapes before
believing it, since that is the trade the caveat names.

Expected size, so it is judged honestly: the producer's 4000
handshakes are the `Slot` (43 samples), `Async$Await` (31), the
`Channel` closures (185) and the `Platform` lambda (30) in the
allocation profile — roughly a third of the 672 bytes per element, and
all of it on the producer's fibre.

## as closed this morning — DONE 2026-09-05 (d13cfd72)

Closed by `Ring.pushMany` + `Channel.sendManyNow` (`private[okay]`,
two laws, each with two threads contending for the same claim). The
primitive was the smaller half of the result; two findings outrank it.

`feedChunked` never needed it — it amortizes by REPRESENTATION, putting
whole chunks into a `Channel[Chunk[A]]`, so the absence of a production
caller is the measured answer, not an omission. And the bulk send is
66.9us against a draining consumer (1.71x past `zioChunked`) but 280.4
against an elementwise one — a 1.43x LOSS, because a full ring makes
every bulk attempt fail its scan and fall back anyway. Batch both ends
or neither. Original entry follows.

`Ring.popMany` batched the consumer's head CAS; `push` still takes the
tail one element at a time, so a chunked SEND (`Channel.mergeChunked`,
`feedChunked`) pays a tail CAS per element the way the receive side
used to pay a head CAS. Symmetric fix: claim a run of writable slots
with one `compareAndSet` on the tail, then publish each stamp. Wants
the same contending-producers law the bulk receive got.

## channel-callback-allocation — HALF DONE 2026-09-05 (d13cfd72)

The send half is closed: `Accepted` plus `CanBlock.blockAccepted` end
the boxing of the acceptance answer, so `boxToBoolean` is off the
send path.

What remains is the RECEIVE half, and it remains BY DECISION, not by
neglect: `receiveBlocking` returns `Option[A]` and `End` is
`Either[Throwable, Option[A]]`, so the `Right`+`Some` pair is in the
return TYPE, not in the implementation. Removing it means the
dedicated SAM below — an abstract primitive on `Channel` and every
implementation with it. Do not reopen this as "two allocations per
element"; it is one allocation pair on one side, priced against a
public signature. Original entry follows.

Leaf samples on the elementwise lane: `boxToBoolean` 8% (Function1 is
not specialized on Boolean, so every `sendAsync` callback boxes) and
`Right.apply` 3.4% plus the `Some` beside it (`End =
Either[Throwable, Option[A]]` wraps each element twice). Chunking
makes both per-batch, which is why they were left; they still stand on
the elementwise path. A dedicated SAM with `onValue`/`onEnd`/`onError`
removes both without a cast, but it changes an abstract primitive on
`Channel` and every implementation with it.
- [ ] intent-examples-in-language — the candidate this lane deliberately
      did not confound into itself: the example MESSAGES stayed English
      throughout, so the native-names arm moved one variable. Translating
      the five few-shot examples is untried, and examples OF A CLASS are
      the one lever that has consistently paid here — unlike every prose
      addition, which has now cost four times running.
- [x] intent-symbolic-patterns — ALREADY DONE, closed 2026-09-04: the
      bake-off built it as the `Patterns` tier (88.6-90.9% where a cue
      fires, 58.3% coverage, 96us, no network) and nobody marked the
      entry. Original entry follows.
- [x] intent-symbolic-patterns (original) — the symbolic tier failed as BM25 over
      examples because BM25 matches CONTENT words and an intent is
      carried by function words and syntax ("could you" vs "shall we").
      Linagora's system did not do BM25: it matched lexical-unit
      PATTERNS tied to frames. That is a different mechanism, it targets
      exactly the failure measured here, and it is cheap to try — but
      only worth it if a zero-network tier is wanted, since the vector
      tier already covers 45% at 96.3% for 12ms.

## okay-agent: understanding without a model — after intent-tier-bakeoff (2026-09-04, specs/intent-classify.md)

The goal is a classifier with NO GENERATION on the request path.
Measured so far: linear probe 86.7% at full coverage (one 12ms embed),
centroid 80.0%, kNN 58.3%, chargrams 60.0%, patterns 51.7% (89% where
they fire), BM25 45.0%; the model tier is ~90%. Everything below is
ordered by what it would FIX, not by novelty.

- [x] conversation-runtime — LANDED 2026-09-04 as `Conversation.scala`
      on durable-waiting-on-a-person. Frame/Slot described by the
      caller, a Reply that is a choice, an intake that asks the next
      unanswered slot, re-asks once, reads back, and holds no state of
      its own. The compiler found the design hole: with no rendering
      at ask time `lang` was unused, which meant the language of an
      exchange was stored nowhere — every Say now carries the text as
      it was asked. Original entry follows.
- [x] conversation-runtime (original) — specs/conversation.md. The runtime a
      human-facing conversation needs, with the boundary drawn so a
      caller owns only its own domain: an intake driver over frames and
      slots, a re-ask when a slot cannot read its answer, a read-back
      before anything is written, a language pinned to the exchange,
      and a reply that is a CHOICE rather than a string. Depends on
      durable-waiting-on-a-person for the suspension; without it this
      is another hand-written state machine, which is what it exists to
      stop. The spec carries the incidents from a working
      implementation of the same shape, including the one where a
      re-derived language flipped mid-intake.
- [x] durable-waiting-on-a-person — LANDED 2026-09-04. `OnRepeat.Await`
      is read in BOTH branches (an awaiting operation has no inner
      effect to run, so it is recognised on its first encounter too),
      `Durable.Awaiting` is the control transfer out, and
      `Durable.awaiting(journal)` names the entry a program is parked
      on. Resuming is `complete` plus re-running: no new mechanism.
      Two properties held down by tests — an awaiting operation never
      reaches the inner handler, and the program's own sequence
      decides what runs next rather than the order answers arrived.
      Original entry follows.
- [x] durable-waiting-on-a-person (original) — `Durable` journals INTENT FIRST and
      the answer after, so an `Entry` with `answer = None` is
      structurally a question asked and not yet answered. But recovery
      reads every missing answer as the crash window, for `OnRepeat` to
      resolve; there is no state for "asked a person, waiting, and this
      is normal, possibly for days". With one, a conversation is a
      durable program — ask, ask, act, resumed across a restart from
      the log — instead of a hand-written state machine, which is what
      a consumer built for want of this. Two constraints it must carry:
      replay resumes from RECORDED verdicts rather than recomputed ones
      (a refitted classifier otherwise rebuilds a different
      conversation), and a suspension takes a message that may be a
      correction, an unrelated request or a command rather than the
      answer, so the resumed value is a choice and the handler decides.
      See "Open requests from a consumer" in specs/intent-classify.md.
- [x] intent-taxonomy-value — DONE 2026-09-04 as `Taxon` (0cf1f7c5): a value with `of[I]` from a Schema and `parsed` from strings, deriving a Schema so it rides as data, plus `check` refusing a label the taxonomy does not hold. Original entry follows.
- [x] intent-taxonomy-value (original) — the model tier reads its classes from
      `Schema[I]`, `NoModel.fit` infers them from its training rows,
      and nothing connects the two: the tiers cannot be aimed at one
      taxonomy without aligning it by hand, and a taxonomy that
      arrives as DATA cannot reach the model tier at all. Blocks
      intent-label-distillation from defining classes rather than only
      examples. A `Taxonomy` (classes, optionally examples per class)
      with `Taxonomy.of[I]` as one constructor and a parsed form as
      another. See "Open requests from a consumer" in the spec.
- [x] intent-language-in-fit — DONE 2026-09-04 as `Row.lang` and `ByLanguage.fit` (0cf1f7c5), with a pooled fallback below `minRows` (32, from the learning curve). The MEASUREMENT is deliberately not run: the parallel set has 30 messages per language, so an arm would train on fifteen — see intent-language-fixture-growth. Original entry follows.
- [x] intent-language-in-fit (original) — a training row is `(text, embedding,
      class)` and cannot say which language it is in, so a multilingual
      fit pools every language into one boundary. intent-language-gap
      measured what that costs (0.741 against 0.929) and
      intent-embedding-choice is about to compare encoders PER
      LANGUAGE, which this row shape cannot express. A grouping key,
      not new mathematics; a pooled fallback where a language is too
      thin. WORTH DOING BEFORE the embedding comparison, not after.
- [x] intent-verdict-ranking — DONE 2026-09-04 (8fe8e809), and then CORRECTED by a sibling (fdcf0d97): the ranking I handed back was invented below rank 1, because `blend` asked the probe one class at a time and split the remainder evenly. The seam was right and what flowed through it was not — my tests asserted the shape and never that rank 2 is the second most likely class. Original entry follows.
- [x] intent-verdict-ranking (original) — `Probe.Verdict` carries `margin` and
      `runnerUp`; `NoModel.Verdict` drops both, so an abstaining caller
      knows only THAT it declined. Wanted by an interface that offers
      the two candidates it could not separate, and required by
      intent-active-learning, which samples by uncertainty and needs
      the distribution. The value exists one layer down.
- [x] intent-trained-codec — DONE 2026-09-04 as `Fitted` (c2fc1949): a record with a derived Schema for every trained model, numbers as bytes rather than digits (21KB against 36KB, 1.7x), and round-trip tests that compare PREDICTIONS rather than fields. Original entry follows.
- [x] intent-trained-codec (original) — `Trained` is arrays with no codec, so
      fitting lives wherever loading lives. A caller that compiles its
      vectors at build time wants to fit there too and load weights at
      boot. Makes "no generation on the request path" also mean "no
      fitting on it".
- [x] intent-slot-descriptor — PROPOSED 2026-09-04 and sent for review,
      not declared finished. `Slot[A]` is a name, a question per
      language and a parser whose failure is a RE-ASK; `Frame[I]` holds
      what is filled and answers `missing` in the reader's language.
      `Temporal` is now one implementation of `parse` rather than a
      special case, and `intent-crf-slots` becomes an alternative
      behind the same seam. The descriptor holds no conversation state:
      suspension stays `Conversation`'s, on `Durable`. Original entry
      follows.
- [x] intent-slot-descriptor (original) — `Temporal` parses one slot in one
      language and intent-crf-slots is filed for the general case. A
      slot as a NAME, a question per language, and a parser
      `String => Option[Value]` whose failure is a RE-ASK: then
      `Temporal` is one parser, another language is another parser
      rather than a rewrite, and the CRF lane is an alternative
      implementation of the same seam. Gives "a filled frame" somewhere
      to live.
- [x] intent-label-distillation — LANDED 2026-09-04 for the ZERO-NETWORK
      tiers, as the learning curve required. 320 generated, 182 kept by
      self-consistency (57% — the model disowns 43% of its own labels),
      and chargrams go 60.0% -> 66.7% on held-out HUMAN data when the
      distilled corpus is ADDED to the fixture. Trained on the
      distilled corpus alone it scores 50.0%, so this is a supplement
      and not a substitute: the model's writing has a different
      distribution from real messages. 66.7% is now the best
      no-network number, above the static table's 63.3%. Original
      entry follows.
- [x] intent-label-distillation (original) — REPRIORITISED 2026-09-04 by
      intent-learning-curve: NOT the one that moves the probe, whose
      curve is flat past 32 examples. It is the lane for CHARGRAMS,
      which are still climbing at 65% and are the only candidate for a
      classifier with no network at all. The text below was written
      before the curve and its premise about the probe is refuted.
      ORIGINAL: Every
      tier here is fitted on 60 labelled messages, and the probe's
      86.7% is a data limit rather than a method limit: it fits 4096
      weights on 60 rows. Use the model OFFLINE, once, to label a large
      unlabelled corpus (its own accuracy is ~90%, and label noise at
      that level is survivable), keep only `Conf.High` plus whatever a
      human confirmed, and refit. This is the reference's own advice
      ("few-shot LLM as a bootstrap for data generation") and the only
      route by which a no-model classifier reaches model accuracy.
      TRIGGER: none needed — it is the cheapest large gain available.
- [x] intent-learning-curve — LANDED 2026-09-04, and it OVERTURNED the
      plan it was meant to confirm. The probe flattens at ~32 examples;
      32 to 60 moves it 81.7-86.7%, which is noise. The centroid, with
      three orders of magnitude fewer parameters, flattens in the same
      place — a signal ceiling, not a capacity one. So labels are NOT
      the binding constraint for the embedding tiers and
      intent-embedding-choice moves ahead of intent-label-distillation.
      Chargrams are still climbing (30 -> 65%) and are the tier that
      distillation should feed. Original entry follows.
- [x] intent-learning-curve (original) — before distilling, measure what more data
      is worth: refit the probe at 15, 30, 45, 60 examples and plot.
      If the curve is still climbing steeply, distillation pays; if it
      has flattened, the ceiling is the representation and
      `intent-embedding-choice` is the lane instead. One afternoon,
      no new code, and it decides which of the two to fund.
- [x] intent-embedding-choice — PARTLY LANDED 2026-09-04, and blocked on
      installation rather than code: exactly one embedding model is
      served and the gateway refuses any other id with 400. Established
      anyway that the ceiling IS representational — the model tier and
      the probe share ZERO errors out of 60, so the signal is in the
      text and the vector is losing it — and that framing moves the same
      model 6.6 points (81.7% to 88.3%), with a SHORT classify
      instruction the best of four. Original entry follows.
- [x] intent-embedding-choice (original) — every tier above 80% goes through ONE
      embedding model, and the Russian gap (0.741 against English's
      0.929) is plausibly that model's multilingual quality rather than
      anything in this code. Swap in a second embedding model behind
      the same seam and re-run the bake-off per language. Cheap, and it
      is the only way to tell a representation problem from a
      classifier problem.
- [x] intent-rule-induction — MEASURED 2026-09-07, not a replacement
      at this corpus size: `Induced` (RIPPER-lite — literals are words,
      adjacent pairs and either at the start; grown by FOIL gain on two
      thirds of the training half, pruned on the third, kept on the
      whole half at a support and a precision floor; deterministic; a
      `Trained` is rules a person can read) beside `Patterns.meeting`
      on the same held-out rows: hand-written fire 53.3% at 90.6%;
      induced at floor 0.8 fire 51.7% at 67.7%, at floor 0.9 (the
      default) 11.7% at 85.7%; support 3 or 4 is worse on both. At
      sixty rows induction buys coverage or precision, not both — the
      corpus is the limit, as intent-static-embeddings and the learning
      curve found for the other tiers; the tier stays, with the grid
      in the suite, for the corpus that grows. Original: patterns are
      88.6-90.9% accurate where
      they fire and fire on only 58.3% of messages, and the cues are
      hand-written. Induce them instead (RIPPER-style: grow a rule,
      prune it against held-out data, repeat) so coverage grows with
      the corpus rather than with someone's patience. Keeps the zero-
      network property, which nothing else above 60% has.
- [x] intent-tfidf-word-linear — DONE 2026-09-07: `WordTfIdf` (a
      vocabulary and IDF fitted on the training half, the vector into
      `Probe`), `TestWordTfIdf` in the default gate beside
      `TestCharGrams`. Same split, same session: **61.7% against
      chargrams' 65.0%** on English (the 60.0% below is the older
      fixture) — so the n-gram tier's number is about having a linear
      model, not about characters, to within three points. Per
      language (15 rows each, thin): tf-idf en 53 / fr 67 / de 27 /
      es 27 / ru 40 / ja 27; chargrams en 40 / fr 47 / de 40 / es 20 /
      ru 53 / ja 60 — characters win exactly where words are not the
      unit (no spaces in ja, inflection in ru), words win on fr.
      51 us per message, a 66 ms fit, 303 words. Original: the
      classical baseline nobody ran:
      word-level TF-IDF into the same logistic regression. It sits
      between BM25 (45.0%) and chargrams (60.0%) in what it sees, and
      it is thirty lines given `Probe`'s optimiser. Worth it to know
      whether chargrams' 60% is about characters or just about having
      a linear model at all.
- [ ] intent-fasttext-subword — subword embeddings TRAINED on the
      corpus plus a linear head, i.e. fastText's actual algorithm in
      plain Scala. Bridges chargrams (language-agnostic, no network,
      60%) and the probe (86.7%, needs a server): a trained
      representation that still ships as an array. Only worth it if
      `intent-embedding-choice` says the server is the problem.
- [ ] intent-grammar-parse — intent by GRAMMAR over `okay-lex` and
      `okay-parse`, the way `Temporal` does dates: deterministic,
      explainable, and refusing rather than guessing. Expensive in
      rules, and the honest reason to want it is a domain where a wrong
      answer is worse than no answer.
- [ ] intent-crf-slots — sequence labelling for the frame's SLOTS
      (who, when, where) rather than its class. `Temporal` fills one
      slot with a parser; the general case is a tagger, and a CRF is
      the classical one. Only after the class problem is settled.
- [ ] intent-active-learning — labels are the bottleneck everywhere
      above, so choose the next ones to label by uncertainty rather
      than by order. Directly compounds with `intent-label-distillation`
      (the model labels, a human confirms the uncertain ones), and
      needs the calibrated confidence `intent-no-model` is building.
- [ ] intent-ensemble-weights — `NoModel` blends the probe with the
      pattern tier using ONE fitted weight from a six-point grid,
      because sixty rows cannot support a fitted second-level model.
      When the corpus grows (see distillation), replace the grid with a
      real stacking model and measure whether it beats the blend.

## channel-chunk-batch-size — REFUTED TWICE 2026-09-06: the consumer already batches at 62 of 64

Taken as channel-batch-floor on the finding that `receiveMany(64)`
hands back ONE element — "4022 handshakes for 4000 elements". Built:
a watermark on the four push-side wakes, a dwell timer as the latency
bound, `drained(atLeast, dwellMillis)`, five laws, and a probe that
priced the dwell (plain 0.2ms; floor16 with dwell 1/5/20ms: 2.0 / 8.5
/ 28ms — `Timer.after` on Loom adds ~1ms plus 40% over the asked
sleep). Then the handshakes were counted PER SIDE, with a counting
`Handler[Async]` wrapped around the consumer's `runWith` only:

| read | awaits (N=4000) | elements per await |
|---|---|---|
| `drained` (default) | 64 | 62.5 |
| `drained(16, 1)` | 64 | 62.5 |
| `drained(64, 1)` | 64 | 62.5 |

**The default already takes a full batch.** There is no ping-pong and
nothing for a floor to do; the whole lane was reverted, laws and all.
The "4022" was a GLOBAL counter in `CanBlock.block` that summed the
producer's 4000 sends with the consumer's 64 receives (4064 − 64 =
4000, one per element sent) and was read as the consumer's. See
`channel-elementwise-wakeups` for the correction and
`channel-bulk-send` for where those 4000 actually live.

What survives from the original entry below is nothing about batch
SIZE. Its numbers predate two rounds of channel work and its central
claim is now measured false; keep it only as the record of the idea.
The one durable artefact is the method: count per side, never with a
static counter both fibres can reach.

## the original entry — PREMISE REFUTED, the idea survives

Audited 2026-09-06. The entry opens by naming "the one lane we lose to
`zio.Queue` — `zioStrongChunk` at 128.0". We do not lose it: d13cfd72
re-measured both sides on one granularity axis and the layered chunked
lane reads **115.9 against zioStrongChunk's 124.0**. Whoever takes
this must not take it to close a gap that is already closed.

What survives is the MECHANISM, which is worth having on its own
terms: a ring wakes a receiver on every push, so average elements per
bulk receive are 43.5 where `StmChannel`'s transaction hands over
363.6. The watermark direction is still the cheap one, and is now
about throughput headroom rather than a deficit. Original entry
follows; its comparison numbers predate the send fast path and the
chunked feed, so re-measure before quoting any of them.

`SentinelChannel` wins elementwise (208.9 against `StmChannel`'s
300.1) and is level chunked (175.3 against 172.3), so the one lane we
lose to `zio.Queue` — `zioStrongChunk` at 128.0 — is still open.

The lever is measured and it is not per-operation cost. Average
elements per bulk receive: `StmChannel` 363.6, `AbruptChannel` 65.6,
`SentinelChannel` 43.5. A ring wakes a receiver on every push, so the
consumer returns before the buffer accumulates; `StmChannel`'s
transaction hands over whatever the buffer holds, and its consumer
therefore takes it in eleven operations rather than ninety.

Two directions, neither obviously right. Hold a woken receiver back
until either a small dwell has passed or the ring has n elements —
throughput bought with latency, and the flush machinery from
`source-merge-chunked` already exists to bound it. Or wake on a
watermark rather than on every push, which costs nothing in latency
when the consumer is already behind and nothing at all when it is
keeping up. Measure both; the second looks cheaper.

Related: this is also why `Ring.pushDeciding` takes a flag and not a
function. Anything between the claim and the publish truncates a
concurrent `popMany` scan, which counts CONSECUTIVE published slots —
a closure there cost 65.6 elements per batch down to 43.5.

## channel-elementwise-wakeups — CORRECTION 2026-09-06 (later the same day): the 4022 was both sides

The count below — `fast=4022 slow=42 parks=42`, read as "the consumer
calls `block` once per element" — came from a STATIC counter in
`CanBlock.block`, which both fibres reach. Counted per side with a
`Handler[Async]` wrapped around the consumer's `runWith` alone, the
consumer performs **64 awaits for 4000 elements** — a full batch every
time. The other 4000 are the PRODUCER's: `Channel.buffer`'s feed does
`c.send(it.next())` once per element, and every `send` is an
`Async.Await` through `block`. 4064 − 64 = 4000, one per element sent.

So the conclusions here that rested on the consumer stand — it does
not park, the watermark was never the lever, the 672 bytes are the
representation — but the per-element HANDSHAKE cost is real and it is
on the send side. That is `channel-bulk-send`'s territory, reopened
with this evidence. The `AtomicBoolean` removal stands too: it saved
one object per handshake on whichever side made it.

## the entry as first written — MEASURED 2026-09-06: the premise is wrong, and the cost is the representation

Taken, counted, and the entry's own framing does not survive.

**The consumer does not park.** Counters in `CanBlock.block` on this
lane's shape, N=4000: `fast=4022  slow=42  parks=42`. Once per element
`block` runs, and 99% of the time `register` has already completed —
the element was there, there was nothing to wait for. So "one unpark
per element on the consumer's critical path" is not what happens, and
the entry's second proposal, waking on a WATERMARK, is not the lever
here: there are 42 wakeups to save, not 4000.

That also retires the reading of the `-prof stack` profile that
promoted this entry. Three quarters of thread time is parked, but it
is the producer fibre and idle scheduler threads — not the consumer.
Counting the consumer directly is what settled it; the sampler could
not.

**What the handshake did cost was an allocation, and one of them is
gone.** `Slot` and `BoolSlot` held `filled` as an `AtomicBoolean`
although it is only ever SET and READ — never compare-and-set — so a
plain `@volatile var` has exactly the same memory semantics and one
fewer object per handshake. Verified by counting bytes, which is the
one measurement a loaded box cannot distort:

| variant | B/op (two runs) | GC count |
|---|---|---|
| `AtomicBoolean` | 2 689 282 / 2 689 255 | 25 / 24 |
| `@volatile` | 2 624 432 / 2 624 424 | 19 / 20 |

64 840 B/op against 4064 x 16 = 65 024 predicted — 0.3% off — and a
fifth fewer collections. **The timing effect was NOT measured**: an
alternating A/B ran while a sibling's job took the box from load 8.5
to 48, and its numbers (240 -> 1749 -> 2191 -> 6607, error bars to
±9438) are void and are recorded here only so nobody mistakes them for
a result. The change lands on the allocation count and on being
semantically identical, not on a speed claim.

**The real number this lane found is 672 bytes per element.** 2.62 MB
per operation over 4000 elements, and the 16 bytes above are 2.4% of
it. A JFR allocation profile says where the rest goes — and it is not
one hole:

| class | samples |
|---|---|
| `Free$Bind` | 161 |
| `Channel` lambdas (four distinct classes) | 185 |
| `Right` + `Tuple2` (uncons's answer) | 140 |
| `Free$Inject` / `Free$Pure` | 59 / 52 |
| `Slot` | 43 |
| `Async$Await` / `Drain` / `Writer$Say` | 31 / 27 / 21 |

Free nodes are 272 samples and closures about 248. That is the
REPRESENTATION: a fragment of program is built and discarded for every
element. There is no single allocation to remove, which is the same
conclusion `channel-per-element-effect-cost` reached from the other
side, and the way not to pay it is not to go per element —
`bufferChunked` already pays it once per 256 elements and reads 19.66.

**Left open, with the trade named.** `Drain` batches by holding a
chunk and calling `receiveMany(64)`, yet `block` runs once per
element: the batch comes back with ONE element, because producer and
consumer ping-pong and the ring never accumulates. Making the batch
real means holding a woken receiver for a dwell or a watermark —
throughput bought with latency, which is `channel-chunk-batch-size`,
and it should be taken there with that trade stated, not here.

## receive-blocking-path-length — DONE 2026-09-06 (§17g): the head CAS was 35%; a single-consumer ring takes it, −25% elementwise

Profiled first (JFR): the head compare-and-swap in `Ring.pop` is 35%
of the elementwise consumer, the pop half of it. Landed
`Queues.strong[A].bounded(n, singleConsumer = true)` — `Ring`'s flag,
`pop`/`popMany` moving the head by a release store — and the actor's
default mailbox built with it. `okaySentinelElem` 202.9 → 152.1 us at
the same bytes; chunked and both actor regimes unchanged within their
bars. Laws: the new flavour answers for ten, the
contending-consumers law is recorded as not claimed. The `Handoff` per
call (8%) and the slot read (14%, the element itself) are what
remains; not taken. Original entry:

### as filed — the 3.7x between elementwise and chunked is path, not parks and not bytes

Filed 2026-09-06 by `channel-elementwise-wakeups`' closing count.
`okaySentinelElem` 205.9 us and `okaySentinelChunk` 55.0 allocate the
same 77–80 bytes per element and neither side parks (§17f), so the
150 us between them — ~37 ns per element — is what one
`receiveBlocking` does over a chunk's per-element share: a `Handoff`
made per call (`CanBlock.handoff()`), `receiveInto`'s scan with its
shared reads (`ended`, `reached`, `endPending`, the senders' queue
head), the `Mark` match, and `await`'s check of `filled`. Candidates,
to measure not assume: a per-thread `Handoff` reused across calls
(one thread blocks on at most one at a time; JVM/Native only, JS has
no `CanBlock`); the shared reads folded into one; nothing that adds
a read of the producer's line. The A/B lane is `okaySentinelElem`
against `okaySentinelChunk` under `-prof gc`, and `TestChannelLaws`
is the law.

## the original entry — MEASURED AND CLOSED 2026-09-06: the wakeup per element is not there

Counted, on the entry's own shape (`okaySentinelElem`: N=4000,
cap=1024, a virtual-thread producer, the consumer on
`receiveBlocking`), 100 runs after 200 warmup, three processes:
**sender wakeups per element 0.000–0.001, sender parks 0.003–0.005,
receiver parks 0.000.** Twelve to twenty producer parks per four
thousand elements, and the consumer never parks at all. There is no
unpark on the consumer's critical path to amortise; a watermark wake
policy would remove nothing. The harness re-taken the same day (first
time since the chunked feed, `-prof gc`, two forks): `okaySentinelElem`
205.9 ±17.5 us / 307 668 B against `okaySentinelChunk` 55.0 / 318 318
B — the SAME 77–80 bytes per element on both, so the 3.7x is path
length per `receiveBlocking`, not allocation and not parking;
`okayStrongElem` (StmChannel) 250.1, so the "SentinelChannel behind
StmChannel" line below is stale too, and the 208.9 → 268.7 regression
does not stand (§17f). What is left of the elementwise cost is the
~37 ns a `receiveBlocking` spends over a chunk's share — a `Handoff`
made per call, the scan, the shared reads — and that is a different
entry if anyone wants it (`receive-blocking-path-length`). The
three-quarters-parked profile that promoted this entry was taken on
another lane at another time; it did not survive a count. Original
entry follows.

### as promoted — OPEN, the PRIMARY lane

Promoted 2026-09-06 by free-cont-stack, which went looking for the
per-element cost in the interpreter and found it here instead. A
`-prof stack` of the elementwise lane puts 58.3% of thread time in
WAITING (86% of that `Unsafe.park`) and another 16.8% in
TIMED_WAITING, all park — three quarters parked — against 24.9%
RUNNABLE, of which the entire effect machinery is about 10 points.
`channel-per-element-effect-cost` closed as an interpreter lane and
points here; read it for the counts and the caveats.

Audited 2026-09-06: still genuinely open — one unpark per element is
on the consumer's critical path and nothing since has addressed it.
But every number below predates the chunked feed (9fe22fdc), and they
come from the guarantee/granularity harness, NOT from the idiomatic
one where the elementwise lane reads 209.3 today. Do not cross the two
sets, and do not assume the 208.9 -> 268.7 regression still stands:
re-run this entry's own harness before deciding the size of the prize.

`channel-send-fastpath` took the chunked lane from 175.3us to 58.7 and
cost the elementwise one 208.9 -> 268.7, which also puts
`SentinelChannel` behind `StmChannel` on that axis (235.5).

The cause is not the extra failed `offer`. It is that the producer can
now saturate the ring, so every send parks and every pop wakes a
sender — one unpark per element on the consumer's critical path. A
chunked consumer amortizes those wakeups across a whole batch; an
elementwise one pays one each.

Worth trying, cheapest first. Give the RECEIVE side the same fast path
the send side just got: `receiveBlocking` still allocates a handshake
slot per element, and in the elementwise shape the consumer is the
bottleneck, so speeding it may pay twice — directly, and by keeping
the ring off its full mark. Failing that, wake senders on a watermark
rather than on every pop, which is the same idea as
`channel-chunk-batch-size` read from the other end.
- [x] intent-second-embedder — LANDED 2026-09-04 and it settled the
      ceiling question: 4B (2560 dims) does NOT beat 0.6B (1024). Bare
      it scores ten points lower, but that is framing — the larger
      instruction-tuned model gains +8.3 from a classify instruction
      against the small one's +1.6 — and framed it reaches 85.0%
      against 88.3%. The mechanism is the learning curve's: 2.5x the
      weights on the same 60 examples, in a regime where data binds.
      So 88.3% is the TASK at this data size, not the vectoriser.
      Original entry follows.
- [x] intent-second-embedder (original) — install a second embedding model and
      re-run the bake-off and the per-language table; this is the
      experiment intent-embedding-choice could not run. Candidates
      against our constraints (local, MLX, multilingual):
      `Qwen3-Embedding-4B/8B` (same family, direct swap), `BGE-M3` and
      `multilingual-e5-large` (multilingual strength, for the Russian
      arm), `jina-embeddings-v3` (has a CLASSIFICATION adapter, not
      just retrieval), `gte-multilingual-base` (half the size).
- [x] intent-static-embeddings — LANDED 2026-09-04. Distilled from our
      own teacher rather than downloaded, so no foreign tokenizer had
      to be matched. Words alone cap at 51.7% even with complete
      vocabulary coverage — a bag of words cannot tell "could you" from
      "we could", the same mechanism that sank BM25 — and adding
      adjacent PAIRS lifts it to 63.3%, the best no-network number so
      far, above chargrams' 60.0%. The remaining 23 points to the
      teacher are CONTEXT, which a static table cannot have. Original
      entry follows.
- [x] intent-static-embeddings (original) — `model2vec`/`potion`: a transformer
      distilled into a LOOKUP TABLE, so there is no neural inference at
      request time at all — roughly 30MB, no server, no round trip.
      The only candidate that could give embedding-grade accuracy with
      the zero-network property chargrams have, and it drops straight
      into `Centroid` and `Probe`, which do not care where a vector
      came from. Directly serves the no-generation goal.
- [ ] intent-instruction-prefix — a short "Classify the intent of this
      message: " prefix measured +1.6 (probe) and +3.3 (centroid), and
      the spread across four framings was 6.6 points. Both are at or
      near the noise floor on 60 messages: re-measure on the grown
      fixture before making it the default, and keep the finding that
      LONG instructions cost (81.7% for the e5-style one).
- [x] intent-static-trigrams-and-pca — two obvious extensions of the
      static table, both filed rather than guessed: adjacent TRIPLES as
      well as pairs (pairs were worth 11.6 points, and the same
      argument applies once more with diminishing returns and a bigger
      table), and `model2vec`'s PCA step to cut 1024 dimensions to
      256 — 1303 units already cost 5.2MB as float32, and a production
      vocabulary of 30k would be 120MB. MEASURED AND LANDED
      2026-09-07: one run, same split, baseline re-measured beside —
      triples +5.0 probe / +11.7 centroid; PCA 256 keeps 91.5% of
      the variance at a quarter of the bytes and GAINS five probe
      points (a denoising); together 68.3% / 66.7% at 2.1 MB against
      61.7% / 53.3% at 5.3 MB. `Static.units3`, `Static.fitPca` /
      `projected` / `variance`; `TestStaticMore` (Live) holds the
      table. 30k units ship at 30 MB (256) or 15 MB (128).
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
- [ ] intent-4b-with-more-data — the 4B embedder is worse at 60
      examples because 2560 dimensions need more of them, which is a
      prediction rather than a defeat: re-run the learning curve on
      BOTH embedders and find where the lines cross. If the 4B
      overtakes past some n, it is the right vectoriser for a
      distilled corpus even though it is the wrong one today.
- [ ] intent-distil-more — 320 generated messages bought chargrams 6.7
      points and the curve was still climbing when the fixture ran out.
      The generator is resumable, so this is machine time rather than
      work: raise the target, re-filter, and find where the gain stops.
      Also worth trying on the STATIC table, which was not fed here
      because its vocabulary would have to be re-embedded — a second
      pass over the teacher rather than a change of method.
- [ ] intent-distil-diversity — the distilled corpus alone scores TEN
      POINTS below a human fixture a third its size, which says its
      distribution is narrow rather than its labels wrong. Prompting
      for a persona, a register or a length before each batch is the
      cheap thing to try; measuring the corpus's own diversity (say,
      distinct trigram ratio against the human fixture's) is the honest
      way to tell whether it worked.
- [ ] intent-distil-dose — a little distilled data is worth ten points
      to the centroid (80.0% -> 90.0% at +40 rows) and more is worse,
      monotonically, down to 78.3% at +320. The optimum was found by
      accident between two arms; find it properly, and find out whether
      the self-consistency filter (which kept 182 of 320 and was NOT
      applied in that run) moves it. Also worth asking whether the
      right knob is a dose at all or a WEIGHT — distilled rows counted
      at less than one in the fit, which a centroid can express and a
      grid over doses cannot.
- [x] intent-centroid-reconsidered — LANDED 2026-09-04 and it RETRACTED
      the previous lane's headline: the centroid's 90.0% was measured on
      BARE embeddings, and with the classify instruction the same recipe
      gives 83.3% -> 85.0%, a peak gain of +3.4 at 20 rows instead of
      +10 at 40. Both are near the noise floor on 60 test messages; what
      survives is the DECLINE at large doses. The 4B and
      instruction-prefix conclusions stand for the centroid too.
      Original entry follows.
- [x] intent-centroid-reconsidered (original) — the centroid at 90.0% is now the
      best result in the programme, beating the probe it was supposed
      to be a baseline for, at four vectors against 4096 weights. Every
      arm in the bake-off and the embedder comparison was read as
      "probe first"; re-read them with the centroid as the subject, and
      re-run the ones where the conclusion turned on the probe's
      number.

## channel-sender-livelock — DONE 2026-09-06 (this lane)

`Ring.hasRoom` answers from the stamp of the position the tail is
about to claim, exactly as this entry prescribed, and it landed with
the relaxed queues (cb51748c). But it was only WIRED on the default
lane: `SentinelChannel` asks `hasRoomAt(route)`, while
`AbruptChannel:101` still subtracted `ring.size < ring.capacity`. The
audit found the fix half-applied and finished it — the weak channel
now asks `ring.hasRoom` too.

The general lesson, this being the fifth defect of one family: adding
the right primitive is not the fix; every caller of the wrong one is.
Grep for the SHAPE (`size <`, `isEmpty`) after landing its
replacement. Original entry follows.

`receiveAsync` used to recheck `isEmpty` before parking, which counts
a CLAIMED-but-unpublished position as "something is there", so the
consumer spun instead of waiting and starved the very publisher it
waited for. Fixed by asking `hasReady` instead.

The sender side still has the mirror: it rechecks `ring.size <
ring.capacity`, and `size` is `tail - head`, which counts a position
whose slot has been popped but whose stamp has not yet been
republished. So a sender can be told there is room, fail its push, and
go round again. The window is two stores wide on a bounded ring and it
has never been observed, but it is the same shape and deserves the
same treatment: a `hasRoom` on `Buffer`, answered from the stamp of
the position the tail is about to claim rather than from a
subtraction.
- [x] intent-state-the-framing — LANDED 2026-09-04. `Conditions` prints
      embedder, framing, split, corpus and any extra beside every live
      row, with no overload that omits them, so a row cannot be written
      without its terms. Wired into the two suites whose rows were
      compared across lanes. The first version DERIVED the distilled
      count and printed `distilled=260` for an arm with no human rows —
      a condition that lies being worse than one that is missing, the
      counts are passed now. Original entry follows.
- [x] intent-state-the-framing (original) — two measurements an hour apart
      disagreed because one silently changed whether the embedding was
      framed with a classify instruction, and nothing in either printed
      row said which. Make every live arm print its conditions —
      embedder, framing, split, corpus — beside its number, so a table
      cannot be compared against one taken under different terms. This
      is cheap, and it is the defect behind the retraction in
      intent-centroid-reconsidered rather than a nice-to-have.
- [x] intent-split-other — MEASURED AND DECLINED 2026-09-05. Carving
      the bin into Social/Support/Errand takes `Other` recall from
      46.7% to 6.7% (composite 26.7%) and all three new classes score
      F1 0.00, because the training half has 15 `Other` rows and three
      ways leaves 4-6 each. A two-way carve also collapses (13.3%), so
      it is row count and not where the line is drawn. All three
      remedies are now measured and the status quo wins. Original
      entry follows.
- [x] intent-split-other (original) — NOW WITH A NUMBER (2026-09-05): `Other`
      scores recall 0.47 in the shipped composite, the worst class by
      a distance, and it is the one whose failure routes out-of-domain
      traffic into a meeting intent. Original entry: `Other` is one
      diffuse bin (mean pairwise
      cosine 0.55-0.645 against 0.68-0.78 for every real class) and it
      carries two thirds of the probe's lead over the centroid while
      being a quarter of the rows. Converting it to an abstention is 20
      points WORSE (68.3% against 88.3%), so the remaining option is
      the consumer's other one: split it into named classes that are
      individually coherent — Gratitude, SupportIssue, Unrelated, or
      whatever the corpus actually holds. Cheap to try on the existing
      fixture by relabelling, and it would let every per-class number
      in the spec be read without an asterisk.
- [x] intent-russian-rows-fixed — LANDED 2026-09-04. All three hazards
      the consumer named were present: the person marker carrying the
      class on one letter, eight Requests in three templates with a
      duplicated pair, and three calques. Ten rows rewritten by
      CONSTRUCTION and one meaning replaced across all six languages.
      Russian fell 86.7% -> 73.3%, which is what fixing a fixture that
      was flattering itself looks like.
- [x] frame-walk-end-to-end — LANDED 2026-09-05 as okay-demo's
      TestWalk: one message to one produced booking, through the real
      tiers, a real journal, a simulated process death and a
      read-back, asserting the VALUE, the DIRECTION and that anything
      was produced. It caught its own instance of the defect it exists
      for on the first run. Original entry follows.
- [x] frame-walk-end-to-end (original) — the consumer's third point, and the one
      I cannot argue with: okay-frame and okay-intent have ONE caller
      and it is a classifier demo. Both defects they hit today lived
      BETWEEN two correct code paths with 237 unit tests green —
      contacts shown to the wrong side, and a notification that was a
      claim rather than a call. A test that walks a whole exchange
      (classify, fill from the message, ask, answer, assume, confirm,
      act) would have caught both and is worth more than another tier.
- [x] intent-per-class-not-aggregate — LANDED 2026-09-05. `Eval` got
      `support`/`balance`/`majorityBaseline`/`worst`; the shipped
      model's tests print per class and assert both the balance
      (majority baseline < 0.40) and a per-class floor (F1 >= 0.50).
      Found what the total hid: `Other` recall 0.47. Original entry
      follows.
- [x] intent-per-class-not-aggregate (original) — the consumer's imbalance
      finding: they filled a corpus hole, one class grew to 137 of 184
      rows, a probe leaned to the majority, and "сегодня в москве шёл
      дождь" came back as a REQUEST at 0.90 — while HEADLINE ACCURACY
      ROSE, 95.8% to 96.2%, because accuracy on an imbalanced corpus
      rewards predicting the biggest class. Every aggregate this
      module publishes has the same exposure, starting with the
      shipped model's 76.7%. Print per-class numbers and the class
      BALANCE beside every total; `Eval` already computes per-class
      scores, so this is a reporting lane, not a measurement one.
- [x] frame-rebind — DONE 2026-09-07: `Frame.rebind(rebuilt*)` answers
      a `Rebound` — the frame re-bound by NAME to the rebuilt
      descriptors, each stored answer re-read from its text by the new
      parser; `rederived` lists every value that came out different
      (name, text, before, after), `lost` the names the new descriptor
      could not read (their words kept in `unread`) or no rebuilt slot
      carries; `clean` when neither. Laws in `TestFrame`; `TestRebind`
      in okay-intent shows the hazard as filed — "next Tuesday" against
      a reference day a week later is a different date, and the move
      is reported, not made silently. Original: after a restart a
      caller rebuilds its `Slot`
      values, and `valueOf` matches by IDENTITY, so a frame read back
      from a journal cannot be read with the new descriptors unless
      the caller threads the rebuilt ones through everything.
      `TestWalk` hit this and the rule ("one descriptor value per
      exchange, passed with the frame") is documented, but a
      `Frame.rebind(slots)` would make the restart case ordinary. THE
      HAZARD, which is why it is not built yet: re-deriving a value
      means re-parsing the stored text, and "next Tuesday" against a
      new reference day is a DIFFERENT DATE — the exact defect
      intent-frame-typed-values removed. It must be an explicit
      request that reports what it re-derived, not a silent
      convenience.
- [ ] intent-other-more-rows — the answer that survived
      intent-split-other, and the second lane today to land on corpus
      size. `Other` is under-predicted (recall 0.47, precision 0.78)
      on 15 training rows, and under-prediction is the failure that
      matters: out-of-domain traffic routes INTO a meeting intent.
      Neither abstention nor splitting fixed it; both made it worse.
      What is untested is the obvious thing — more `Other` rows, and
      whether recall moves with them. The suite that would show it
      already exists (`TestSplitOther`, `TestModels`), so this is a
      corpus lane with its measurement already written.
- [x] intent-english-corpus-twins — LANDED 2026-09-05. Three
      near-twin pairs rewritten, the guard asserted on `labelled`, the
      artifact regenerated and every published number re-measured:
      composite 76.7% -> 75.0%, near half 86.7% -> 83.3%, far half
      unmoved. Original entry follows.
- [x] intent-english-corpus-twins (original) — `TestFixtureHygiene` found three
      near-twin pairs in `labelled`, the English corpus the shipped
      model is fitted on and every published number is measured
      against ("Suggestion: we meet on Monday at 9" against "Suggest
      we cancel Monday and meet Wednesday instead"; two Requests that
      both say "send me the agenda"; two Notifications about a room
      change). Fixing them changes the shipped artifact and every
      number quoted from it, so it is its own lane with its own
      re-publish rather than a tidy-up.
- [x] intent-span-runaway — DONE 2026-09-07: `Reading.grounded
      (message)`, the decoder-side guard — a span is kept only if its
      text is in the message (grounding) and its stretch is not
      already covered by a span kept before it (distinctness, the
      check the twenty-span run needed: every text was a real
      substring). Matching on lowercased, whitespace-flattened text,
      first occurrence, the model's order kept. Four laws in
      `TestClassify` (the twenty-span cycle collapses to two, an
      ungrounded span drops, a clean reading is the identity, overlap
      is judged on stretches); the live suite guards before it counts
      and prints what the guard dropped. `decide` unchanged. Original:
      nothing bounds the number of spans a
      `Reading` may carry. One live run answered a nine-word message
      with TWENTY, cycling Notification+Request+Other+Proposal five
      times; it did not recur on a second run. GROUNDING would not
      catch it — the repeated texts were real substrings of the
      message — so the check that would is DISTINCTNESS: no two spans
      covering the same stretch. Worth pairing with the grounding rule
      (every span's text must be in the message, which held 12/12
      across two runs) as decoder-side guards rather than prompt
      wording.
- [x] mail-loopback-tls — LANDED 2026-09-05. Needed `Tls.server`, the
      missing mirror of `Tls.client` for upgrading an already-accepted
      socket mid-protocol; with it, the client's whole STARTTLS path
      runs against a real handshake. Original entry follows.
- [x] mail-loopback-tls (original) — okay-mail's STARTTLS path is covered by the
      pure protocol tests and NOT over a real socket: the loopback leg
      runs in the clear, because a TLS server needs a certificate and
      a key and this module has no such fixture. okay-tls's own tests
      have one; borrowing that shape would close it.
- [ ] mail-consumer-adoption — the consumer who asked for okay-mail
      replaces `Identity.console` with it. Not my lane to do, but the
      one that tells whether the seam is right: their `deliver` is
      `(Channel, String, String) => Unit` and `Mail.Send` has to plug
      in without anything else changing, which was their stated
      requirement.
- [x] intent-window-by-dim — MEASURED 2026-09-07 (`TestWindowByDim`,
      specs Results): at 4096 the narrower windows keep every class
      clean on the composite (Other F1 0.52, on the floor; the shipped
      (3,5) @4096 has 0.64), and (2,3) @4096 is the best configuration
      on the table — 80.0% behind the cues against the shipped 75.0,
      71.7% under a typo against 63.3. Under the typo `Other` falls
      under the floor in EVERY configuration, the shipped one included
      (0.42): that is the class's fifteen rows (intent-other-more-rows),
      not the window. No default moved; the size-for-points decision
      is filed below. Original: the n-gram window against the per-class
      law at each hash width; a 2–4 window survives a typo at both
      widths and at the shipped width takes `Other` from recall 0.47 to
      0.33 while the total rises.
- [ ] intent-shipped-model-4096 — the shipped model at (2,3) @4096:
      +5.0 points behind the cues (80.0 vs 75.0%), +8.4 under a typo
      (71.7 vs 63.3), `Other` F1 0.52 clean (the floor holds), at four
      times the artifact — `Fit` chose 1024 for a quarter of the size
      when the cost was two points. The owner's call: if taken, refit
      through `MakeModel` with `dim = 4096, low = 2, high = 3`, re-take
      every number `Models.scala`'s doc comment and `TestModels` /
      `TestSecondAuthor` pin (61.7% alone, 75.0%, the register-shift
      table, per language), and move `Fit.grams`' defaults with it so
      the artifact stays what the generator produces.
- [x] intent-typo-robustness — MEASURED 2026-09-07, default kept:
      per window at both widths (`TestTypoRobustness`, default gate),
      (3,5) 65.0 → 55.0% @4096 and 61.7 → 53.3 @1024 under one
      transposition; (2,4) 65.0 → 63.3 / 61.7 → 65.0; (2,3) 68.3 →
      66.7 / 58.3 → 66.7; the TF-IDF control 61.7 → 56.7. A smaller n
      buys the typo back — and the shipped model refitted at (2,4) or
      (2,3) loses `Other` (recall 0.47 → 0.33, F1 under the 0.50
      per-class floor) while its totals rise, so the default stays
      (3,5) and the reason is written on it. Filed
      intent-window-by-dim; the rows are intent-other-more-rows.
      Original: character n-grams are supposed to
      survive a typo, and this model does not: one deterministic
      transposition in the longest word takes it from 61.7% to 55.0%
      (2026-09-04). At 60 training messages the hashed 3-5-grams are
      too sparse for the redundancy that argument depends on. Two
      measurable fixes — more rows, or a smaller n — and the suite
      that found it (`TestSecondAuthor`) already measures the result.
- [ ] intent-second-author — PARTLY ANSWERED 2026-09-04 by measuring
      the gap instead of the corpus: 66.7% on the least-familiar half
      against 86.7% on the most, 65-67% under mechanical register
      shifts, and every shipped quote corrected to 65-70% for a
      message somebody else wrote. What remains is the part no
      measurement replaces — a corpus this repository did not write.
      Original entry follows.
- [ ] intent-second-author (original) — the provenance problem the review could
      not fix: the rows are still one hand's Russian, rewritten by the
      same hand that wrote them. A gap measured against my own language
      is a joint measurement of the model and me. The consumer offered
      REVIEW, which is what was available and is now spent; what is
      missing is a second AUTHOR, for Russian and for whatever
      languages the fixture keeps.
- [x] intent-end-to-end — LANDED 2026-09-04 as `okay.demo.IntentRouter`,
      the first caller these tiers have had, and it exposed three
      frictions no test had found: a filled `Frame` hands back TEXT
      rather than the parsed value, the pattern tier speaks canonical
      names so every caller writes a mapping, and `Taxon` is connected
      to none of the tiers that classify. All three filed rather than
      quietly fixed. Original entry follows.
- [x] intent-end-to-end (original) — NOTHING CALLS ANY OF THIS. Thirteen files, a
      dozen measured lanes, and inside okay there is no path where a
      message arrives and a decision leaves. A consumer has their own
      router; okay-intent has no caller of its own. This finds what
      measurement cannot — awkward signatures, missing errors, the
      order a caller actually needs things in — and there is already
      one symptom: `Frame.filled` throws the parsed value away, which
      surfaced the moment I tried to write how it would be used and
      never in any test. Highest value of anything on this list.
- [ ] intent-structured-output — every lane bought its answer's SHAPE
      by persuasion: a rendered example, written rules, field order.
      OpenAI-compatible gateways take `response_format` with a JSON
      schema, which makes the shape a property of DECODING rather than
      of asking nicely. Never tried once. If the gateway supports it,
      it may close the decode question outright and take accuracy with
      it — one experiment, not a research programme, and it is not a
      data problem.
- [x] intent-multi-intent-measured — LANDED 2026-09-05. Twelve
      two-intent messages in the fixture; the shipped path answers
      with one label by construction (matched either intent 10/12, and
      the cue tier's runner-up carries the second 5/12 where the
      router discards it); the model tier returns two spans 6/12, the
      right pair 5/12, the right pair in order 4/12, every span
      grounded in the message 12/12. The claim is a property of ONE
      tier and now says so. Original entry follows.
- [x] intent-multi-intent-measured (original) — spans have been in the type since
      the first lane and were argued for as the thing a flat list
      cannot express, and the fixture contains NOT ONE message with two
      intents. The mechanism has never been exercised. Either measure
      it or stop claiming it.
- [x] intent-jmh-row — LANDED 2026-09-05. A JMH row per tier, plus
      the load cost nobody had ever timed. The quoted microseconds
      were 50-70x too high because a cold loop in a test measures the
      JIT: probe 76 -> 1.7, centroid 90 -> 1.3, chargrams 92 -> 13.7,
      the fit 404ms -> 40.1ms. Load of the shipped model: 58.9us.
      Original entry follows.
- [x] intent-jmh-row (original) — this line quotes microseconds everywhere (76us
      probe, 90us centroid, 92us chargrams) and every one of them is a
      `System.nanoTime` around a loop inside a test: no warmup, no JIT
      accounting, one run. The repo keeps `src/jmh/history.tsv` for
      exactly this, and by its standard those are not measurements. A
      benchmark row per tier, or the numbers should stop being quoted.
- [x] intent-frame-typed-values — LANDED 2026-09-04. `Frame` keeps
      `Answered` (slot, text, parsed value) and `valueOf` takes the
      SLOT, which is the evidence the answer has that type. One
      isolated cast, guarded by `a.slot eq s` and tested with a
      same-named slot of a different type getting nothing back. The
      caller's test that recorded the defect now pins the property.
      Original entry follows.
- [x] intent-frame-typed-values (original) — `Frame.filled` is `Map[String,
      String]`: a slot knows its type `A`, parses the answer to prove
      it is acceptable, and then stores the raw text. A caller that
      wants the date parses it a SECOND time, with the same reference
      day, and nothing in the type says so. Demonstrated from
      `IntentRouter` rather than argued. The obstacle is holding
      heterogeneous parsed values without a cast — a type-indexed map,
      or `Frame` carrying a tuple of its slots' types; the consumer has
      built one and may already know which.
- [x] intent-cues-for-a-taxonomy — LANDED 2026-09-04. `Patterns.Cues`
      pairs a cue set with the `Taxon` it decides, checked once at
      construction; `renamed` moves it onto another taxonomy and is
      total in both directions, so the router's silent `case _ =>` is
      now a `Left` naming the class nobody mapped. `IntentRouter` lost
      both the translation and the `.filter(taxonomy.has)` behind it.
      Original entry follows.
- [x] intent-cues-for-a-taxonomy (original) — `Patterns.meeting` hardcodes the
      canonical class names, so a caller with a domain-bearing taxonomy
      writes a mapping, as `IntentRouter.canonicalToTaxonomy` does and
      as every caller after it will. `Cue.cls` is a `String` and could
      carry any names: what is missing is a way to state a cue set
      AGAINST a `Taxon`, and a check that every cue names a class the
      taxonomy holds.
- [x] frame-language-with-grammatical-gender — SETTLED 2026-09-07
      (frame-language-tag-fallback): the caller keys by the tag it
      owns (`pl-formal-f`) and `Slot.lookup` finds a wording along the
      tag — the tag, each shorter prefix at a `-`, the fallback;
      `question`, `show`, `options` and `speaks` all go through it, so
      `untranslated` is honest for a tag. The library models nothing
      about gender or register. Laws in `TestFrame`; docs/modules/
      okay-frame.md says the rule. Original: the migrating consumer
      raised it and could not test it: `Slot.ask` is keyed by a
      language CODE, and a language whose question differs by the
      grammatical gender of the ADDRESSEE (Polish Pan/Pani, and the
      formal registers around it) needs more than a code — or needs
      the caller to key by "pl-formal-f" and own the choice. Their
      four languages dodge it because Polish there addresses
      informally. Worth settling before the Map-keyed language is
      called done for all languages.
- [x] intent-temporal-multilingual — DONE 2026-09-07: `Temporal.
      Multilingual`, a lexicon per language (fr, de, es, ru, uk, pl;
      ja as a string scan) — weekday, month and relative-day words as
      token PREFIXES so inflections and German compounds match, the
      qualifier before or after the weekday, `N jours` / `vor N Tagen`
      / `через N дня`, the next-week pair, `15h` / `15 Uhr` / `15時`,
      `M月D日`. English is tried first and unchanged. The law
      (`TestTemporalMultilingual`): every dated row of the parallel
      fixture reads the same `When` in every language as in English;
      the relative and counted forms per language against one Friday.
      Original: (absorbs the older
      intent-temporal-other-languages, filed twice by me before I
      noticed the first.) `Temporal` parses English, so
      `Frame.fillFrom` fills English rows and declines the other five
      languages: measured 5/5 in English and 0/5 in fr, de, es, ru, ja
      over the parallel fixture (2026-09-04). The router degrades
      correctly — it asks, in the reader's language — so this is a
      coverage lane rather than a correctness one. The shape is
      already there: `parse` is a word-list scan over weekday, month
      and relative-day vocabularies, so a second language is those
      three vocabularies plus its own qualifier words ("prochain",
      "nächsten", "próximo", "следующий"), not a new design. Japanese
      needs a different tokeniser and should be its own decision.
      The fixture's parallel set already carries "jeudi prochain", "am
      Montag", "el martes", "во вторник" and Japanese weekday forms,
      so the test data for this exists.
- [x] intent-fitted-model-ships — LANDED 2026-09-04. `Models.meeting`
      is a fitted CharGrams model that ships (43KB generated source,
      cross-platform), `Fit` is the corpus->model->file->model door,
      and `MakeModel` regenerates the artifact with a test asserting
      the committed bytes are what it produces. 76.7% at full coverage
      behind the cues on held-out English, no network. Original entry
      follows.
- [x] intent-fitted-model-ships (original) — NOTHING SHIPS A FITTED MODEL, and
      there is no documented way for a caller to obtain one. Every
      measured tier above the pattern cues needs a `Centroid.Trained`
      or a `Probe.Trained`, which today exists only inside a test that
      fitted it from the fixture. A caller reading the module has the
      types, the accuracy tables, and no path from "I have messages"
      to "I have a model" — `Fitted` writes one as data and nothing
      writes the file. Named to the operator as a usability blocker on
      2026-09-04 and not filed until now, which is the miss this entry
      exists to correct.
- [x] intent-one-entry-point — LANDED 2026-09-04. `okay.intent.Router`
      holds the measured tier order and the four outcomes;
      `Router.of` refuses a tier that does not speak the taxonomy;
      `Router.offline()` needs nothing. The demo is a caller now and
      is shorter for it. `CharGrams.renamed` came with it, so a
      domain-bearing taxonomy can use the shipped model. Original
      entry follows.
- [x] intent-one-entry-point (original) — the composition of the tiers lives ONLY
      in `okay.demo.IntentRouter`: cue tier first, vector tier below
      it, escalate under the margin, fill the frame, ask what is
      missing. That order is the measured one and a caller outside the
      demo has to re-derive it by reading twenty Results sections.
      okay-intent should hold the composed door itself, with the demo
      as its caller rather than its definition. Also named on
      2026-09-04 and unfiled until now.
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
- [x] intent-duration-multilingual — DONE 2026-09-07: `Duration.
      Multilingual`, a lexicon per language (fr, de, es, ru, uk, pl;
      ja as a string scan) — unit words as token prefixes, number
      words with their genders and the one-and-a-half words
      (`anderthalb`, `полтора`, `półtorej`), a ten and a unit as one
      number (`сорок пять`, `cuarenta y cinco`), the fraction phrases,
      `et demie` / `y media` after the hours and `с половиной` / `i
      pół` before them, `N時間半`. The fixture is in the suite — eight
      meanings, eight wordings each, dictionary facts — and the law is
      `Temporal`'s: one meaning, one value. Original: `Duration` reads
      English; the parallel fixture has no duration rows, so the law
      that settled `Temporal`'s seven languages has nothing to hold on
      to yet.
- [x] intent-taxon-wired-to-tiers — LANDED 2026-09-05. Every `Trained`
      carries the `Taxon` it was fitted against; `train` infers,
      `against(taxon, rows)` declares and refuses a label outside it,
      `silent` names what a declared taxonomy holds and the rows never
      taught. `NoModel.fit` refuses cues and a probe that speak
      different names, which was a live silent-degradation bug. The
      taxonomy is deliberately not persisted. Original entry follows.
- [x] intent-taxon-wired-to-tiers (original) — request 1 asked for one taxonomy
      both tiers read, and what landed is one taxonomy that NEITHER
      tier reads: `Classify` takes a `Schema[I]`, `Patterns` takes
      cues, `Centroid` takes whatever labels it was fitted on, and a
      caller checks `taxonomy.has` by hand afterwards. The value is
      right and the wiring is absent — `Taxon` should be what a tier is
      fitted or built against, so a mismatch is a compile or fit error
      rather than a silent disagreement.

## channel-per-part-waiters — DONE 2026-09-05

Senders wait per part now, and the buffer reports `lastRoute` so a
freed slot wakes a sender that can use it. 111546us to 485 at sixteen
producers, a 230x fix; the bounded relaxed channel is now 5.3x past a
single ring and 5.5x past `zio.Queue`, and scales the right way.


## channel-per-element-effect-cost — CLOSED as an interpreter lane 2026-09-06, redirected

Taken as free-cont-stack on the hypothesis this entry invites: that
the 26% in `runFree` and the 13% in Free allocation are the left-nested
re-association, `Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g))`,
which rebuilds a node and a closure every step, and that an explicit
continuation stack removes it.

**Counted before writing any of it, and the hypothesis is dead.** A
probe in `runFree`, this lane's own shape, N=4000:

| case | elementwise | chunk-native |
|---|---|---|
| `rotate` — the re-association | **64** | 4 |
| `Bind(Pure, f)` | 4001 | 17 |
| `Bind(Inject, f)` | 8020 | 36 |
| `Pure` | 2 | 2 |

The rotation is 64 steps out of 12085 — half a percent. The other
12021 take the two fast branches, which allocate nothing today. A
continuation stack would optimise 0.5% of the walk and add a cons cell
to the 99.5%. Not written.

What the counts DO show is the real structure — three interpreter
steps and two effect injections per element — and they explain the
chunked lane in one line: **59 steps against 12085** for the same 4000
elements, which is why it reads 19.66 against 209.3.

**Then the step count itself was measured, and it is not the lever
either.** One of the two injections per element is the CALLBACK:
`runForeach` takes `A => Unit ! Async`, so a plain side effect is
lifted through `Async.Run`. Walking the same source with a plain
function removes that injection, its `Bind`, and a third of the steps
(`PerElementStepBenchmark`, N=4000, quiet):

| lane | us/op |
|---|---|
| `elem_effectCallback` (today's surface) | 209.313 ±1.504 |
| `elem_plainCallback` | 199.621 ±3.308 |

**4.6%.** A third of the interpreter steps is worth five percent, so
the interpreter is not where the lane's time goes. (That lane is a
measurement, not a proposed API — `Source.runForeach`'s documentation
states the lifting rule deliberately. And it would not be a fair pair
against ZIO, whose `runForeach` callback is an effect too.)

**A fresh profile says where it does go.** The profile this entry
rests on predates `bufferChunked` and the linear-view feed. Re-taken
today (`-prof stack`, 1 fork, elementwise lane):

```
58.3% WAITING (86.1% of it Unsafe.park)   24.9% RUNNABLE   16.8% TIMED_WAITING (all park)
  within RUNNABLE:  go$4 3.9%   resume 3.7%   runFree 2.5%   receiveMany 0.8%   popMany 0.3%
```

`runFree` is **2.5% of wall time, not 26%**, and the whole effect
machinery is around 10%. Three quarters of the time the thread is
PARKED. Caveats stated: one fork, ten seconds, and JMH filtered half
the runnable frames — so read the ordering, not the decimals.

So the last gap on this lane is not the interpreter and not the queue.
It is the wakeup handshake, which already has an entry:
**`channel-elementwise-wakeups`** — one unpark per element on the
consumer's critical path. That is where this work goes next, and it
now has evidence rather than a hunch. The remaining interpreter idea
(make a per-element `Writer` step cheaper) is not refuted, but it is
priced: everything above says the ceiling is around a tenth of the
lane.

## the original entry, kept for the reasoning — WITHDRAWAL REVERSED, gap narrowed

Audited 2026-09-06. This entry withdrew the chunk-native read as
measured-and-worse (447.9 against elementwise 264.5) on the finding
that there was nothing to chunk: the producer delivered 1.67 elements
per `receiveMany`, because `Channel.buffer` fed the channel at one
`Free` step per element.

The entry then named its own fix — "give `Channel.buffer` a
chunk-native feed so the producer emits arrays" — and that fix LANDED
(9fe22fdc, `bufferChunked` + the linear-view feed). With the producer
able to run ahead, the withdrawn lane is the fastest one we have:

| consumer shape | producer | us/op |
|---|---|---|
| chunked read (`drained` over chunks) | `Channel.buffer`, per-element | 447.9 — withdrawn |
| chunked read (`drained` over chunks) | `bufferChunked(64, 256)` | **19.66 ±0.17** |
| elementwise (`okayChannelForeach_elem_runForeach`) | `Channel.buffer(1024)` | 209.3 ±3.5 (was 264.5) |
| `zioChannelForeach_chunk_runForeach` | — | 133.0 ±16.4 (was 139.1) |

Read those first two rows carefully: they are NOT one lane
re-measured. Same consumer shape, different producer — which is the
whole finding. A consumer-side batch is worthless while the producer
is the bottleneck and worth 23x once it is not, so the withdrawal was
correct about its own moment and wrong as a verdict. Generalise that
before withdrawing anything else measured behind a slow producer.
The elementwise row IS one lane re-measured, same harness, same name.

What is genuinely left: the ELEMENTWISE path at 209.3, where 62% is
still effect machinery (`runFree` 26%, `Async` handler 15%, `Free`
allocation 13%, `resume` 8%) against 8% in the channel. Making a
per-element `Writer` step cheaper is the remaining work, and it is a
Cont/interpreter lane, not a channel one.

NOT SETTLED HERE: whether 19.66-vs-133.0 is a fair pair at all — one
is our chunk-native surface, the other ZIO's chunked one. That is
precisely what `benchmark-fairness-audit` is measuring; take its
verdict, not this table's, before quoting a ratio anywhere. Original
entry follows.

Reading a buffered channel one element at a time: **264.5us against
`ZStream.fromQueue`'s 139.1**, the one row in the idiomatic table
where zio genuinely leads after five mismatched pairings were fixed.

Profiled (`okayChannelForeach_elem_runForeach`): **62% effect
machinery** — `runFree` 26%, the `Async` handler 15%, allocating
`Free` nodes 13%, `resume` 8% — against **8% in the channel itself**.
The queue is not the cost; the per-element program step is.

**A chunk-native read was tried and WITHDRAWN, measured.** The idea
was to batch the PROGRAM the way `popMany` batched the queue: a
`drainedChunks: Source[Chunk[A]]` whose tree steps once per batch. It
measured **447.9 against the elementwise 264.5** — worse, and the
reason is the batch size: on this path the producer delivers an
average of **1.67 elements** per `receiveMany` (max 64). There is
nothing to chunk. `Channel.buffer` feeds the channel through the
effect system at one `Free` step per element, so the consumer never
falls behind and the buffer never fills.

That is the same finding as `channel-send-fastpath`, one layer up:
batch size is set by how far the PRODUCER can run ahead, and here it
cannot, because its own per-element cost is the interpreter.

So the work, if it is taken: make a per-element `Writer` step cheaper,
or give `Channel.buffer` a chunk-native feed so the producer emits
arrays. The second is likely the smaller change — `Channel[Chunk[A]]`
already exists and `mergeChunked` already uses it — and it would make
a chunked read worth having, which it is not today.


## feed-staged-loop — measured and declined

After `feed-linear-view`, the obvious next step was to stage the feed:
loop with `offer` while the buffer has room and build a program only
where waiting happens, so the remaining `send` + `flatMap` per element
would go too.

Measured, it does not pay:

| lane | linear view | + staged loop |
|---|---|---|
| chunk-native | **19.66 ±0.17** | 20.47 ±0.54 |
| elementwise | **209.3 ±3.5** | 241.3 ±3.2 |

Neutral on the chunked path and 15% WORSE on the elementwise one, and
the reason retires the idea rather than inviting a second attempt.

On the chunked path there was nothing left to stage: `feedBatched`
already sends per CHUNK, so the whole run built sixteen node pairs,
not two per element. The per-element traffic had already been removed
by the linear view.

On the elementwise path the consumer is slow, so the buffer is
saturated nearly always — and then the loop costs a FAILED `offer` and
an `Option` per element and still builds the program to park. Strictly
more work than the plain `send` it replaced. The loop only pays while
there is room, and there is no room.

**Staging removes the building of a program; it cannot remove the
waiting.** What is left in the feed after the linear view IS the
parking, and that is the work the program exists to describe.

If anything revives this, it is the adaptive feed (see
`channel-per-element-effect-cost`): a producer that notices it is
saturated and stops trying to offer. That is a different lane, driven
by state rather than by shape.

## fs2-chunked-merge-lanes — the fs2 rows in §6 and ChunkFlush are the singleton spelling only

**CLOSED 2026-09-06** (fs2-chunked-merge-lanes): re-paired at N=500 and N=2000; fs2 chunk-native 84 / 109, slightly ahead of ZIO in §6b. See CHANGELOG.

Filed by benchmark-fairness-audit (2026-09-06). `ChunkFlushBenchmark`'s
`fs2Chunked` lane chunks AFTER the merge (`a.merge(b).chunkN(k)`), so
the merge itself still sees singletons; measured at N=2000, chunking
BEFORE it (`Stream.emits(range).chunkN(256).unchunks` a side) reads
281 against 35 700. §6's table is N=500 and was not re-paired: add
the chunked-before lane to `MergeBenchmark` and `ChunkFlushBenchmark`
at their own N and put the row beside the singleton one — do not scale
the N=2000 number. Same for `fs2.Stream.range` anywhere it feeds a
competitor lane: it is per-element by construction (3.10.2,
Stream.scala:3981), and `emits`/`chunkN` is the chunked spelling.

## close-the-gaps — refuted attempts, so nobody tries them blind

- `Chunks.elements` as a single cursor over the chunk walk (no
  `Iterator.flatMap`): 23.3 → 22.5, inside noise. Boxing through
  `Iterator[A]` is the per-element cost; the win is the chunk-native
  path, which exists. Reverted 2026-09-06.
- JVM `Fiber.joinEither` on `CompletableFuture.get()`: 19.6 → 22.3,
  worse in all three rounds — `get()` spins before parking. Reverted
  2026-09-06. If the join is ever revisited, the thing to try is a
  fast path that reads the future's completed value without
  registering a callback, NOT a different park.
- `runForeach` — DONE (runforeach-one-walk, 2026-09-06): 159.7 → 99.9,
  0.63x. The channel lanes did not move: their per-element cost is the
  Async operation `.drained` forwards per element, not the walk.

## actor-receive-offer-first — the mirror of feed-offer-first, on the loop that reads one message at a time

Measured 2026-09-06 (`actorTell`, §17): 4000 tells through an actor
read 295.9us against 198.5 through the bare mailbox shape, 1.49x, and
the JFR says why per message: `Slot` 142, `Right` 95, `Some` 89, the
`Platform` lambda 87, the `Await` anon 87. The actor loop calls
`receiveBlocking()` once per message — deliberately, so supervision
knows which message was the poisonous one — and each call runs the
full handshake even when the message is already there, which under
load it always is. `feed-offer-first` removed exactly this on the send
side (−38%). The receive side needs a synchronous poll the loop can
try first: a `receiveNow(): Option[A] | end` on `Channel`, answered
from the ring without a `Slot`, an `Await` or the `Right(Some(_))`
pair, falling back to `receiveBlocking()` only when empty. That also
finishes `channel-callback-allocation`'s receive half for this caller.
Laws: the one-message-at-a-time property must survive (a poll takes
ONE), close/fail/end must be seen through the poll, and the ordering
against parked receivers must hold. Expected: most of the 0.49x.

## actor-ask-timer — DONE 2026-09-06 (§17e): the wait as one operation, 4.4 KB → 1.5 KB per ask

The timer's thread went in `small-wins` (5.5 → 4.4 KB); the rest was
`race` itself — a fiber per side for a contest between two callbacks.
`Reply.await` is now one `Async.await` over `box.receiveAsync` and
`Timer.after`, first wins, other cancelled; `ask` needs no
`Scheduler`. `actorAsk` 2332 → 1557 us and 880 429 → 308 646 B/op
(−33% / −65%). Of the two shapes filed below, "arm the timer only if
the reply is already there" is moot for a sequential ask (the actor
has not run yet when the send returns) and the single-slot `Reply`
stays open at a few hundred bytes of upside. Original entry:

### as filed — 13 microseconds and 5.5 KB per ask

`Reply` is a `Channel[R](2)` and `await` is `Async.race(box.receive,
Async.sleep(within))`: a virtual-thread timer armed for every ask,
whether the answer takes a microsecond or never comes. Measured
(`actorAsk`, §17): 13.0us and 5.5 KB per round trip; the KB is the
timer's stack chunk. Two cheaper shapes, to measure not assume: arm
the timer only if the reply is not already in the box after the send
(the common case under a responsive actor), or a single-slot box
instead of a full channel. Matters only for ask-heavy callers.

## reactive-bridge-profile — DONE 2026-09-06: 5.67x → 2.63x, −51% time, −41% bytes (§17b)

Counted per side first: the reader made 4001 awaits for 4000 elements
(one handshake each, where `drained` takes 62), and the pump walked
the source through a memoising `toLazyList` — a cell, a `State$Cons`
and a thunk per element. Two fixes, each alone neutral or worse, and
together **312.9 → 152.5 us, 2 951 724 → 1 746 516 B/op**: the pump on
the linear view runs ahead, so the reader's chunks (up to 64 per
handshake, demand still following consumption) fill instead of
parking. The third withdrawal-reversal of the day and the rule it
leaves: measure a consumer-side batch only after the producer that
fills it is fast. Laws 49/49 (TestReactive + TCK) at every step. What
remains is representation: `Writer` per element, `uncons`'s three
objects on the pump, an atomic decrement of demand, boxing, the ring.

## as filed that morning — 5.67x and 738 bytes per element, unprofiled

The largest ratio measured on 2026-09-06 (`reactiveRound` vs
`plainSource`, §17): a round trip through `Flow.Publisher` and back
through `Flow.Subscriber` costs 312.9us and 2.95 MB against 55.2us and
0.86 MB. The shape has two channels of 256 and two half-window demand
batches per window. Before anything is changed: a JFR class breakdown
of `reactiveRound`, and a count per side of the awaits on each of the
two channels — the method that settled the channel lanes today. The
guess this entry refuses to make is which of the two channels, or the
demand accounting, is the 738 bytes.

## behavior-state-boxing — a primitive state boxes on every step

`Behavior[S, M] = (S, M) => S ! Async` over `S = Long` allocates a
`java.lang.Long` per message (141 of ~900 allocation samples on
`actorTell`). Not a library defect — a generic `S` cannot be
specialised without a second trait — but worth one line in the actor
docs, now written: give a behaviour an `AnyRef` state. Closed by the
doc line; kept here so the sample count has a home.

## actor-stop-strands — DONE 2026-09-07 (small-wins): a supervised Stop drains and discards, and `stopped` comes true

The loop now calls `discardRest()` after `fail` + `close` under
`Supervise.Stop` and `Escalate`: it reads the closed mailbox to its
end -- which is the failure, since the channel was failed -- and drops
what it reads. The messages were accepted and will never be handled;
that was already true, silently. Now `finished` means what it says,
`ActorRef.stopped` turns true, and the law in `TestPoisonLaws` asserts
it again (19/19 across the actor module). The `stop()` doc carries the
qualifier: a supervised stop drains and DISCARDS; a caller who needed
those messages handled wanted `Resume` or `Restart`. Original entry
follows.

## as filed — Supervise.Stop closes with accepted messages inside, and `stopped` is never true

Found 2026-09-06 writing a law for actor-receive-offer-first. On a
poisonous message under `Supervise.Stop` (and `Escalate`) the loop
does `mailbox.fail(e); mailbox.close(); running = false` — and stops
READING. Messages already accepted behind the poisonous one stay in
the mailbox, nobody drains them, and `ActorRef.stopped`, which is
`mailbox.finished` — "every accepted element handed over" — can never
become true. This is not the poll-first loop's doing: the old loop did
exactly the same. It is a standing semantics, and it is at odds with
the module's own note that `stop()` DRAINS under the strong contract.
Two honest shapes: drain-and-discard the rest before closing (so
`stopped` means what it says and the stranded messages are
acknowledged as dropped), or document that a supervised stop strands
and `stopped` is not the thing to wait on. Either way the actor docs'
"stop, DRAINING" line needs a qualifier. Not taken in the lane that
found it; the law there waits on the closed mailbox instead.

## actor-receive-offer-first — MEASURED AND DECLINED 2026-09-06: +19% in the regime that matters

Built: `receiveNow(): Poll[A]` on `Channel` (default `Empty`,
`SentinelChannel` answering from the ring, refusing while a receiver is
parked as `offer` refuses while a sender is), and the actor loop
polling before it parks. Eight laws held. The A/B against the old loop
in both regimes (§17a): **+19% time with an empty mailbox** (−14%
bytes), −17%/~0 time and −35% bytes with a full one. A failed poll is
a second read of the producer's cache line, ~17ns × 4000 = the 67us
lost; a responsive actor's mailbox is empty most of the time. Reverted
in full — no `receiveNow`, no `Poll`, the loop as before. Kept: the
`actorTellBacklog` lane, so both regimes are always measured, and the
poison laws (`TestPoisonLaws`), which held for the old loop and had
never been written.

## actor-receive-fused — the shape that wins in both regimes, not yet built

Why offer-first loses on receive when it won on send: a failed `offer`
reads the producer's OWN line (the ring tail it is about to write, no
contention); a failed poll reads the line the OTHER side writes. So
the try must not be a second scan. Fold it into `receiveAsync`'s first
scan — a variant that RETURNS `Got(a)` when its first pop hits and
only otherwise enqueues, rechecks and answers through the callback —
so a hit is one scan and one small object, and a miss is exactly the
old path. And the handshake a miss pays is a `Slot` plus a callback
lambda: make the callback BE the slot (one object with `value`,
`filled`, `waiter` and `apply`), which halves `CanBlock.block`'s
allocation for every caller, not only the actor. Expected: the full
regime's −35% bytes kept, the empty regime at parity or better.
Measure both with `actorTell` and `actorTellBacklog`; the A/B method
is in §17a.

## actor-receive-fused — DONE 2026-09-06 (§17c): the receive-side handshake, one object, no second scan

`Handoff[A]` in core — the callback that is its own slot — with
`CanBlock.handoff()`/`await(h)` on JVM (park/unpark) and Native
(monitor), `Channel.receiveInto(h): Boolean` (default: register and
answer false, correct before fast; `SentinelChannel`: the first scan
with an early return on a hit), and `receiveBlocking` rebuilt on them.
Old against new, both regimes: **bytes −5.2% (empty mailbox) and
−12.9% (full), time at parity in both** — the regression that sank
the poll-first loop does not occur, because the try is the scan the
handshake was going to do. Every `receiveBlocking` caller in the
library gets it. This also finishes `channel-callback-allocation`'s
receive half for the blocking form: the `Right(Some(_))` pair is gone
from the hit path, and the `Option[A]` return costs its one `Some`.
The `receive` PROGRAM (`Async.Await` + callback) is untouched and
still pays the pair; that is the remaining half, priced against the
`End` type as before. Laws: `TestHandoff` (7), `TestChannelLaws`,
`TestPoisonLaws`; Native compiles.

Process note kept here because it cost an hour: the A/B chain that
produced these numbers also WIPED the six uncommitted source edits —
a quoted `$FILES` list made its backup `cp` fail while the
`git show master:f > f` overwrite succeeded. The measurement was
valid (NEW ran before the swap); the tree was rebuilt from the
transcript and re-verified. Rule, now in memory: commit before any
script rewrites tracked files; restore with `git checkout`, never
`cp`.

## source-unfold-tuple — DECLINED by design 2026-09-07

`Source.unfold` costs 13% over `Source.range` on its lane (section 6c),
and the cost is the `Some((a, s2))` per step. That pair is what the
CALLER's `f: S => Option[(A, S)]` returns -- the `LazyList.unfold`
signature, which is the point of offering `unfold` at all. `range` is
faster only because it is not generic: it knows its step is `i + 1`
and tells `i` with no state to carry. Removing the tuple means a
second step type (`Step[S, A]` with `Emit(a, s)`/`Done`) that every
caller would have to learn for a 13% that only shows on a stream that
does nothing else. Not taken; a caller with a hot unfold writes the
specialised source, as `range` does.

## drain-copy-per-element — DECLINED by design 2026-09-07

`Drain` is a case class and `Stream[Drain, Async].uncons` answers
`Some((d.held(d.at), d.copy(at = d.at + 1)))`: three objects per
element -- `Some`, the pair, the advanced cursor -- 42 of ~900
allocation samples on the elementwise channel lane. The cursor is the
library's re-observation law: `uncons` on the same `Drain` twice must
answer the same element, so the advance MUST be a fresh value, and a
mutable index would make a `Drain` a linear resource in disguise. The
`Option[(A, S)]` pair is `Stream.uncons`'s contract for every stream.
Neither is a hole; both are the representation. Recorded so the next
profile does not re-file them.

## actor-ask-timer — DONE 2026-09-07 (small-wins, §17d): 5.5 → 4.4 KB per ask

The JVM `Timer` no longer starts a virtual thread per arm: one
scheduled executor holds the delay as a task, and the callback gets a
virtual thread only when it fires. A fast `ask` cancels before that,
so it allocates the task and nothing else — `actorAsk` 1 108 493 →
879 176 B/op, −20.7%; time 2594 → 2150 ±770 on a bursty box. What is
left per ask is the `Reply` channel (a ring of two), the send and the
race; a single-slot box instead of a channel is the remaining shape,
not taken here. All actor laws 19/19, including the timeout ones.

## drained-chunked-door — DONE 2026-09-07 (small-wins, §17d)

§6c recorded that `.drained.chunked()` re-chunks what `Drain` already
batched and reads 318.7 against 209 elementwise, and named it a
footgun. It cannot be forbidden by type — a `Source[A]` does not say
what it is made of — so the fix is the right door: `Channel#
drainedChunks: Source[Chunk[A]]`, each `receiveMany` batch told as one
chunk, nothing re-done. 79 469 B/op against 1 413 443 for the same
channel read one at a time, 17.8x less; the scaladoc names `.chunked()`
on a drained source as the wrong door.

## json-fast-read — DONE 2026-09-07: `Json.readStrict`, the second door

The operator's call ("let there be a choice"). `Json.read` stays the
lossless road — scanner, CST with every trivia token, projection,
fold — and keeps its three promises: byte-for-byte losslessness,
damage-as-data, a half-arrived document that still decodes.
`Json.readStrict` goes characters → `Schema` with no tree: the strict
recursive descent of `JsonValue.Parser` (an index, a slice for a plain
string, `parseDouble` on a number's slice) driving `Cbor.get`'s walk —
products by field name with the lossless decoder's own rules (unknown
fields ignored; absent → declared default → None-if-optional →
refusal), sums as the one-entry object `Json.encode` writes, options
as `null`, lists/vectors, iso, bytes as base64, chars as one-character
strings. It refuses — `Left` — anything it is not sure of, exactly as
`JsonValue.parse` answers `None`.

**The law, as a test:** `readStrict(write(a)) == read(write(a))` over
a corpus with every schema shape, whitespace everywhere the grammar
allows, unknown fields, defaults; and on a truncated or damaged
document the strict door is `Left` where the lossless one still
projects. 88/88 across the codec module.

**Measured (2 forks, bars tight):** `textToOrderStrict` **743.9 ±14.2
ns / 5 048 B/op** against circe's 813.1 ±69.6 / 3 416 and the lossless
road's 14 264 ±125 / 127 724 — 0.92x circe in time, 1.48x its bytes;
19.2x faster and 25.3x less allocation than `Json.read`. The strict
Schema walk costs about 3.3x the bare value parse (227 ns / 2.2 KB): the field map, the
erased parts and `make` are most of it, and a STAGED strict decoder —
the macro `Staged` already generates for a `Json` value and for CBOR
bytes — is the shape that would take it to circe's bytes. Filed as a
next lane if wanted, not taken here.

## bench-cross — DONE 2026-09-07 (§18): the same four shapes on JVM, JS and Native

`BenchCross` in src/test/scala-cross: a Live-tagged munit suite, four
shapes through `Async.runAsync`, thirty warmups, median of twenty and
minimum, platform from `java.vm.name`. Run per platform with the
build's `--exclude-tags=Live` REPLACED by an include (`set every Test /
testOptions := ...`), as the `integrationTest` alias does; an include
after `--` runs nothing. First numbers in §18 and the ledger. JS and
Native stable across two runs; the JVM column is a ruler against JMH,
not JMH.

## native-interpreter-allocation — DONE 2026-09-06: the collector is not it; the count is six objects per bind

§18: `bindChain`, N nested flatMaps with no channel, reads 497–517 us
on Native against 164–230 on JS and the JVM's warm ~190 — 2–3x, stable
across two runs. Nothing platform-specific is in that code path; the
difference is the allocator paying for the same `Free` nodes and
closures. Before anything is changed: a Native allocation profile of
that lane (Scala Native's GC has `-Dscalanative.gc.stats`-style
counters), and a check whether the interpreter's per-step objects can
be fewer on every platform — which `free-cont-stack` measured as not
the case for the re-association, but did not measure for the
closures. Native is also where `channelChunks` beats `channelElem` by
the most (2.1–4.6x): whatever is done here, the chunks door is the
Native reader's first move already.

**Measured (docs/benchmarks.md §18a).** The counter is immix's
`GC_STATS_FILE` (one row per collection), and `GC_INITIAL_HEAP_SIZE`
takes the collector out of the picture. Default heap: 503 / 498 us,
ten collections in the whole process, 11.4 ms of collector time.
Heap 2G: ZERO collections, and the lane reads 570 / 559 us — slower,
because every allocation now touches memory the process never wrote.
So the collector is not the cost; the mutator's allocation path is,
and it scales with objects allocated. The JVM reference for the same
program (`PerElementStepBenchmark.bind_runWith`, `-prof gc`) is 27.5 us
and 540,984 B/op: 135 bytes per bind, about six objects — `Inject`,
`Run`, the `() => i` thunk, `Bind`, the `x => go(...)` closure, and
by the byte count a boxed `Long` for `x`. Native pays roughly 21 ns
per object-and-step for those where the JVM's TLAB and escape analysis
pay near nothing. The lever is fewer objects per bind, on every
platform, filed as `free-bind-node-count`. Note also that BenchCross's
JVM column (189–483) is 5–12x the JMH figure for the same shape: 30
warmups of 4000 binds is not warm; read that column against JMH, as
its header says.

## free-bind-node-count — DONE 2026-09-06: the injection is 56% of the Native bind; a fourth node declined, a direct loop filed

`native-interpreter-allocation` counted 135 bytes and about six
objects per bind on the JVM, and showed that on Native that count IS
the cost (2G heap, zero collections, no faster). The candidates:
`Inject(Run(() => a))` is three objects for one operation — a Free
node that carries the thunk directly would be one; the `Bind` and its
closure are the program and stay; the boxed `Long` argument is the
generic `Function1` in `flatMap`. Measure on `bind_runWith` (JVM
bytes/op) and on Native's `bindChain` before and after each; the
Native number is the one that moves. Not a rewrite of `Free`: its
`fold` is the interpreter and every handler matches on three cases.

**Measured (§18b).** `pureChain` — the chain over `okay.pure(i)`,
no injection — reads 232.9 / 228.1 us on Native against `bindChain`'s
532.6 / 493.9 in the same process; under JMH 19.4 us / 380,984 B
against 27.6 / 540,984. The injection is 40 of 135 bytes and 30% of
the JVM bind but 56% of the Native one. The terminals differ:
`runWith` on `Free` is already a direct loop (`runFree`, Effects.scala
— `H.handle(e)` a plain call), so its 8 us is the objects and one
virtual call; `runAsync`, which BenchCross runs everywhere, is
`Drive.apply` re-entering `fold` per operation with a polymorphic
handler value, a closure per step and the answer back through `k` —
12.6 us on the JVM (`bind_runAsync` 40.2 vs `bind_runWith` 27.6).
A fourth `Free` case is declined by count: 118 sites outside
Free.scala match on the three cases directly. What is open is the
loop, not the node: `async-direct-loop`.

## async-direct-loop — DONE 2026-09-06: a quarter off the bind on Native (−26%), JS (−20% by minimum) and the JVM's `runAsync` (−27%); §18c

Landed: `Drive.apply` as a `while` over `Free`'s cases with the
operation dispatched to `op` (next program, or null when parked).
Native `bindChain` 534 → 396 us, JS 160 → 128 by minimum, JMH
`bind_runAsync` 40.2 → 29.2 us and one 16-byte closure per bind gone;
controls (`pureChain` Native, `bind_runWith`) unchanged. `runAsync`
is 2.3 us over `runWith` now; what remains of Native's injection cost
(163 of 396 us) is the three objects, the `op` call and `f()`, and
the next lever there is `free-bind-node-count`'s declined node — not
worth 118 match sites for it. Original entry:

### as filed

`free-bind-node-count` put a number on `runAsync`'s round-trip:
`Drive.apply` calls `fold` afresh for every operation with a
`[X] => F[X] => (X => Free) => Unit` value, `h(a)` builds the
`k => …` closure per step, the answer returns through `k`, and the
loop re-enters `fold` from the top — 12.6 us of `bind_runAsync`'s
40.2 on the JVM, and an unknown share of Native's 300 us of injection
(§18b), on a platform that inlines none of it. `runFree`
(Effects.scala) and `Stm`'s runner (Stm.scala:271, `case
Bind(Effect(e), k) => loop(k(perform(e, log)))`) are the precedent:
a direct match over `Free`'s cases with the effect's operations
inlined, no handler value. Write `Drive.apply` that way — the
left-nested rotation and the `Bind(Pure, f)` step as in `fold`, `Run`
inlined, `Await` keeping its exchange cell exactly — and measure
`bind_runAsync` (JMH, `-prof gc`) and `bindChain` on Native and JS
before and after; `bind_runWith` is the JVM ceiling and `pureChain`
the floor. Laws: every Async suite; cancellation at the next
operation and the callback-during-registration exchange do not move.
Not a change to `Free`, `fold`, `runFree`, or any other handler.

## raft-wire-election-flake — DONE 2026-09-07: Live-tagged, and made robust where it now runs

The operator's rule, stated again 2026-09-07: every flake moves to
the integration tests. `TestRaftWire` is tagged `Live` -- out of the
default gate, run by `sbt integrationTest` -- after failing three full
gates in two days under matrix load while passing 3/3 in isolation
every time. Two fixes went in with the tag, for the run it now has:
the cluster is built through a retrying door (`freePort` closes its
socket and the node binds later; the 2026-09-06 gate lost that race
to a BindException) that closes half-built nodes before trying again,
and the waits around the protocol are budgets a loaded box can meet
(election 15s, settle 6s, commit 15s; the nodes' own 20/200/50ms
timings are the law and are untouched). Verified: excluded from the
gate, and 5/5 under the integration flag with four CPU burners
running.

The same day, the same rule, a second suite: `TestSupervision` ("the
default is Stop -- a failure ends the actor") missed its 5s wait for
`stopped` once under a full matrix at load 8, after 2/2 other full
gates; 20/20 in isolation under four burners. Tagged `Live`, wait
budget 30s; `TestPoisonLaws` keeps `stopped`-after-poison in the gate.

Audit note on this section: its remaining `[ ]` items --
channel-impls, channel-impls-correctness, ring-channel-waiters,
channel-ring-unbounded, channel-multififo-many-producers -- are not
flakes, and the channel rewrite of 2026-09-05/06 answered three of
them under other names: the ring channel is `SentinelChannel` (the
default), the unbounded ring is `Segments`, many-producer FIFO is
`AdaptiveFifo` behind `Queues.relaxed`/`adaptive`. They are left in
place for a reader of this section's history; a later audit may close
them against those commits.

## json-strict-staged — DONE 2026-09-07: `Staged.strict[A]`, 2.45x circe, and a correction

The `Staged` macro's third target: `StrictJsonCodec[A]` from
`Staged.strict[A]`, generated over `JsonStrict.Reader` the way the
CBOR codec is generated over `Cbor.In` — primitives call the reader's
own `number`/`string`/`bool`, products go field by field into slots by
name with unknown fields SKIPPED (JSON's rule, not CBOR's refusal) and
absences filled as the fold fills them, sums by the one-entry object,
recursion and Mirror-less types falling back to the interpreted walk.
Laws: staged == interpreted strict == lossless over a corpus, plus the
refusals; 94/94 in the codec module, on three platforms (2485 in the
gate). `JsonStrict.Reader` became public for it: a package-private
member reached from a quote is an "unstable inline accessor" in the
caller's compilation unit — "access from wrong staging level".

**Measured (2 forks):** `textToOrderStrictStaged` **323.1 ±15.6 ns /
2 320 B/op** — 2.45x faster than circe (792.8 / 3 416), 32% less
allocation, 2.36x faster than the interpreted `readStrict` (764.1 /
4 968), and 112 bytes over the bare value parse (225.8 / 2 208): it
reads at the cost of scanning.

**The correction.** `textToOrderStaged` — `Json.parseValue` plus
`Staged.json[Order].decode` of the tree — reads 348.4 ns / 2 680 B in
the same run: 2.3x circe, and it has been in `CodecBenchmark` since
staged-codecs. It was never on the price list, and the list's
sentence "need raw speed? use circe" stood beside it. `json-fast-read`
measured its interpreted door at 0.92x circe and called that the
choice; the existing staged road was already better, and that lane
did not say so. The price list now carries all three doors and their
prices; the fastest is this one.
