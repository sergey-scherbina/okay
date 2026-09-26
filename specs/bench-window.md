# bench-window — a benchmark gets its quiet box by protocol, not by luck

## Overview

A JMH lane measures wall-clock time, so it needs the box to itself;
a test gate does not, and many run at once. Until now the two did not
talk: `jmh-lane.sh` took its lock and WAITED FOR QUIET, and `gate.sh`
started whenever an agent asked. On a box where siblings gate back to
back the quiet never came. Measured 2026-09-26 (ready-merge): 101
attempts in an hour, the lane never ran once — load 17-65, up to three
busy sbt at a time. Waiting for quiet without anything that MAKES quiet
is starvation.

The fix is the readers–writers protocol with WRITER PREFERENCE
[Courtois, Heymans & Parnas 1971]: gates are readers (any number at
once), a benchmark is the writer (the box to itself).

1. **Every gate holds a token while it runs** — a file named by its pid
   in `$TMPDIR/okay-bench/gates/`. A dead pid's token is ignored and
   removed by whoever reads it, so a crashed gate blocks nobody.
2. **A benchmark files a request** — `$TMPDIR/okay-bench/want/<pid>` —
   the moment it is queued, before it waits for the lane lock.
3. **A gate whose start finds a live request waits** before starting
   sbt, and RUNNING gates are not touched: they finish, and their
   tokens go.
4. **The lane runs when no token is live and `quiet` holds** (the
   existing box reading — other projects, Docker and the operator are
   outside the protocol, and the quiet check at both ends of a lane
   stays exactly as it was).
5. **Both waits are bounded.** A gate waits at most
   `OKAY_BENCH_GATE_MAX_WAIT` (900 s) and then starts anyway, saying
   so; the lane that it then contaminates is discarded and retried by
   `jmh-lane.sh`'s existing end check. A lane waits for the lock at
   most `JMH_LANE_LOCK_WAIT` (3600 s) and for quiet as before.

When no benchmark is queued nothing changes: a gate's cost is one
`touch`, one directory listing and one `rm`.

## Interface

- `scripts/bench-window.sh` — SOURCED by `gate.sh` and `jmh-lane.sh`
  (one vocabulary, `quiet.sh`'s precedent): `bw_gate_enter`,
  `bw_gate_leave`, `bw_want`, `bw_unwant`, `bw_live gates|want`.
  Executed with `--status` it prints the live gates and requests.
- `OKAY_BENCH_DIR` (default `$TMPDIR/okay-bench`), `OKAY_BENCH_POLL`
  (10 s), `OKAY_BENCH_GATE_MAX_WAIT` (900 s), and
  `OKAY_BENCH_WINDOW=off` for a gate that must not wait (it still
  takes a token, so a benchmark still sees it).
- `jmh-lane.sh`: waits for the lane lock instead of refusing
  (`JMH_LANE_LOCK_WAIT` seconds, 0 = the old refusal); its quiet wait
  also requires no live gate token.

## Behavior

- [x] with no request, a gate enters at once and its token is gone
      after it exits (success, failure, or a signal)
- [x] with a live request, a gate waits, and enters as soon as the
      request goes
- [x] a gate waits no longer than its cap, then enters and says so
- [x] a request or token whose pid is dead is ignored and removed
- [x] the gate's token is taken BEFORE it looks for requests, and the
      lane's request is filed BEFORE it looks for tokens — so a gate
      and a lane starting at the same instant cannot both proceed
- [x] `jmh-lane.sh` does not start a lane while a gate token is live,
      and starts it when the token goes
- [x] `jmh-lane.sh` waits for a held lane lock and runs when it frees;
      `JMH_LANE_LOCK_WAIT=0` refuses as before
- [x] `--read` replays take no token (no sbt runs)
- [x] both selftests green under `sh` and `bash`

## Stage 2 — draining versus demoting (a measurement, not yet a decision; backlog `bench-window-demote-measure`)

On Apple silicon `taskpolicy -b -p <pid>` moves a running process to
the background QoS class, which the scheduler keeps on the efficiency
cores. If a gate demoted that way leaves the performance cores quiet
enough, the window need not wait for gates at all — they would slow
down instead of stopping. Memory bandwidth, the shared cache and heat
stay shared, so this is a hypothesis. The experiment: the control lane
(`MergeBenchmark.okaySourceSingleDrain`) (a) alone, (b) beside a
running gate demoted with `taskpolicy -b`, (c) beside the same gate
undemoted; alternating, two rounds. (b) within the control's own
error of (a) makes demotion the better protocol; otherwise draining
stays. Runs in the first window this stage makes.

## Decisions

- **Files and pids, not a daemon** — every piece of the existing
  coordination (`jmh-lane.sh`'s lock, `ci-runner.sh`'s) is a directory
  and a pid checked with `kill -0`; a daemon would be one more process
  to keep alive on a box whose idle reaper kills processes by rule.
- **Tokens in `gate.sh`, nowhere else** — `gate.sh` is the only road
  to sbt in this repo (AGENTS.md), so `gate-retry.sh` and
  `ci-runner.sh` inherit the token by calling it.
- **A cap on the gate's wait instead of a window budget** — a budget
  needs a clock both sides agree on and a rule for what happens at its
  end; a cap per gate bounds exactly the thing the operator cares about
  (how long a gate can be held) with no shared state at all.
- **pid reuse** — a recycled pid could keep a stale file "live". The
  window is bounded by the gate's cap either way, and macOS recycles
  pids slowly; not worth a start-time check yet.

## Results

**Stage 1, 2026-09-26.** `scripts/bench-window-selftest.sh` (6 cases,
under `sh` AND `bash`), `jmh-lane-selftest.sh` 4–4d (queue behind the
lock, `JMH_LANE_LOCK_WAIT=0` refusal kept, a live gate token holds the
lane, the request lives exactly as long as the lane) and
`gate-selftest.sh` 10 (a queued benchmark holds the gate's start; its
token is gone after; a `--read` takes none), the last also run by hand
under `/bin/sh`. Two mutants watched failing: a gate that ignores
requests (bench-window-selftest 2–4 red) and a lane that ignores
tokens (jmh-lane-selftest 4c red).

The one box behaviour the selftests cannot show — that a window
actually OPENS on this box with siblings gating — is shown by the
first lane run after landing: `ready-merge-numbers` is that lane.

Seen on the way, not this lane's: `gate-selftest.sh` 5 (a busy host
survives) is load-sensitive — its host is a shell spin, and on a
loaded box one 6 s window got under a whole second of CPU, which the
integer CPU count reads as 0 and the watchdog as a stall. It passes
alone in 12 s on master and on this branch; filed as
`gate-selftest-busyhost-load`.

**Stage 2** (drain versus demote) is carried as backlog
`bench-window-demote-measure`: it needs the window stage 1 makes.

## References

- P. J. Courtois, F. Heymans, D. L. Parnas. *Concurrent control with
  "readers" and "writers".* Communications of the ACM 14(10):667–668,
  1971. https://doi.org/10.1145/362759.362813
