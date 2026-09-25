# Working on this project — start here

Two scripts, both at the repo root, both self-documenting (`--help`):

```
./build.sh    tests, compile checks, packaging a deployable
./bench.sh    JMH benchmarks, the Wrocław streaming benchmark, A/B decisions, the history log
```

Nothing else is a valid way to invoke `sbt` in this repo — not a bare
`sbt test`, not `sbt compile`, not even a quick one-off check.

## `./build.sh`

```
./build.sh                      full gate: sbt test, through gate.sh
./build.sh test [sbt-command]   the same gate, or any other sbt command
                                 through it — a single module's test,
                                 a probe, anything
./build.sh compile              a cold-safe compile check (Test/compile)
./build.sh package <module-id> <module-dir> [image:tag]
                                 build one deployable's fat jar
                                 (+ Docker image if a daemon answers)
```

Every one of these runs the sbt command through `scripts/gate.sh`, never
bare `sbt`. That is not a style preference — `gate.sh` is what tells a
real failure apart from the one false red this repository is known to
produce (a Scala Native test process that dies silently and is
re-run, alone, before being believed); it is what catches a compile
warning ("no warnings, ever" is enforced, not just requested); and it
is what notices a build that has gone silent AND idle for 8 minutes —
a genuine stall, like a JMH fork stuck on someone else's lock file —
takes a thread dump, kills it by PID, and says so, instead of leaving
it to be discovered an hour later. A bare `sbt` invocation gets none
of that, silently. `./build.sh test <anything>` is the same one path
for a 30-second spot check as for a full landing — see AGENTS.md's
"single-path-verification" for why that matters in a repo several
agents commit to.

## `./bench.sh`

```
./bench.sh run <pattern> [sbt-module]   JMH in one module (default: okayJVM)
./bench.sh compare <pattern>            JMH in the `compare` module (ecosystem lanes)
./bench.sh wroclaw [days] [rounds] [fraction]
                                         the Wrocław streaming benchmark
./bench.sh ab <name>                    an A/B that decides a default
./bench.sh history [grep-pattern]       the benchmark history (history.d + the archive), tabulated
```

`run` and `compare` go through `scripts/gate.sh` too, for the same
stall watchdog — a hung JMH fork looks exactly like a stalled build
(silent, near-0% CPU) and gets caught the same way. `wroclaw` and `ab`
are their own multi-stage drivers (one JVM per lane, their own timing)
and run directly, unwrapped.

Before trusting a benchmark number, read the `performance` skill
(`.agents/plugins/performance/commands/performance.md`): one
measurement is a hypothesis, alternate A/B in the same session, never
compare a fresh number against yesterday's table. The numbers
themselves, with methodology and the refuted experiments, are in
[`docs/benchmarks.md`](docs/benchmarks.md); the raw log is one file
per measurement in `src/jmh/history.d/` (`scripts/history.sh new
<measure>` names it by its UTC instant; tab-separated, `date sha
host_load workload mine ref ratio note`), with everything before
2026-09-25 in the archive `src/jmh/history.tsv`.

## Where the rest of the documentation lives

| | |
|---|---|
| [`README.md`](README.md) | what this library is and why, for someone who has never seen it |
| [`AGENTS.md`](AGENTS.md) | the full policy for working in this repo: coordination between agents committing to one `master`, build facts that bite, the gate, the boards — read this before doing anything non-trivial |
| [`docs/README.md`](docs/README.md) | the module index — one page per module, guide/tutorial/reference |
| [`specs/`](specs/) | design specs, written before the code, kept in sync with it |
| [`SPRINT.md`](SPRINT.md) / [`BACKLOG.md`](BACKLOG.md) / [`CHANGELOG.md`](CHANGELOG.md) | pointers to `sprint.d/`, `backlog.d/`, `changelog.d/` — read with `scripts/board.sh` / `scripts/changelog.sh` |
