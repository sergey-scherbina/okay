## backlog-audit-0918 - what on the boards is real, what was already done, and what the library is still missing

The operator asked, with the section counts in hand (okay-core 28,
okay-agent 20, okay-codec 5, okay-http 5, okay-rag 5, okay-resilience
4, build 4): which of these are actually needed, and what does the
library still lack. The answer had to start with the record, because
the counts were wrong.

**THE RECORD WAS 95 ENTRIES BEHIND.** The board that says "open work
only" held 82 files whose first line was `- [x]` — closed in place,
never moved — and twelve more that READ as open whose work had landed
or whose text answered itself: `dialogue-continue-as` (landed
2026-09-17), `federation-refusal` (landed, spec stage 2 all ticked),
`parse-depth-timer-warns` (the timer holds its value since 63db4156
and the gate has refused warnings since 2026-09-11), the two okay-http
port ledgers (every suite they name is `Live` since nio-port-scope),
all four okay-resilience entries (three closed by hedge-bounds the
same day, the fourth's last item is okay-persist's `Saga`),
`growing-order-instrumented-repro` (its own last paragraph says
"closed as a question"), and two entries that were decisions written
as tasks. Plus one torn fragment ("~~Say so.~~") of an archived entry.
Ten `optics-outside-*` items were filed under `openapi/`, so the
optics-outside section read as all-closed with its five open items in
somebody else's section. Three CLOSED arcs sat in the sprint's Doing
and Queue. One entry named a function that had moved
(`Secrets.scheme` is `Conf.scheme`). All moved verbatim to
BACKLOG-ARCHIVE.md, seven empty sections retired, the sprint emptied,
the fact corrected. The residue of the workflow arc — the scan that is
"wrong for millions" — is now a backlog entry (`workflow-timers-index`)
instead of a paragraph in a closed sprint arc.

**AND THE GUARD, so it does not happen a sixth time.** Five stale
records were found by readers in one day before this lane; the reason
is that nothing checked the board. `scripts/board.sh --check` and
`TestBoardEntries` now refuse a `- [x]` first line under `backlog.d/`
— a closed entry moves to the archive the day it closes. Watched to
fail on a planted entry before the plant was removed.

**WHAT IS REAL, of the 92 that remain.** Read every one against the
files; the ranking is by what it would cost a user, not by section.

- **One correctness defect in the core, and it is the operator's
  decision:** `growing-order-drain-guarantee`. A producer's element
  is delivered ahead of its own predecessors across the growing
  channel's one-shot swap — mechanism named, reproducer in
  `ProbeGrowingOrder`, two fixes refuted by the trace, one candidate
  (seal part 0 at adoption) unpriced on the push path, and the third
  option is to weaken the per-producer FIFO promise, which the entry
  argues against because the actor mailbox stands on it silently.
  Nothing else open on the boards is a known wrong answer.
- **Two hazards in the gate:** `twonode-fixed-ports` (two real JVMs on
  hardcoded 18091/18092 in the DEFAULT gate — two agents gating at
  once collide, seen once), and `ci-native-flake` (the nightly Native
  job has no rerun-alone for the lost-process false red the local gate
  already handles).
- **One performance defect with three quarters left:**
  `merge-chunk-size-curve-inverted` — okay's chunked merge slows as
  the chunk grows where every competitor speeds up; a quarter was the
  Take/Writer pairing in `unchunked`, the rest is unlocated.
- **Small and real, each an afternoon:** `aggregator-sum-hides-its-
  specialization` (an `OfLong` overload for `sum` at `Long`),
  `openapi-ops` (the last four undeclared operations; needs an answer
  type that carries its status, a design worth one decision),
  `ci-affected-tests-only`, `spark-4-2`, `json-raw-nesting-jmh-pending`
  (a re-measure on a quiet box), `codec-two-roads-audit` (where else
  does a fast road exist that nothing takes by default — the last such
  check was worth 37x), `scan-into-the-other-scanners`,
  `http-streaming-responses` (Netty aggregates `r.body.bytes`,
  Netty.scala:95, while `Response.body` is already a `Source`).
- **Correct as records, gated on a trigger nobody has pulled** — about
  fifty entries: the five optics-outside candidates, the handler-fusion
  and freer-base roads, the direct/staged roads, the four
  `Delim`/`Once`/`Logic` gates, both UI native records, bulk, deploy,
  R, py-arrow, the Wrocław and windows measurements, the three ledgers
  (native runner, E198, cold TASTy). Each says what would open it.
  They are not work; they are the reasons work was not done, kept so
  it is not re-derived.
- **Not library gaps at all — a data programme:** every one of the
  twenty okay-agent/intent entries is gated on rows a PERSON writes
  (a second author, 40-60 out-of-domain rows, a fixture twice the
  size) or on real traffic. The distillation lanes measured generated
  rows at zero. No agent can move them and the section should be read
  that way.

**WHAT THE LIBRARY IS MISSING**, argued from ROADMAP P13 (the
operator's order) and today's boards, not from a wish list:

1. `Validated` (P13 #1) — DONE; the applicative arc closed today.
2. Durable workflows as the flagship (P13 #2) — the model is complete
   and every stage is ticked; what is missing is the OPERATIONS an
   adopter at scale needs: an index for due timers and waiting signals
   instead of a scan, and a loop that calls `tick` so nobody writes
   one (`workflow-timers-index`, filed by this lane). That is the gap
   between "Temporal's core idea" and something sold as an engine.
3. Dataflow as the embedded tier (P13 #3) — two things the roadmap
   already backlogs and nobody has taken: the PUBLISHED CEILING
   (`dataflow-ceiling` — the throughput and state size past which a
   cluster is the honest answer, with the bottleneck named) and the
   Flink migration PAGE (`dataflow-flink-migration-path`; the engine
   work is done). Stage 12 is blocked on machines that are not this
   one and is not to be pretended at.
4. Capability lists from `Static` for okay-di (P13 #4) —
   `di-needs-from-static`, correctly triggered on the first time a
   declared need and the code disagree.
5. The one hole under everything: the growing channel's order across
   its swap. It is a decision before it is a lane, and it is listed
   first above for that reason.

**RE-CHECKED THE SAME EVENING (backlog-recheck), when the operator said
the data looked stale — and six of the 92 were.** `optic-law-rewrites`
had LANDED that afternoon (the audit listed it as open); `windows-int-
key-panes` was REFUTED on 2026-09-11 by the very lane whose archived
entry sat beside it; `gate-warm-warning-blindness` asks for exactly the
count the gate has printed since the watchdog; `freer-base` and
`freer-base-stage2` were one open item filed twice; and two entries
that call themselves SUPERSEDED in their first line were still ticked
open. The sweep's rule caught only `- [x]`; an entry closed in PROSE
and left `- [ ]` is the shape it missed, and the six are in the archive
now. The count of open work is 86. The verdict above stands with one
correction: `optic-law-rewrites` is not open.

Commits: this one, and the recheck.
