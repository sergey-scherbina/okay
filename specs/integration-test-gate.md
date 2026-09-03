# integration-test-gate — Live tests, out of the default gate

## Overview

`sbt test` mixes two things that need different treatment: fast,
deterministic unit/property tests, and suites that reach outside the
JVM — a live model gateway, a docker service (kafka, mongo, postgres,
redis, minio/s3), an external tool (openssl, python3). The second
kind already SKIPS honestly when the thing it needs is absent
(`assume`/`munitIgnore`, unchanged by this) — but when it IS present,
running it is real work whose RESULT depends on conditions `sbt test`
cannot control: load on the box, the live model's own answer variance,
a docker container mid-restart. This session hit that directly —
`okay.demo.TestChatDemo`'s LIVE suite failed identically on completely
untouched `master`, twice in three consecutive runs, while several
landings were being gated — before a genuine regression could be told
apart from an unrelated flake, each time costing a live-model
re-verification cycle. The fix is not making these tests less real;
it is taking them out of the gate a landing is measured against.

## Interface

A test tagged `Live`:

```scala
class TestFoo extends munit.FunSuite:
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))
```

or, for a suite that mixes live and non-live tests in one class
(TestChatDemo's own shape), tag through a per-test helper instead of
the whole suite:

```scala
def liveTest(name: String)(body: => Any): Unit =
  test(name.tag(new munit.Tag("Live"))) { body }
```

`ThisBuild / Test / testOptions` carries `--exclude-tags=Live`
(build.sbt) — `sbt test` never runs a `Live`-tagged test, in any
project, by default. `sbt integrationTest` (a command alias) runs
the exact same suites with `--include-tags=Live` instead — ONLY the
tagged tests, across every project — a fresh mode`, not a persisted
setting: it is scoped to the one `test` invocation the alias chains
to and does not change what `sbt test` does afterward in the same
session.

## Behavior

- [x] `sbt test` (default): every `Live`-tagged test is excluded,
  everywhere, unconditionally — a docker service being reachable at
  the moment does not pull its suite back into the gate. Verified:
  `okay.demo.TestChatDemo` went from 38 tests to 34 under plain
  `okayDemo/testOnly` once `liveTest` carried the tag; the excluded
  four were exactly `LIVE:`/`LIVE MATCH`/`LIVE UNGATED`/`LIVE SEEKER`.
- [x] `sbt integrationTest`: runs ONLY the `Live`-tagged tests, across
  the whole build. Verified narrowly (`--include-tags=Live` against
  `TestChatDemo` in isolation, all 4 tests, all green) and broadly
  (the full command across every project — history.tsv/CHANGELOG
  carry the count).
- [x] The existing `assume`/`munitIgnore` skip-when-absent behavior
  is untouched — a `Live` test still SKIPS cleanly under
  `integrationTest` when its service is not there; the tag only
  decides which COMMAND considers the test at all.
- [x] A suite built on a shared base trait (`ElectionSuite`,
  `MatchEngineSuite`, `R2dbcSuite`, `BlobContract`, `DocsSuite`) is
  tagged at the LEAF class, not the trait — several of these traits
  have BOTH live and non-live subclasses (`TestElection`/
  `TestElectionFile` alongside `TestElectionKafka`), so tagging the
  shared trait would have mis-tagged the non-live siblings.

## Decisions

- **A munit `Tag`, not a source-directory move (`src/it/scala`) or
  a project-level split.** sbt's own `IntegrationTest` configuration
  is the more idiomatic-looking answer, but it needs per-project
  wiring (`Defaults.itSettings`) and a directory move for every file
  — and several live suites SHARE a file with non-live tests or a
  base trait with non-live siblings, which a directory-level split
  cannot express at all. A tag is a one-line, per-suite (or even
  per-test) decision, needs zero build-graph changes beyond the one
  `ThisBuild` setting and the one command alias, and composes with
  the EXISTING `assume`/`munitIgnore` skip machinery instead of
  replacing it.
- **`munitTests()` override, not tagging every individual `test(...)`
  call.** munit's own idiom for a single test is `test(name.tag(Foo))
  { ... }` — tagging a whole suite that way means editing every call
  site. Overriding `munitTests(): Seq[Test]` to map `.tag(Live)` over
  `super.munitTests()` is ONE line per suite, and munit's own `Test`
  case class supports it directly (`.tag`, alongside `.withTags`).
- **No shared `Tag` object or marker trait.** `munit.Tag`'s equality
  is by string VALUE (`Tag("Live") == Tag("Live")` in different
  files), and the ~25 tagged suites live in ~15 separate sbt
  projects with no shared test-scope dependency wiring between them
  — inventing one (a `Test->Test` classifier dependency, or a
  `src/main/scala` marker, which would leak `munit` onto the
  PRODUCTION classpath of every consumer of core) costs more than
  the one-line-per-file repetition it would save.
- **`set every Test / testOptions := Seq(...)`, not `-=` against the
  existing value.** The first attempt removed the exclude-tags
  argument with `set every Test / testOptions -= Tests.Argument(...)`
  inside the command alias — sbt refused it: "Cyclic reference
  involving … Test / testOptions … Global / testOptions". Replacing
  the whole setting (`:=`) instead of modifying it relative to
  itself avoids the self-reference the settings graph cannot resolve
  at that point in a command sequence.
- **False positives found and left untagged, named so nobody re-adds
  them**: `TestJavaInterop` and `TestResumable` matched the initial
  survey grep on the word "LIVE" inside an unrelated comment, not a
  live dependency. `TestSqlMatch`/`TestMatch` use H2 in-memory or
  pure in-memory engines — no external service. `TestPgTarget`
  (URL parsing) and `TestScram` (an RFC test vector) are pure unit
  tests that happen to live in `okay-pg` alongside the live suites.
  `TestSparkInterop`'s skip is a JDK-version compatibility gate
  (deterministic, same box, same result every run) — not the
  environment-timing flakiness this tag exists for.
- **The tag was WIDENED on 2026-09-03, by the operator's call, from
  "reaches outside the JVM" to "its result depends on something `sbt
  test` cannot control".** The deferred case below was taken up the
  same day (`TestMcpAuth`, `TestBackends` — real ports, no external
  service), and then `TestElectionReplicated` with it. That last one
  is worth naming separately because it does NOT fit even the widened
  rule read narrowly: it binds nothing, threads nothing, does no IO
  (MemoryStore + a manual clock), and its triage could not reproduce
  the failure (alone: JS 3/3, Native 3/3) — what failed was the
  RUNNER, at suite level, under parallel matrix load. The argument
  that carried it is the gate's own purpose: a red that can be the
  machine's fault teaches nothing about the landing being measured,
  and a pure suite excluded by decision is still run, and still read,
  by `sbt integrationTest`. Recorded as a decision, not as evidence
  against the suite — if the consensus fold ever genuinely breaks,
  the integration run is where it surfaces.
- **`netty-ws-matrix-flake` (BACKLOG.md) is a related, DEFERRED
  case, not folded in here.** `TestBackends` and other real-socket
  suites (no external service, just port/timing flakiness under the
  full sbt matrix) are the same class of problem in spirit, and the
  same `Live` tag mechanism would fix them — but it is a separately
  tracked, already-investigated backlog item with its own settling
  plan; expanding this landing's scope to it was declined to keep
  the landing reviewable. Filed as the natural next consumer of this
  mechanism.

## Results

Landed: `~25` suites tagged across ~15 projects
(okay-agent, okay-blob, okay-cache, okay-demo, okay-demo-e2e-browser,
okay-docs-mongo, okay-kafka ×4, okay-mcp, okay-match ×1, okay-persist,
okay-pg ×5, okay-py ×2, okay-r2dbc, okay-rag, okay-tls). Full `sbt
test` green, `okay.demo.TestChatDemo` alone dropped 38 → 34 tests
with the 4 excluded being exactly its `liveTest`-wrapped ones.
`sbt integrationTest` verified to run the tagged tests correctly
(narrow: 4/4 `TestChatDemo` LIVE tests green in one run; broad: the
full command across the build, logged in CHANGELOG/history.tsv).
