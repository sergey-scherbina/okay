# audit-ready — the architecture an okay application follows to pass its own audit

## Overview

okay-audit (specs/okay-audit.md) checks a boundary; this spec says what
to build so that the check PASSES and what it produces is the evidence a
DORA reader asks for (Regulation (EU) 2022/2554 Art. 8 inventory, Art. 9
change, Art. 13/17 root cause, RTS 2024/1774 Art. 12 logging; the business
side is `~/work/my/jobs/biz/regulatory-mapping.md`). Operator ask,
2026-10-05: *«спроектируй чистую архитектуру полностью совместимую с DORA
аудитом … что нужно нам самим поменять … надеюсь не много».*

The answer is: little. The library already has every mechanism — the
row as the declaration (`A ! F`), `Replayable[F]` as the replay discipline
spelt as a type, `provide`/`Module` as compile-time composition, `Resource`,
okay-kernel's ports and forbidden edges, okay-watch's sealed evidence
journal — and this spec adds the RULE that connects them to the audit,
plus the few places where our own code breaks the rule today.

## The rule, in one sentence

**A program's row names every way it reaches the world; the world is
reached only through handlers; handlers live in modules the audit lists;
everything a handler is told is journaled.** Four clauses, four layers.

## The four layers

| Layer | What is in it | Audit layer | May reference |
|---|---|---|---|
| **1. Domain** | values, rules, decisions; `A ! F` programs whose `F` is the ports below | `business` | the core, okay-data, its own values — none of `Boundary.Default` |
| **2. Ports** | the effects: `Clock`, `Random`, `Db`, `Http`, `Files`, `Console`, `Uid` — operations as data, no bodies | `business` | nothing but the core |
| **3. Handlers** | one module per external thing: the Postgres `Db`, the real `Http` over the platform's `Web`, the system `Clock`; the journaling wrapper; the replaying wrapper | `handlers` | whatever that thing needs — and that is what the inventory lists |
| **4. Runtime** | the core, `Async`, the platform, the scheduler; the main that assembles 1–3 with `provide`/`Module` or the kernel | `runtime` | the JVM |

Dependencies point DOWN only: 1 → 2 → core; 3 → 2; 4 → all. okay-kernel's
`OkayModules.forbid` states that in the build and fails the load on a
violation (specs/kernel.md "Forbidden edges"); okay-audit states it in the
bytecode and fails the build on a reach. Two witnesses, one rule.

## Interface — what an application writes

```scala
// 2. ports — a module of its own, depends on the core only
enum Clock[A]  derives Effect: case Now()               extends Clock[Instant]
enum Random[A] derives Effect: case Next(bits: Int)     extends Random[Long]
enum Db[A]     derives Effect: case Query(sql: String, args: Vector[Any]) extends Db[Rows]
enum Http[A]   derives Effect: case Get(url: String)    extends Http[Option[String]]
                               case Post(url: String, body: String) extends Http[String]

// 1. domain — `business`: the row IS the inventory of what this code may do
def placeOrder(o: Order): Receipt ! Db + Payments + Clock

// 3. handlers — `handlers`: each one a module the audit lists
object SystemClock:   def run[A, R](p: A ! Clock + R): A ! R        // System.currentTimeMillis lives HERE and nowhere else
object Postgres:      def run[A, R](ds: DataSource)(p: A ! Db + R): A ! R
object Journaled:     def run[A, R](j: Journal)(p: A ! Db + Http + Clock + R): A ! R   // records every ask and answer, sealed
object Replayed:      def run[A, R](j: Journal)(p: A ! Db + Http + Clock + R): A ! R   // answers from the journal; a request not recorded is REFUSED by name

// 4. runtime — main: the one place that names real things
@main def app = Async.run(Postgres.run(ds)(RealHttp.run(SystemClock.run(Journaled.run(journal)(placeOrder(o))))))
```

The `Replayable` constraint (Replayable.scala, durable-workflow stage 1)
is the type-level half: a program meant to be replayed may hold only
effects whose re-execution nobody observes; `Clock`, `Random`, `Uid` and
every port are therefore NOT in it — they are answered by a handler, and
the handler is what the journal records. That is already how okay-watch's
trace works: every collector reads through `Web`, `Evidence` wraps `Web`,
and a trace rebuilds byte for byte from the kept answers with the network
untouched (okay-watch specs/trace-evidence.md, TestTraceEvidence).

## Behavior

- [ ] a domain module under `auditLayer := "business"` passes `sbt audit`; a `Clock.Now()` in it is a port operation, a `System.currentTimeMillis` is a finding
- [ ] the ports module references nothing but the core (forbidden edge in the build + zero reaches in the audit)
- [ ] each handler module appears in the inventory with exactly the providers its external thing needs (`java.sql` for Postgres, `java.net` for the real Http) and nothing else
- [ ] the journaling handler records every operation of every port with its answer and a timestamp FROM THE JOURNALED CLOCK, sealed (each entry hashes the one before); the replaying handler answers from it, refuses a request not recorded, by name, and runs with the network unreachable
- [ ] `main` is the only business-visible place where a real handler is named; a test swaps every handler with `provide` and no production code changes
- [ ] the evidence pack of a build is: `target/audit/report.{txt,json}` (boundary + inventory, with input hashes), the forbidden-edge rules, the journal export of a run, and a replay report — four files, no narrative
- [ ] okay-data passes as `business`: `Hlc` and `Uid` take their clock and random source as parameters with the platform's as the default given (backlog.d/okay-data)
- [x] okay-watch: the pure rules, screening, trace decisions, case values, filing and ISO code live under `okaywatch.domain` as `business`; collectors, transports, feeds, APIs, storage and mixed I/O companions remain `handlers`. Package-prefix classification proves the boundary within one sbt project.

## What we change in our own code (the honest list)

Measured 2026-10-05 (okay-audit dogfood; a grep over okay-watch for the
rule's APIs, per package):

| Where | What breaks the rule | Change | Size |
|---|---|---|---|
| okay-audit | layers are per sbt PROJECT; okay-watch is one project with business and handlers side by side (`okaywatch.trace` 152 reaches in 17 files, `okaywatch.api` 246 in 47, `okaywatch.collect` 26) | `auditLayer` accepts package prefixes too: `auditLayers := Map("okaywatch.trace" -> "business", "okaywatch.collect" -> "handlers", …)`; a class is classified by the longest matching prefix, the project's layer is the default | small — one match in `Audit.run`, the manifest gains a column |
| okay-data | `Hlc.system` reads `System.currentTimeMillis`; `Uid` reads `scala.util.Random.nextLong` | clock and random source as parameters, platform's as the default `given` in a `handlers` companion (`okay.data.platform`) or in okay-platform | small — two constructors; callers unchanged through the default |
| okay (core) | there is no `Clock` / `Random` port in the core: every application re-invents them, and okay-watch reads `Instant.now` in 20 files | `okay.Clock` and `okay.Random` effects in the core (operations only), handlers in okay-platform (`SystemClock`, `SystemRandom`) and in the journal (`Journaled`, `Replayed` answer them from the record) | medium — new, additive; the `Replayable` doc already names them as the effects that are NOT replayable, which is exactly why they must be ports |
| okay-watch `trace` | `Evidence` already journals `Web`; legacy replay already retained the sealed report time, but did not record a typed Clock answer (the earlier rebuild-time diagnosis was refuted) | report time through the `Clock` port, journaled with `Web`; modern replay refuses missing answers, legacy replay preserves the sealed time | implemented in okay-watch |
| okay-watch `api` | pages and routes read the clock, files and the network directly (246 reaches) — correct for an API layer, which IS a handler layer | classify as `handlers`; nothing to change | none |
| okay-watch root (`Rule`, `Watch`, `Risk`) | the rules read `Instant.now` and `Random` (77 reaches in 21 files, most in `Demo` and `Synthetic`) | `Demo`/`Synthetic` are handlers (they make data); `Rule`/`Watch`/`Risk` take time from the `Clock` port | small |
| okay-codec, okay-bayes, okay-java | reach files/processes/TLS/`Random`/reflection | stay `handlers`; okay-bayes gets its `Random` as a parameter when a replayable model is wanted (not now) | none now |

Everything else — the core's `Replayable`, `provide`/`Module`, `Resource`,
the kernel's ports and forbidden edges, okay-watch's sealed journal and
rebuild — stays as it is. The list above is three small lanes and one
medium one; none changes a public signature except by adding a default.

## Out of scope

- Runtime enforcement (ScopedValue + agent) and JPMS enforcement: specs/okay-audit.md, "JPMS" (jpms-boundary).
- A "clean architecture" rename of okay's packages: not needed for the rule; the one-package-per-module question is okay2's (jpms-boundary, stage C).
- The AI Act's Art. 12 record-keeping for high-risk systems: the same journal, a different reader; not before 2027.

## Decisions

- **Ports are effects, not interfaces.** An interface (`trait Clock { def now: Instant }`) passed as a parameter is the same dependency inversion, but it is invisible to the row and to `Replayable`, and a journaling wrapper has to be written per interface. An effect is journaled once, by the handler that sees every operation as data.
- **The audit classifies by package prefix as well as project**, because a product like okay-watch is one project by design (one jar, one assembly) and splitting it into sbt projects to satisfy a scanner would be the tail wagging the dog. The forbidden-edge rules stay per project; the two witnesses then cover different grains, which is fine.
- **The clock is a port even where replay is not wanted today**, because the moment a buyer asks for "root cause from the journal" (DORA Art. 13) the time of the run must be in the journal, and retrofitting it is the 20-call-site lane above done under pressure.
- **Not a framework.** No base class, no annotation, no container: the four layers are four `auditLayer` values, a dozen `forbid` lines and the discipline the row already imposes.

## Stages

1. okay-audit: package-prefix layers (`auditLayers`), manifest column, the dogfood states okay-watch's packages. *(lane: audit-package-layers)*
2. core: `Clock` and `Random` ports; okay-platform handlers; `Journaled`/`Replayed` answer them. *(lane: clock-random-ports)*
3. okay-data: `Hlc`/`Uid` sources as parameters; okay-data back to `business`. *(lane: data-clock-and-random-reach, filed)*
4. okay-watch: domain classification, recorded Clock answers beside `Web`, `auditDomain` in Dagger and Java-only audit/evidence export. Implemented in okay-watch commits `b24d2e0`, `2d4ed53`, `fe0cb07` (specs/audit-domain-classification.md there).

## Results

Stages 1 through 3 are now in the library: `auditLayers` classifies a
single artifact by longest package prefix without leaking a rule into another
module; `Clock` and `Random` are core operations with deterministic test
handlers and platform handlers; and `Hlc` / `Uid` take their sources as
parameters, moving the ambient sources to `okay-platform`. `okay-data` is
therefore again a `business` module and `sbt audit` passes.

Stage 4 is implemented in okay-watch. Its unchanged boundary policy passes
219 domain classes with no findings or allows; mixed I/O remains inventoried
as handlers. Clock answers are sealed beside Web answers and modern replay
refuses unrecorded operations by name. Legacy evidence still rebuilds byte
for byte from its sealed report time.

The Java-only evidence exporter verifies the original journal, copies kept
answers and reports, replays the copy, and hashes the audit policy, report,
build/dependency rules and exported files. Offline replay no longer opens SQL
progress storage. A strict jlinked image without SQL/Unsafe passed export
with OS-denied network access and writes to the original evidence. Scoped
tests and local audit passed; the full Dagger container pipeline was not run.
