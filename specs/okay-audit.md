# okay-audit — the effect boundary, checked at every build

## Overview

okay's type `A ! Db + Payments` names the effects a program DECLARES. On the
JVM nothing stops the body from doing `new java.net.Socket(...)` as well: the
type system tracks declared effects, it does not forbid undeclared ones.
Haskell has `unsafePerformIO`, Rust has `unsafe`; the honest claim is the same
in all three — not "a function without the effect cannot do it", but **"every
reach to the outside world sits in a small, listed set of handlers, and each
build checks that nothing else reaches"**. A small trusted base plus a
boundary check is the control; this module is the check.

The check reads BYTECODE, not Scala source: it then covers the Java facade
(specs/java-effects.md, specs/java-capabilities.md), Kotlin or Clojure
callers, and every third-party jar on the business classpath — which a
compiler plugin never sees. A class file's constant pool lists every class,
field and method it refers to (JVMS §4.4); that list, per module, against a
rule set, is the whole job.

The report has a second reader. For DORA (Regulation (EU) 2022/2554, Art. 8:
inventory of ICT assets and their dependencies) the handler layer's list —
which module touches which external API — IS the dependency inventory,
generated from the code and current at every build. One scanner gives both
the proof of the boundary and the inventory (operator ask, 2026-10-05; the
business side is `~/work/my/jobs/biz/regulatory-mapping.md`, section 2a).

## Interface

```scala
package okay.audit

/** one reference a class file makes: owner class, member name, descriptor;
  * `kind` is Class | Field | Method | InterfaceMethod | MethodHandle |
  * Native (an own `ACC_NATIVE` method, owner = the class itself) */
final case class Ref(from: String, kind: Ref.Kind, owner: String, name: String, descriptor: String)

object Scan:
  def classFile(bytes: Array[Byte]): Vector[Ref]          // one class, JVMS §4 constant pool
  def directory(dir: Path): Vector[Ref]                   // every .class under it
  def jar(jar: Path): Vector[Ref]                         // every .class entry
  def classpath(entries: Seq[Path]): Map[Path, Vector[Ref]]

enum Layer:
  case Business   // may touch the outside world only through okay effects
  case Handlers   // the trusted base: listed, never failed
  case Runtime    // okay core and its platform: listed as the runtime, never failed

/** a rule names an API prefix (`java.net.`) or one member (`java.lang.System#currentTimeMillis`) and WHY it is outside the boundary */
final case class Rule(api: String, why: String)

/** an allow is a named, reasoned exception for ONE module; unused allows are reported */
final case class Allow(module: String, api: String, reason: String, owner: String)

final case class Boundary(layers: Map[String, Layer], rules: Vector[Rule] = Boundary.Default, allows: Vector[Allow] = Vector.empty)
object Boundary:
  val Default: Vector[Rule]   // the list below

final case class Finding(module: String, ref: Ref, rule: Rule)
final case class Inventory(module: String, layer: Layer, byProvider: Map[String, Vector[Ref]])   // provider = the owner's top package (`java.sql`, `com.zaxxer.hikari`)
final case class Report(inputs: Vector[(Path, String)],   // path and sha-256 of every scanned jar/dir
                        findings: Vector[Finding],         // business modules only; empty is the pass
                        inventory: Vector[Inventory],      // handlers and runtime
                        unusedAllows: Vector[Allow]):
  def passed: Boolean = findings.isEmpty
  def text: String                                         // for people: findings first, then the inventory
  def json: Json                                           // for the evidence pack (okay-codec)

object Audit:
  def run(boundary: Boundary, modules: Map[String, Seq[Path]]): Report
```

sbt (stage 1): `auditLayer := Layer.Business | Handlers | Runtime` per project,
`audit` on the root scans every project's `Compile / products` and
`Compile / dependencyClasspath` under its layer (a jar inherits the layer of
the project that brings it), writes `target/audit/report.{txt,json}`, and
FAILS the build on a finding. A project without `auditLayer` is reported as
`Untracked` — visible, not failed, so adoption can be gradual; the goal state
is zero untracked.

CLI (stage 2): `okay-audit --business a.jar,b.jar --handlers h.jar --runtime okay-core.jar [--allow allows.conf] --report out/` — the same scanner for Maven and Gradle users of the Java facade.

### The default rules (`Boundary.Default`)

API prefixes and members a business module may not reference directly; each
has a one-line `why`. Grouped:

- **network:** `java.net.`, `javax.net.`, `java.nio.channels.`, `java.rmi.`, `javax.naming.`, `java.net.http.`
- **files and console:** `java.nio.file.`, `java.io.File`, `java.io.FileInputStream`, `java.io.FileOutputStream`, `java.io.FileReader`, `java.io.FileWriter`, `java.io.RandomAccessFile`, `java.io.Console`, `java.lang.System#out`, `java.lang.System#err`, `java.lang.System#in`, `scala.Console`, `scala.io.Source`, `java.util.logging.`
- **databases:** `java.sql.`, `javax.sql.`
- **processes and the JVM:** `java.lang.ProcessBuilder`, `java.lang.Process`, `java.lang.Runtime#exec`, `#exit`, `#halt`, `#addShutdownHook`, `java.lang.System#exit`, `#load`, `#loadLibrary`, `#getenv`, `#getProperty`, `#setProperty`, `scala.sys.process.`, `scala.sys.package#env`
- **time and randomness (nondeterminism):** `java.lang.System#currentTimeMillis`, `#nanoTime`, `java.time.Clock#system*`, `java.time.Instant#now`, `java.time.LocalDate#now`, `java.time.LocalDateTime#now`, `java.time.ZonedDateTime#now`, `java.time.OffsetDateTime#now`, `java.util.Date#<init>()`, `java.util.Random`, `java.util.concurrent.ThreadLocalRandom`, `java.util.SplittableRandom`, `java.security.SecureRandom`, `java.util.UUID#randomUUID`, `scala.util.Random`
- **threads (the runtime's job, not a program's):** `java.lang.Thread`, `java.util.concurrent.Executors`, `java.util.concurrent.ForkJoinPool`, `java.util.concurrent.CompletableFuture#runAsync`, `#supplyAsync`, `scala.concurrent.ExecutionContext$Implicits`
- **escape hatches (the ways around any static check):** `java.lang.reflect.`, `java.lang.invoke.` **except the two bootstraps every compiler emits** (`java.lang.invoke.LambdaMetafactory`, `java.lang.invoke.StringConcatFactory`, and `java.lang.runtime.ObjectMethods` for records), `java.lang.Class#forName`, `#getMethod`, `#getDeclaredMethod`, `#getField`, `#getDeclaredField`, `#newInstance`, `#getConstructor`, `#getDeclaredConstructor`, `java.lang.ClassLoader`, `java.net.URLClassLoader`, `java.lang.foreign.`, `sun.`, `jdk.internal.`, `java.io.ObjectInputStream`, `java.io.ObjectOutputStream`, `java.lang.Runtime#getRuntime` (only to reach the members above), and any own method with `ACC_NATIVE`

A business module reaches all of these THROUGH an effect whose handler lives
in a `Handlers` module (`Clock`, `Random`, `Console`, `Db`, `Http`, ...) or
through okay's runtime (`Async`, `Produce`). The rule list is a value: a
buyer's own additions (`com.example.legacy.`) are rules like any other.

## Behavior

- [x] a business class that constructs `java.net.Socket` is a finding naming the class, the member, the API and the rule's `why`; the build fails
- [x] a lambda (`LambdaMetafactory` bootstrap), a string concatenation (`StringConcatFactory`) and a record's `ObjectMethods` in a business class are NOT findings
- [x] `Class.forName`, `Lookup.findStatic`, `ObjectInputStream` and an `ACC_NATIVE` method in a business class are findings — the escape hatches are closed by the same check
- [x] a jar on a business project's classpath is scanned under the business rules and a finding names the jar, so a library that does I/O must move to the handler layer or be allowed by name
- [x] a handler module's references are listed, never failed; the inventory groups them by provider (the owner's top package) and names the module
- [x] okay core and its platform are the `Runtime` layer: listed as such, their thread and socket use is not a finding anywhere
- [x] an `Allow` has a module, an API, a reason and an owner; one without a reason is refused by name before any scan; an allow nothing matched is reported as unused
- [x] the report's inputs carry each jar's and directory's sha-256 so a report ties to one build; `text` and `json` are deterministic (sorted) for the same inputs
- [x] a project without a layer is `Untracked` in the report, not failed
- [x] dogfood: `audit` over okay's own build with `okay`/`okay-async`/platform as `Runtime`, the I/O modules (`okay-http`, `okay-sql`, `okay-persist`, `okay-r`, `okay-py`, ...) as `Handlers`, and `okay-java`'s examples plus one chosen pure module as `Business` passes, and its inventory names every provider those modules touch — the first inventory for the evidence pack
- [x] cost: the whole okay classpath scans under the time of one small test suite (measure and record in Results)

## Out of scope

- **Runtime enforcement** (a `ScopedValue` "inside a handler" plus an agent intercepting I/O constructors): layer 4 of the business document, only if a buyer requires enforcement in production rather than at build. Not this module.
- **JPMS descriptors and JVM flags** (`--illegal-native-access=deny`, no `--add-opens`): stage 3 records them into the report from a launcher config; this module does not generate `module-info.java`.
- **Proving the core**: the `!` type and handlers follow the type-and-effect discipline (Koka, Eff, Frank); the core's evidence is its size and its tests. Scala 3 capture checking is experimental and not relied on.
- **Reflection by string**: a class loaded by a name the scanner cannot see is exactly why reflection and class loading are themselves forbidden in the business layer; the scanner does not follow them.
- **Hermetic replay in CI** (no network, read-only filesystem; the replay must be byte-identical): okay-watch's job, in its own specs.

## Design

- Zero dependencies, JVM only (the scanner reads jars and directories;
  a Scala.js build has no class files to audit). `okay-codec` for the JSON
  report — in-house, already a dependency of everything that reports.
- The class-file reader is the one `src/test/scala/TestInlineBudget.scala`
  already carries (JVMS §4, one screen, `methods(cls)`), lifted into
  `okay.audit.Scan` and extended from "method code lengths" to "the constant
  pool's references plus each method's access flags". Not
  `java.lang.classfile`: dotty 3.9 cannot load its sealed model types
  (resume-inline-budget-guard, specs/core-gaps.md stage 4).
- A reference is read from `CONSTANT_Class`, `CONSTANT_Fieldref`,
  `CONSTANT_Methodref`, `CONSTANT_InterfaceMethodref`,
  `CONSTANT_MethodHandle` and the bootstrap of every
  `CONSTANT_InvokeDynamic`; plus `ACC_NATIVE` on own methods. Descriptors
  are kept so a rule can name an overload (`Date#<init>()`).
- Matching is by prefix on the owner's binary name with `/` turned to `.`,
  and by `owner#member` for a member rule; the three bootstrap exceptions
  are the only hard-coded carve-out, and they are listed in the report's
  header so a reader sees them.
- The constant pool OVER-approximates: a reference in dead code still counts.
  That is the right direction for a boundary — the check is sound for every
  direct reference and says so; what it cannot see (reflection, class
  loading, native code) it forbids instead.

## Decisions

- **Bytecode, not a Scala compiler plugin or Scalafix rule** — covers Java,
  Kotlin and Clojure callers of the facade and third-party jars; a plugin
  sees only Scala sources of the current build.
- **Own scanner, not ArchUnit** — ArchUnit is a dependency with its own
  classpath model and no inventory output; the rule set here is a value and
  the inventory is half the point. Not `jdeps` — package-level only; it
  cannot name `System#currentTimeMillis` or fail on a member.
- **Three layers, not two** — okay's own core spawns threads and opens
  sockets (Async, the platform); calling it "handlers" would hide it in the
  inventory, calling it "business" would fail it. `Runtime` names it for
  what it is: the trusted base below the handlers.
- **Fail only business** — handlers are supposed to touch the world; their
  job here is to be LISTED. A finding is a reach from where none should be.
- **Allows need an owner and a reason** — the Ack pattern of
  specs/security.md: anything weaker than the default is a named decision.
- **Carve-outs are listed in the report's header, every run** — the
  bootstraps, the bootstrap types, the lazy-val idiom. A reader who
  distrusts the check sees exactly what it chose not to count.

## Stages

1. `okay-audit` module: `Scan`, `Boundary.Default`, `Audit.run`, text report,
   sbt `auditLayer`/`audit`, dogfood on okay's own build. Tests: a fixture
   module of hand-written classes for each behavior item above.
2. JSON report with input hashes, allows, unused allows, inventory by
   provider; the CLI jar for Maven/Gradle users of the Java facade.
3. Launcher check: read a deployment's JVM flags and module descriptors,
   record `--illegal-native-access`, `--add-opens`, `--enable-native-access`
   into the report as configuration evidence.
4. (okay-watch) hermetic replay job in `dagger/okay-watch.dsh`: replay a
   recorded case with no network and a read-only filesystem; byte-identical
   or the job fails.

## JPMS — what the JVM's module system adds, and what it cannot (decided 2026-10-05)

The question: can JDK 9+ modules enforce the boundary this spec checks by
scanning? Partly, and the part it enforces is worth taking; the part it
cannot is why the scanner stays.

**What JPMS enforces at run time, per module, with no scanner:**

| Reach | JPMS mechanism | Covers rule |
|---|---|---|
| `java.sql`, `javax.sql` | not readable without `requires java.sql` | databases |
| `javax.naming`, `java.rmi`, `java.net.http`, `java.util.logging`, `java.management`, `java.scripting`, `java.desktop` | each its own module; unreadable unless required | network (part), console (logging) |
| `sun.misc.Unsafe` | lives in `jdk.unsupported`; unreadable unless required | escape hatch |
| `java.lang.foreign`, JNI `System.load*` | restricted methods (JEP 472): denied unless `--enable-native-access=<module>`; `--illegal-native-access=deny` turns the warning into a refusal | foreign, native |
| deep reflection into the JDK and into other modules | strong encapsulation: fails without `--add-opens` / `opens` | reflection (part) |

**What it cannot enforce, ever:** everything in `java.base` — `java.net`
(sockets), `java.nio.file`, `java.io.File*`, `Thread`, `System.currentTimeMillis`,
`Random`, `Class.forName`, `MethodHandles`, `ClassLoader`. Every module
reads `java.base`. So the business rules that matter most for replay (time,
randomness, files, sockets) are scanner-only; JPMS adds a second,
JVM-enforced witness for the rest, and the launcher flags for native code.

**What JPMS adds beyond enforcement — the inventory from the live JVM:**
`ModuleLayer.boot().modules()` and each `ModuleDescriptor.requires()` name
what is actually loaded, at run time, not at build. okay-watch already
derives its shipped runtime this way (`jdeps --print-module-deps` → `jlink`,
okay-watch specs/deploy.md): a jlinked image that carries no `java.sql`
module cannot run SQL, whatever the jar contains. That image's module list
is inventory evidence too.

**The obstacle in okay itself: split packages.** okay's modules share the
package `okay` (`okay.Optic` in okay-optics, `okay.Hlc` in okay-data, the
core's `okay.*`), and JPMS forbids one package in two modules — named OR
automatic, on the module path. So okay's modules cannot become named
modules without one package per module, and cannot even sit on the module
path as separate automatic modules. As ONE shaded jar (okay-watch's assembly,
module-info discarded) okay is one automatic module and the rule holds.

**Decision — three stages, none blocking the other:**

- **A. okay-audit reads descriptors (stage 3, cheap, no packaging change).**
  `Scan` reads `module-info.class` where present (`ModuleDescriptor.read`),
  and the report says per rule whether it is *JVM-enforced* for that module
  (descriptor present and the module not required) or *scan-only*; detects
  split packages across inputs ("not JPMS-ready: package `okay` in okay-optics
  and okay-data"); records the launcher flags (`--illegal-native-access`,
  `--add-opens`, `--add-exports`, `--enable-native-access`) from a jvm-options
  file; and gains `Audit.runtime()` — a self-check an app calls at startup
  that writes the boot layer's modules, their requires, native-access grants
  and the input arguments into its evidence journal (okay-watch first).
- **B. The deployable as a named module (okay-watch).** okay as one automatic
  module (the assembly), the app's own code (`okaywatch.*`) as a named module
  with an explicit `requires` list, jlinked without `java.sql`/`jdk.unsupported`
  unless a handler needs them, launched with `--illegal-native-access=deny`
  and no `--add-opens`. The JVM then enforces the first table for the app's
  business code; the scanner covers `java.base`. The two reports must agree.
- **C. One package per module, `module-info` everywhere — okay2, not okay.**
  The rename is the price and okay2 is where it is affordable; then
  `uses`/`provides` can state handlers as services ("this module provides
  the Db handler"), and the descriptor itself becomes the effect/handler map.
  Spark (okay-spark) stays out: its own JPMS story is unfinished upstream
  (the `--add-opens` list in build.sbt is Spark's, not ours).

Not chosen: `SecurityManager` (removed in JDK 24); a custom
`ModuleLayer` for business code that excludes `java.sql` etc. — real, but
stage B gives the same with jlink and no runtime machinery of ours.

## Results

Stage 1 (2026-10-05, this lane). `okay-audit`: `Scan`, `Boundary`, `Audit`,
`Report`, `Main`; sbt `auditLayer` per project, root `audit`; `TestAudit`,
12 tests on Java and Scala fixtures.

**Dogfood, 24 JVM projects + the Scala library as one `runtime` row, 53
scanned inputs (class directories and third-party jars), 21 s warm for the
whole task including sbt.** The first run FAILED with 232 findings, and
all of them were right:

- **The lazy-val idiom.** Every Scala 3 module with a `lazy val` reached
  `MethodHandles#lookup`, `Lookup#findVarHandle`, `VarHandle#compareAndSet`
  (okay-optics, okay-lex, okay-crdt: exactly these 4 references each).
  A VarHandle reaches memory, not the world, and `lookup()` alone reaches
  nothing — carved out (`Boundary.LazyValIdiom`); `Lookup#findStatic`,
  `#findVirtual`, `#unreflect*`, `#defineClass` stay findings and the
  `Reflect` fixture calls `findStatic` to prove it.
- **Bootstrap types as bare classes.** javac writes `MethodHandles`,
  `MethodHandles$Lookup`, `MethodType`, `CallSite` into the pool as bare
  `CONSTANT_Class` entries (InnerClasses) for every lambda; a bare class
  reference to one of `Boundary.BootstrapTypes` is part of the carve-out, a
  MEMBER reference is not.
- **Native methods needed their own rule**: an own `ACC_NATIVE` method has
  the class itself as owner, so no API rule matched it — `Boundary.Native`.
- **Four modules were not business.** okay-codec (files, processes, TLS,
  SecureRandom, `sys.env`), okay-bayes (`scala.util.Random` in 50 places,
  a shutdown hook, `Thread`), okay-java (`Files.lines`, `reflect.Array`),
  okay-data (`Hlc` reads `System.currentTimeMillis`, `Uid` reads
  `Random.nextLong`). All four moved to `handlers`; okay-data's two are
  genuine design findings — a clock and a random source in a data module —
  filed in backlog.d/okay-data.
- Business after the first run: okay-optics, okay-parse, okay-lex,
  okay-crdt — PASS. Untracked: none of the 24.
- **The inventory shows what nothing else in the build does**: okay-kafka's
  jars reach JNI (`LZ4JNI`, `XXHashJNI`, snappy's `BitShuffleNative`) and
  `sun.misc.Unsafe`; okay-jetty reaches `javax.naming`, `java.sql.DriverManager`
  and three randoms; the Scala library itself reaches `Class.forName`,
  `ClassLoader`, `URL#openStream`. That is the Art. 8 inventory an auditor
  asks for, from one command.

The self-report is committed as `okay-audit/dogfood/report.txt` (local paths
relativised; `maven2:` for the Coursier cache) so it can be read and linked
without a build; `sbt audit` regenerates it.

Deviation from the Interface above: `Report.json` is written by a
twenty-line writer of its own, not okay-codec's `Json` — the module stays
at zero dependencies so a Maven/Gradle user of the Java facade (stage 2)
can run it alone.
