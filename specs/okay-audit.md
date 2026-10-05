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

- [ ] a business class that constructs `java.net.Socket` is a finding naming the class, the member, the API and the rule's `why`; the build fails
- [ ] a lambda (`LambdaMetafactory` bootstrap), a string concatenation (`StringConcatFactory`) and a record's `ObjectMethods` in a business class are NOT findings
- [ ] `Class.forName`, `MethodHandles.lookup`, `ObjectInputStream` and an `ACC_NATIVE` method in a business class are findings — the escape hatches are closed by the same check
- [ ] a jar on a business project's classpath is scanned under the business rules and a finding names the jar, so a library that does I/O must move to the handler layer or be allowed by name
- [ ] a handler module's references are listed, never failed; the inventory groups them by provider (the owner's top package) and names the module
- [ ] okay core and its platform are the `Runtime` layer: listed as such, their thread and socket use is not a finding anywhere
- [ ] an `Allow` has a module, an API, a reason and an owner; one without a reason is refused by name before any scan; an allow nothing matched is reported as unused
- [ ] the report's inputs carry each jar's and directory's sha-256 so a report ties to one build; `text` and `json` are deterministic (sorted) for the same inputs
- [ ] a project without a layer is `Untracked` in the report, not failed
- [ ] dogfood: `audit` over okay's own build with `okay`/`okay-async`/platform as `Runtime`, the I/O modules (`okay-http`, `okay-sql`, `okay-persist`, `okay-r`, `okay-py`, ...) as `Handlers`, and `okay-java`'s examples plus one chosen pure module as `Business` passes, and its inventory names every provider those modules touch — the first inventory for the evidence pack
- [ ] cost: the whole okay classpath scans under the time of one small test suite (measure and record in Results)

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

## Results

(after stage 1)
