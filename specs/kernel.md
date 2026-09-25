# okay-kernel: a microkernel — contracts, plugins, the plan, the start

## Overview

The operator, 2026-09-25: *«сделать в окей общие механизмы для
микроядерной архитектуры и потом ее использовать у нас … Сервис лоадер
и контракты версий тоже.»* The product it is for is okay-watch: one
engine, two builds (a desktop app and an Enterprise server), which today
are the same jar with the server's parts cut out after assembly by
`zip -d`. A microkernel makes a build what it is ASSEMBLED from.

The library already had most of the pieces and not the mechanism:

- effects and handlers — a capability written against its interface and
  run by whichever implementation is installed;
- `provide`/`providing` and `Module` (Provide.scala) — composition
  checked by the COMPILER: a missing capability does not compile;
- `Resource` — acquire in order, release in reverse, once;
- contract suites (okay-docs' `DocsSuite`, okay-blob's `BlobContract`) —
  one statement of a port's laws that every engine must pass.

What static composition cannot do is the other half: a set of parts
known only when the program STARTS — found on the classpath, dropped
into a directory by a customer, switched off by configuration — each
built separately, possibly against another version of the contract it
implements. That half is this module. It does not replace `Module`: a
program whose parts are all known at compile time should keep using
`Module`, which proves more.

## The model

Five nouns.

- **Version** — SemVer 2.0 (major.minor.patch, an optional pre-release,
  which orders BELOW its release). The unit of every contract here.
- **Range** — what a requirer accepts: `^1.2` (caret: same major, at
  least 1.2; on 0.x same minor), `=1.2.3`, `>=1.2 <2`, `*`.
- **Port[A]** — a CONTRACT: a name, the contract's version, an arity
  (`One`: exactly one provider; `Many`: any number, all used) and laws
  (named checks every implementation must pass). Identity is the name.
- **Plugin** — a part: an id, its own version, the range of kernel APIs
  it was built for, what it NEEDS (a port, a range, optional or not) and
  what it PROVIDES (a port, the contract version it was built against,
  and how to make the implementation from what it needs, as a
  `Resource`).
- **Plan** — the answer to "can these plugins run together, and in what
  order", computed without running any of them.

### The version contract, exactly

A port's version is the version of its CONTRACT (the trait `A` and its
laws), not of any implementation. It changes by SemVer: a major when an
implementation or a user written against the old one would break, a
minor when the contract grows compatibly.

A provision states the contract version it was BUILT against (`built`),
literally — a plugin compiled last year against 1.1 says 1.1 even when
the host now ships 1.4. Given the host's port version `host`:

1. `built.major == host.major`, else **Incompatible** — the contract
   changed under it.
2. `built <= host`, else **Incompatible** — built against a contract
   newer than this host knows; it may use what is not there.
3. A need's range must accept `built` — a user that needs 1.3 cannot be
   served by a provider built against 1.2 (it lacks what 1.3 added).
4. The plugin's kernel range must accept `Kernel.api`, else
   **KernelMismatch**.

Rules 1–2 are what binary compatibility on the JVM actually permits
between separately built parts; rule 3 is what the requirer said.

## Interface

```scala
package okay.kernel

final case class Version(major: Int, minor: Int, patch: Int = 0, pre: String = "")
object Version { def parse(s: String): Either[String, Version] }

enum Range { case Caret(v); case Exact(v); case Between(lo, hiExclusive); case Any
             def accepts(v: Version): Boolean }
object Range { def parse(s: String): Either[String, Range] }

enum Arity { case One, Many }
final case class Law[A](name: String, check: A => Either[String, Unit])
final class Port[A](val name: String, val version: Version, val arity: Arity, val laws: Vector[Law[A]])
object Port { def one[A](name, version, laws*); def many[A](name, version, laws*) }

final case class Need(port: Port[?], range: Range, optional: Boolean = false)
final case class Provision[A](port: Port[A], built: Version, make: Wiring => A ! Resource)

trait Plugin {
  def id: String
  def version: Version
  def kernel: Range = Range.Caret(Kernel.api)
  def needs: Vector[Need]
  def provides: Vector[Provision[?]]
}

trait Wiring { def one[A](p: Port[A]): A; def all[A](p: Port[A]): Vector[A]; def maybe[A](p: Port[A]): Option[A] }

enum Problem { DuplicateId, KernelMismatch, Missing, Incompatible, Unserved,
               Ambiguous, UnknownChoice, Cycle, LoadFailed, LawBroken }

final case class Plan(order: Vector[Plugin], serving: Map[String, Vector[(Plugin, Provision[?])]])

object Kernel {
  val api: Version
  def plan(plugins: Seq[Plugin], choose: Map[String, String] = Map.empty,
           disabled: Set[String] = Set.empty): Either[Vector[Problem], Plan]
  def start(plan: Plan, verify: Boolean = true): Running ! Resource
}
trait Running extends Wiring { def installed: Vector[Installed]; def describe: Vector[String] }

// JVM
object Discover {
  def services(loader: ClassLoader = …): (Vector[Plugin], Vector[Problem.LoadFailed])
  def jars(dir: java.nio.file.Path, parent: ClassLoader = …): (Vector[Plugin], Vector[Problem.LoadFailed])
}
```

## Behavior

- [ ] `Version.parse` reads `1`, `1.2`, `1.2.3`, `1.2.3-rc.1`; refuses
      anything else with the input named. Order: numeric by part, a
      pre-release below its release, pre-releases compared as strings.
- [ ] `Range.parse` reads `^1.2`, `=1.2.3`, `>=1.2 <2`, `*`. Caret on
      0.x is same MINOR (`^0.3` accepts 0.3.9, refuses 0.4.0).
- [ ] `plan` answers EVERY problem at once, not the first: a list an
      operator reads and fixes in one go.
- [ ] two plugins with one id: `DuplicateId`.
- [ ] a plugin built for another kernel API: `KernelMismatch`.
- [ ] a required port nobody provides: `Missing`; an optional one: fine,
      `maybe` answers None.
- [ ] a provision built against another major, or a newer minor than the
      host's port: `Incompatible` (plugin, port, built, host).
- [ ] providers exist and none is in the need's range: `Unserved`
      (with each candidate's built version).
- [ ] a `One` port with two providers: `Ambiguous`, unless `choose`
      names one (`choose(port) = pluginId`); a choice naming a plugin
      that does not provide it: `UnknownChoice`.
- [ ] plugin A needs what B provides and B needs what A provides:
      `Cycle` with the path.
- [ ] the order is topological (a provider before its users) and, where
      free, by id — the same plugins give the same order on every run.
- [ ] `disabled` removes plugins before planning; what then goes missing
      is reported as missing.
- [ ] `start` makes each provision in plan order under one `Resource`:
      released in reverse, once, also when a later one throws.
- [ ] `start(verify = true)` checks every provided value against its
      port's laws; a broken law stops the start with `LawBroken`
      (plugin, port, law, why) and releases what was made.
- [ ] a plugin's `Wiring` answers only the ports it declared in `needs`;
      reading another is an error naming both — a plugin cannot reach
      past its declaration.
- [ ] `Many` ports: `all` answers every compatible provider in plan order.
- [ ] `Discover.services` loads every `META-INF/services/okay.kernel.Plugin`
      provider; one that fails to load or construct is a `LoadFailed`
      (class, why) and the others still load.
- [ ] `Discover.jars(dir)` does the same over every `*.jar` in a
      directory, under one class loader whose parent is the host's.

## Forbidden edges (okay-deploy's sbt plugin)

A microkernel is only as small as its dependency graph lets it be, and
that graph drifted twice in one day (ops-docs-vendor-drivers,
http-mcp-agent-edge): okay-ops reached two database drivers and
okay-http reached an LLM client, and nothing said no. So the graph gets
rules, checked where it is defined.

`OkayModules.forbid(from, to, why)` — two regexes over project ids and a
reason. At load, the COMPILE-scope closure of every project matching
`from` (its `dependsOn` edges whose configuration maps compile or
runtime, followed transitively) must contain no project matching `to`;
otherwise the load fails naming the path and the reason.

- [ ] okay's own rules: okay-http reaches no okay-mcp, okay-agent,
      okay-llm, okay-rag; okay-ops reaches no okay-docs; the core reaches
      nothing; okay-kernel reaches only the core.
- [ ] a violating rule fails the load with the path
      (`okayHttpJVM -> okayMcpJVM -> okayAgentJVM`) and the why.
- [ ] a test-scope edge is not a violation.

## Module

`okay-kernel`, cross-built (JVM, JS, Native); depends on the core only.
`Discover` is JVM-only (`src/main/scala-jvm`): ServiceLoader and class
loaders are the JVM's. `OkayModules` lives in okay-deploy's sbt plugin,
beside `OkayDeploy`, so a consumer that already loads that plugin
(okay-watch) states its own rules with it.

## Out of scope

- Hot reload and unloading. A plugin set is decided at start; a
  different set is a restart. Unloading classes safely is OSGi's
  problem, and determinism is worth more than it.
- Class-loader isolation between plugins (one loader per jar). One
  loader for a plugins directory, parent-first: two plugins needing two
  versions of one library is a packaging problem this does not hide.
- Configuration of plugins. A plugin's `make` reads what it needs; a
  configuration port (`Port.one[Config]`) is the way to hand it one.

## Decisions

- **Contract version literal in the provision**, not read from the
  `Port` at run time: `port.version` evaluated inside a plugin reads the
  HOST's value, which is exactly the number that cannot tell a stale
  plugin from a current one.
- **Every problem at once.** A plan that stopped at the first missing
  port would be fixed one restart at a time.
- **Laws at start, not only in tests.** A plugin from a directory was
  never run through this repository's tests; the kernel is the last
  place its contract can be checked before it is trusted.
- **Wiring refuses undeclared ports.** A plugin that reaches past its
  `needs` makes the plan a lie: the order and the missing-check were
  computed from what it said.
