# DI: modules, wiring and the containers next door

## Overview

The operator's ask (2026-09-09): our own dependency injection, "not
worse and even better than every competitor", declarative where that
follows, and the same code working unchanged inside whatever the
neighbours were written for — Spring Boot, ZIO, Guice, whatever else
there is. The architecture ours, not theirs.

Most of it is already standing (specs/context-functions.md,
docs/capabilities.md): a dependency is a TYPE in the signature
(`Db ?=> X`, a `using` parameter, `wire[Db]`), a missing one is a
compile error with the requirement in the message, `provide` installs
for an expression, `providing`/`and` compose environments as values
with no arity cap and nearest-wins overriding, and a test double is
one more `and`. Spring finds a missing bean at start-up, Guice at
injector creation, ZLayer by its own runtime's types; here the
compiler finds it, and there is no container to start.

What was NOT standing, and is the gap between "context functions" and
DI: an installer holds a READY value. Nothing opened a pool, ordered
start and stop, or let one module's construction see the module
before it. Spring's `@Bean` methods take other beans as parameters
and the container closes them in reverse; `ZLayer` is a resource-
scoped constructor whose `>>>` feeds one layer's output to the next.
Stage 0 adds exactly that piece, on the two primitives the core
already had — `Providing` and the `Resource` region — and nothing
else.

Everything after stage 0 is the operator's list in the order it can
be built: qualifiers, the plan as a printable value, the bridges,
and the join with specs/deployment.md, where an application's
"needs" are literally the unresolved inputs of its root module.

## The model (stage 0, SHIPPED)

One new noun:

- **Module** — `Module[F]` wraps `Providing[F] ! Resource`: an
  installer that has not been built yet. `module[A](acquire)(release)`
  is one capability acquired in the scope; `Module.value(a)` and
  `Module.ready(providing)` are modules with nothing to build; the
  scope that releases them is `Resource.run`, in reverse order,
  whatever the program did (specs/delimited-control.md's region).

Two operations, both `Providing`'s, with one difference:

- `m and that` — `that` is typed `F[Module[G]]`: it is built INSIDE
  the left's context. A plain module coerces (a value is a context
  function that ignores its argument); a module whose acquisition
  needs an earlier one is written `Db ?=> Module[…]` and reads it
  with `wire[Db]`. That is the dependency graph: it is the
  composition itself, the compiler checks it, and an acquisition
  naming a capability no module before it installs does not compile.
  Left acquires first, so the right (inner) releases first. The
  right is the inner layer and wins under nearest-wins, so a test
  double is `base and Module.value[Log](fake)` — `Providing.and`'s
  order and override story, unchanged.
- `m { body }` — install everything and run the body inside the
  scope; the result is `B ! Resource`, handed to `Resource.run`.

```scala
val db   = module[Db](Db.open(url))(_.close)
val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
  module[Pool](Pool.over(wire[Db]))(_.close)
Resource.run((db and pool) { wire[Pool].borrow() })
```

The rule docs/capabilities.md already states, kept and sharpened: a
module ACQUIRES a resource (the pool, the server) and INSTALLS an
environment (`Pool`, `Http`). What it installs is what one scope
shares safely; the per-call resource — a connection borrowed from the
pool — stays an explicit argument, as before.

## Behavior

Stage 0 (core, `Providing.scala`, TestModule):
- [x] modules acquire left to right and release in reverse at the end
      of the scope, through `Resource.run`
- [x] a module's acquisition reads the module before it (`wire[Db]`
      inside `module[Pool](…)`) — the graph is the composition
- [x] a test double overrides by composing to the right, and builds
      nothing
- [x] a dependency no module installs is a COMPILE error naming the
      type (`compileErrors`, the message quoted)
- [x] a failing acquisition releases what was acquired before it
- [x] the vocabulary runs on EVERY platform, not only where it was
      written (di-cross): `TestModuleCross` in `src/test/scala-cross`
      covers acquisition order and reverse release, a dependent
      module, the override, `plan` off the type, `exports` with the
      erased class and `Resource.open`'s closer — JVM, JS and Native.
      It found nothing: the code was already right, the guard was
      missing. `Module` is shared core, and both non-JVM platforms
      replace the test sources with `scala-cross` alone, so the JVM
      suite could never have said this

Stage 1 — qualifiers and the plan as a value (SHIPPED, di-stage1):
- [x] two capabilities of one type are told apart by TYPE, never by
      string: an opaque type per role (`Primary`, `Replica`) is the
      qualifier; a `Module.value[Primary](…)` installs one and
      `wire[Replica]` cannot see it — a compile error naming the role.
      No new mechanism (TestModule, `ModuleRoles`)
- [x] `m.plan`: what the module will install, in acquisition order,
      as a `Vector[String]` — read off the module's TYPE by a macro,
      before anything is built (a plan that acquired to be printed
      would be a trace). A dependent module's contribution is its `G`
      in the type of `and`, so the plan needs no value. Names are the
      type symbols', so an opaque qualifier shows as itself
      (`Vector("Primary", "Log")`) where its erased class could not
- [x] okay-conf joins with no new API: a config is a `Module.value`,
      a `Secrets` resolver is another, and the connection module is
      `(DbConf, Secrets) ?=> Module[…]` resolving the `Secret` inside
      its acquisition — the value exists only between `Secrets.get`
      and the constructor argument; a miss fails the acquisition
      naming the REFERENCE (okay-conf TestConfModule)

Stage 2 — the bridges (each its own satellite; instances inward,
values outward, the P3 rule from specs/interop.md):
- [x] `okay-spring` (SHIPPED): `OkaySpring.register(ctx, m.exports)` —
      one singleton per installed capability, named by the plan, typed
      by the erased class, a `DisposableBean` closing the scope with
      the context (reverse order, tested); `OkaySpring.bean[A](ctx)`, a
      bean as a module looked up when the scope builds; the
      `ReactiveAdapterRegistry` adapter so a controller returns
      `A ! Async`, registered by `OkayAutoConfiguration` (Boot's
      `AutoConfiguration.imports`, tested under
      `ApplicationContextRunner`). Two core additions carried it:
      `Resource.open` (acquire now, closer later — the scope whose end
      belongs to somebody else) and `m.exports`, `plan`'s macro twin
      that generates the body collecting each ambient value. The
      `@Configuration` the entry first named became `register` on a
      `GenericApplicationContext`: a configuration CLASS is Spring's
      way of saying a value, and we have the value
- [x] `okay-zio` gains `ZioLayers` (SHIPPED): `toLayer` (a module
      under ZIO's `acquireRelease`), `fromLayer` (a layer built in a
      `Scope` the module closes), `fromEnvironment` (a `Providing`).
      One capability per conversion — their environment is typed by
      Tags per member, ours by the chain, and each composes in its own
      words (`++`, `and`)
- [x] `okay-guice` (SHIPPED): `OkayGuice.bindings(m.exports)` binds by
      NAME from the plan and by type where the erased class is unique
      among the exports (two opaque roles over one class: two names,
      no type binding, no duplicate); the closer is a bound
      `ModuleScope` instance since Guice has no lifecycle;
      `OkayGuice.instance[A](injector)` asks by type when the scope
      builds
- [x] `okay-cdi` (SHIPPED, di-tails): `OkayCdi.extension(m.exports)`, a
      portable Extension adding one synthetic `@Singleton` `@Named`
      bean per export at `AfterBeanDiscovery` and running the closer
      at `BeforeShutdown` (a singleton nobody selected is never
      destroyed, so the bean's destroy is not the hook); `instance[A]`.
      Weld SE in tests
- [x] the WebFlux end-to-end (di-tails): through the handler stack in
      the default gate, and Boot + Netty on a random port under `Live`.
      It found the adapter alone insufficient — see Decisions

Stage 3 — the join with deployment (specs/deployment.md), SHIPPED:
- [x] the root module's unresolved inputs ARE the application's
      declared needs: `Needs.of[Root]` (okay-deploy) walks
      `Pg ?=> Files ?=> Module[…]` at compile time and summons one
      `given Needs[A]` per input — the capability's own declaration
      of what it is to a deployment (`Need.Database`, `Need.Volume`)
      — into the `Vector[Need]` a `Service` carries; tupled inputs
      and a root with nothing left both read; an input without a
      `Needs` is a compile error naming it. What only the PLACE can
      say (`Need.Port`, `Need.Dns`, `Need.Tls`) stays beside them in
      the `Service`: the type says what the code needs, not where it
      runs

## Interop: rendering, not emulation

The principle is specs/deployment.md's, applied to wiring: **a
declarative layer over what already works in each place, orchestrating
the CONTAINERS and never becoming one.** A `Module` is a value; a
Spring configuration, a ZLayer, a Guice module are RENDERINGS of it,
and a foreign container is a SOURCE of values for a `Providing`. So
one module written here runs under Spring Boot by rendering, under
ZIO by conversion, standalone by `Resource.run`, and the code that
uses `wire[Db]` does not know which.

CDI is built too (okay-cdi, on the operator's go): a synthetic bean
per export, the closer at shutdown, `select` as the source of values.
The four bridges share the one seam, `m.exports` plus `Resource.open`,
and none of them touched the core — Micronaut's own container and
Quarkus (CDI) are covered by okay-cdi's shape.

What is deliberately NOT emulated, so the reader is not surprised:
AOP proxies and `@Transactional` (the region and `Typed.transact` are
the answer here, and a Spring-managed transaction is reachable from
a bean, not from our code), classpath scanning and annotation-driven
injection INTO our classes (ours take their dependencies as
constructor parameters and `using` clauses; that is the condition of
portability, not a loss), hot reload, and Spring's actuator (okay-ops
is the actuator, and a Boot app can mount both).

## Components declare their own needs (module-facts, 2026-09-09)

The operator's question after needs-runtime — can a provider declare
itself, without a method per kind of dependency? — exposed the hole
in stage 3 as first built: it read only a root's UNRESOLVED inputs,
and a module that opens its infrastructure itself was invisible. The
demo opened `okay-board.log` and its manifest declared a port and no
volume. The component that opens the thing is the one that knows what
it needs; it had nowhere to say so.

**The model.** A module carries FACTS beside its installer. A fact is
a typed key with its own merge (`Fact[V]`, held in `TMap`, the core's
honest runtime-keyed map); the core knows no deployment word.
okay-deploy defines one kind, `Needs.Declared`, and the spelling is
the `Need` constructors themselves at the point of opening:

```scala
moduleAs[Store, FileStore](FileStore.open(file))(_.close())
  .needs(Need.Volume(dir))
```

`and` merges facts left to right; `Needs.declared(app)` reads them
off the composed value. No wrapper, no factory per kind; `Needs[A]`
stays only for an input a place hands in from outside.

**When they can be read: after config, before infrastructure.** A
dependent module's facts hide behind its function until its input
exists — the wall `plan` met, and why `plan` reads the type. So a
module now knows whether it is READY: `Module.value`/`ready` are,
acquired ones are not. When the left of `and` is ready, the right is
applied at once and its facts and readiness carried up; behind an
acquisition they wait for the scope. The config is a value, the
module that opens a log at a path from that config declares the
volume, and the deployment reads it with nothing opened — which is
exactly when a manifest is written. Modules downstream of an
acquisition seldom need anything from a place, and the limit is
stated in a test rather than hidden.

**What it found, first use.** The demo's settings shipped `chatLog`
— `OKAY_CHAT_LOG`, the two-node log DIR — set to `:memory:`, while
the store reads `OKAY_CHAT_DB`: the container wrote its board to an
unmounted file believing it ran in memory. `ChatConf` is now one
value read the same way by `main` (the environment) and by the
deployment (its own settings), the key is `chatDb`, and the volume
the store declares reaches every rendered target — a PVC in the
chart, a volume in compose, `ReadWritePaths` and an `install -d` in
the unit — from one line where the file is opened.

## What using it taught (di-dogfood, 2026-09-09)

The arc was complete and had never built an application: outside the
core's own tests, the only user of `Module` was okay-deploy's test of
`Needs`. So okay-demo's `ChatDemo` — four capabilities, one of them a
log on disk — was rewired by it. The app boots and prints its own
plan (`chat: modules Store, Board, Transport, Secrets`). Three things
the tests could not have told us:

- **A capability is installed under one type and released under
  another.** The store is a `FileStore` to open and close, and a
  `Store` to everything that reads it — `Store` has no `close` and
  should not grow one for this. `module[A]` forces both to be `A`,
  which leaves an application choosing between over-specifying every
  consumer and a type test in the release. `moduleAs[A, R <: A]` is
  the missing spelling and is now in the core.
- **An application's body is itself a program in the scope.** A
  server is `Server ! Resource`, not a value, so `m { body }` answers
  a program inside a program and the discarded-value lint fires at
  the call site. `Module.use` flattens it once, where it belongs.
- **A global `lazy val` is exactly what a module replaces, and every
  reader of it becomes a door.** `routes` built its ops surface from
  the global store, so a module's store beside it would have opened
  one log twice — the failure the code's own comment describes.
  `routes` takes `Store` as a capability now. That is the cost of the
  conversion, and it is the whole point: the graph moves from a
  global into the type.

Two things improved by construction rather than by intent: the log is
CLOSED when the region ends (the `lazy val` it replaced never was),
and the demo's tests no longer reach the repository's real
`okay-board.log` through that global when they touch an ops route.

Stage 3 on this app was declined at first and then FIXED, because the
reason was a gap rather than a mismatch (needs-runtime). The demo's
root asks for a `Timer`, which no place provides, and `Needs.of`
treated every unresolved input as the place's business — so the only
answers were a lie (declaring a `Need` for a timer) or dropping the
guarantee that an undeclared input stops the build. A root's inputs
are MIXED, and the declaration now says which kind each is:
`Needs(Need.Database(…))` for the place, `Needs.runtime` for what the
process brings. `Timer` and `Scheduler` are declared runtime in
`Needs`'s own companion, once, for every application. The undeclared
input is still a compile error, and its message now offers both
answers.

What that leaves for the demo is a true statement rather than a
missing feature: `ChatDemo.Root` is named in the app, and
`Needs.of[ChatDemo.Root]` is EMPTY — it provisions its own store,
transport and secrets, and its one remaining input is the runtime's.
okay-demo's deployment test pins that, so the day the root gains a
database it did not provision, the build stops until someone says
what that is.

## Lifetimes (di-prototype, 2026-09-10)

The arc had one lifetime: a `module` installs one value for a region.
An instance per consumer needed a named trait per capability, and the
pure and the releasing shapes had DIFFERENT types — so a provider
that began closing what it made broke every consumer.

`New[A]` is the one type, `fresh[A]` the one consumer word, and
`prototype` comes in the two spellings that mirror
`Module.value`/`module`. `New[A].apply()` answers `A ! Resource`
ALWAYS, including where nothing is released: uniformity at the call
site is worth a program wrapper, because the alternative is that
every consumer learns whether its instance is closed. Pinned by a
test that runs ONE consumer against both providers.

The instance is released by the region its `fresh` runs in, which
makes the caller the one who chooses the lifetime — a per-request
region (`Resource.scoped`, the region as an expression, since `run`
forwards a row and a per-call region has nothing to forward) or the
application's. Inside a long-lived region every `fresh` accumulates
until it ends; stated in the docs and in a test rather than left to
be discovered.

Also here: `plan` now keeps a capability's type ARGUMENT, so a
prototype reads as `New[Conn]` rather than `New` — the difference
between a plan and a list of type constructors.

FOLLOW-UP (fresh-says-why, 2026-09-10). `fresh[Db]` where a
`module[Db]` installed the singleton is the mistake this pair
invites, and the answer was "No given instance of type okay.New[Db]
… for parameter n of method fresh": the type, not the fix. `New`
carries an `@implicitNotFound` naming both roads now — `wire` the one
the region installed, or have the PROVIDER offer a prototype — and
`fresh` is respelled `New[A] ?=> (A ! Resource) = wire[New[A]]()`,
because that is what carries the message to the call site. MEASURED:
as a `using` parameter the compiler prints its own text and the
annotation never appears; through the context function it does. The
respelling also says in the code what was only true in the prose —
`fresh` IS `wire` at another type, one primitive underneath.

## Set-binding, and why memoisation was the wrong question (di-multibind, 2026-09-10)

The comparison table named four gaps and called two of them small.

**Set-binding is built**, on the machinery that already merges: each
module declares its piece as a `Fact`, and `installing(k)` merges
every piece by that kind's rule and installs the result as a
capability. Building it moved facts through the BUILD as well as the
value — a contribution declared below an acquisition is not known
until that acquisition has happened, and losing it would have made
the feature a half-truth. `Module.built` carries `(Providing, Facts)`
now and `build` is its first half, so nothing outside changed. The
early `facts` preview still stops at the first acquisition, and that
is right: a deployment reads it before anything opens.

**Memoisation is not built, and the reason is the shape rather than
the effort.** ZLayer memoises because a layer EMBEDS its
dependencies, so a diamond builds the shared one twice. Here a shared
dependency is an INPUT — `Db ?=> Module[…]` — so the diamond does not
arise: the application installs `Db` once and every reader sees that
one. What can still bite is installing one capability twice, where
the second wins and the first is acquired for nothing. That gets
`m.shadowed`, read off the plan with nothing built. It is a REPORT
and not an error because a test double is exactly a deliberate
double, and refusing it would break the override idiom the arc has
had since stage 0.

## A fact's merge is a Monoid (fact-is-monoid, 2026-09-10)

`Fact[V]` declared `empty` and `merge` — which is `Monoid[V]`, the
one this core has had in Fold.scala all along, with instances for
Vector, List, String and every Alternative, and `Group[N]` for
numbers. Two names for one thing is what this repository forbids, and
the cost fell on every contributor: two methods written by hand where
the instance already existed.

A `Fact[V]` now CARRIES its monoid, so the usual kind is one line and
no methods (`object Routes extends Fact[Vector[Route]]`), and a rule
the givens do not have is passed in — `Monoid.of(zero)(f)`, added for
exactly this. okay-deploy's `Declared` is one of those: two modules on
one volume declare one volume, so its merge dedups rather than
appends.

The operator's question was whether the collection should be
abstracted (Foldable was the suggestion). The right abstraction is not
folding a container but COMBINING two contributions, which is the
monoid; with it, a collection stops being special — a fact over
`String` concatenates and one over `Int` sums, pinned by a test.

## Decisions

- **`Module` is a class wrapping the program, not an alias over it.**
  The first cut was `type Module[F] = Providing[F] ! Resource` with
  the combinators as extensions on `Providing`'s companion. It
  compiled, and `m { wire[Db].q }` did not: the body of an EXTENSION
  `apply` was typed without its expected type and eagerly applied —
  the E10 trap of specs/context-functions.md, met from a new side —
  while `Providing.apply(m) { … }` spelled out worked. A class method
  types the same body as `Providing`'s does. Kept in the class's doc.
- **The companion factory is `Module.ready`, not `Module.apply`.**
  An `apply` overload on the companion shadows the constructor call
  and every internal `Module(prog)` becomes `new Module(prog)`;
  naming the lift for what it is reads better and costs nothing.
- **Dependency between modules is the right operand's TYPE**,
  `F[Module[G]]`, not a separate `flatMap`/`>>>`. It reuses the one
  fact the whole design rests on (a context function auto-applies
  where its given is in scope), so a dependent module and an
  independent one compose with the same word, and inference finds `G`
  through the type lambda as `Providing.and` already does.
- **Stage 0 adds no plan, no names, no tags.** Ordering and release
  are the region's; names belong to stage 1.
- **A container holds the scope open; `Resource.open` hands it the
  closer** (stage 2). `Resource.run` owns the end of its scope; a
  Spring context, a Guice injector, a `main` with a shutdown hook
  own theirs. `open` acquires now and returns the value with an
  idempotent closer, releasing in reverse on a failed acquisition as
  `run` does. Resource-only by signature: there is no home to forward
  a row to.
- **`exports` generates the collecting body; nothing reflects on the
  values.** The alternative — recording values as `module` builds
  them — would have put a name and an `Any` into every module for the
  sake of one bridge. The macro reads the same chain `plan` does and
  writes `(a: A) ?=> (b: B) ?=> Vector(Installed(…, a), …)`, so the
  values reach the container through the givens they were installed
  as. The bridge meets Spring's `Object`-typed API in two places,
  both restating a check already made (`Class.cast`, the registry's
  class test).
- **Needs are a given per capability, not a field on `module`**
  (stage 3) — REFINED by module-facts: the given stays for an input
  the place hands in; a component that opens its own infrastructure
  declares its need as a FACT on the module, typed by a key the
  reader defines, so no deployment word enters the core and the
  declaration sits where the thing is opened. The first sketch had the deployment fact ride on the
  module (`module[Pg](…).needs(Need.Database(…))`), which puts a
  deployment word into every module that touches a database and
  into the core. A `Needs[A]` given lives where the capability is
  defined, once, and the macro summons it only for the inputs a
  root still has — a module that RESOLVES its `Pg` internally
  contributes nothing, which is right: the deployment provisions
  what the code cannot make.
- **WebFlux needs a result handler, not only an adapter** (di-tails).
  Two measured facts: `WebFluxConfigurationSupport` makes a
  `ReactiveAdapterRegistry` BEAN and never consults the shared
  instance (a program registered only there reached Jackson: "No
  serializer found for class okay.Free$Bind"), and WebFlux reads the
  element type from generic index 0, which for `Free[Async, A]` is
  the effect (a `String ! Async` came back as `data:hi!`, a
  server-sent event). So the auto-configuration registers a
  `BeanPostProcessor` for every registry bean and an
  `OkayResultHandler` ordered before `ResponseBodyResultHandler` that
  hands the value on as a `Mono` under a stand-in return type whose
  index 0 is our index 1. The alternative — a wrapper type with `A`
  first — would have changed the controller's signature, which was
  the one thing the ask ruled out.
- **The plan is the type, not a record of the build** (stage 1). The
  entry first said "via the TypeableK seam"; that seam names an
  effect signature by its runtime class, which is the wrong tool
  twice over: it cannot see a dependent module's value without its
  dependency, and it erases an opaque qualifier to its underlying
  class. The module's `F` already carries the answer — the curried
  chain `A ?=> B ?=> … ?=> X`, outer to inner in acquisition order —
  so `plan` is a macro walking `F[Marker]` to the marker. It costs
  nothing at runtime and names the qualifier as written.

## Out of scope

- a runtime container of our own, reflection, annotations, classpath
  scanning; cyclic dependencies (a value graph cannot express one,
  and that is the feature, not the limit); emulating any foreign
  runtime beyond the bridges above
