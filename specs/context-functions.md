# Context functions: what the capability arrows buy us

## Overview

Scala's `A ?=> B` makes A ambient in B — and the arrows are
FIRST-CLASS: returnable, storable, self-applying where a given is in
scope. This spec records what that buys THIS stack, on an
experimental base (below) rather than folklore, under the adoption
doctrine (specs/delimited-control.md, Adoption): everything here is
ADDITIVE — capabilities enter as extra doors, never rewrites. The
stack already stands on the capability style: `CanBlock` and
`Scheduler` are context capabilities, and `Secrets`/`Crypto`/`Store`
are the same idea as explicit traits.

## The experimental base (2026-09-01, scratch experiments E1-E14)

Every claim below was compiled, not assumed:

- **Linear rebinding of the SAME context type via `given` is
  impossible** — anonymous givens clash by name (E1/E2); named ones
  are ambiguous at use (E5). The language defines the question away:
  implicit scope is lexical, same-type rebinding in one scope is
  ambiguity.
- **Type-CHANGING given-chains are linear and work** (E3):
  `given Conn = connect(); given TxOpen = begin(); given TxDone =
  commit()` — each line sees the previous line's context. This is
  PState written in implicits. The honest hole, demonstrated: STALE
  phases stay in scope (use-after-commit compiles) — Scala has no
  linear types; capture checking may close this someday.
- **The import-thread works** (E6, on 3.7.4 AND LTS 3.3.4): a step
  returns a holder whose given member has a FIXED name;
  `import step.given` after each call threads a SAME-typed context
  linearly. Mechanism established by counterexample (E7): it is NAME
  shadowing — different member names restore ambiguity. Footgun,
  stated: a forgotten import silently uses the stale context.
- **Nested `using` parameters resolve to the NEAREST enclosing
  scope, no ambiguity** (E8) — inner shadows outer by nesting depth.
  This is what makes implicit prompts sound.
- **A stored `A ?=> B` self-applies where a given A is in scope**
  (E8) — deferred requirements as VALUES.
- **Unbounded-arity provide via the applicative idiom: the idiom
  EXISTS, the single definition is blocked** (E11/E12, 2026-09-01).
  Currying IS the n-ary Reader idiom — `(A, B) ?=> C` and
  `A ?=> B ?=> C` interchange, and a DIRECT ascription against a
  match type `Given[T, B] = T match { EmptyTuple => B; h *: t =>
  h ?=> Given[t, B] }` reduces and eta-expands a using-method into
  the curried chain (verified). But in PARAMETER position the same
  type defeats both halves: a using-method argument is not
  eta-expanded against the unreduced match type, and an ascribed
  context-function VALUE is eagerly auto-applied against it (the
  E10 trap, generalized) — explicit type arguments do not save it.
  So today: fixed-arity overloads (ContextFunctionN is built in to
  22) and nested `provide` (which associates exactly like the
  idiom's <*>) are the honest floor — and the floor SHIPPED
  (ctx-provide-n): provide is generated to all 22 arities, the Cats
  mapN answer applied (their "unbounded" is 22 generated overloads
  too; tools/gen_provide.py regenerates). Re-test the single
  definition on future compilers — param-position match-type
  reduction is the one missing piece. (E16 then answered from the
  flank: type-LAMBDA composition delivers the uncapped form —
  `providing`/`and` — without match types.)
- **Macros cannot rewrite a block into nested implicit scopes** —
  Scala 3 macros run after typing; implicit resolution has already
  happened. A pre-typer compiler plugin could; that road is noted,
  not taken.
- **The Reader monad ON context functions is definable and
  degenerate — the compiler already runs it** (E13, 2026-09-01).
  `given ctxMonad[A]: Monad[[B] =>> A ?=> B]` compiles and works
  in combinator style, and the bodies are the punchline: `map` and
  `flatMap` are both literally `f(fb)` — auto-application of the
  receiver against the ambient given IS the Reader diagonal, so
  the whole instance is the identity written four ways. Method
  syntax on a BARE ctx-fn is worse than broken: the receiver
  eagerly applies (E10), and `(x: Env ?=> String).map(f)` silently
  dispatches to `String.map` over Chars — a wrong-method trap, not
  an error. Boxing restores `.map`/`for`, but the language forbids
  the cheap boxes — `opaque type` over a context function type is
  rejected outright ("context function type cannot have opaque
  aliases"), and so is `AnyVal` — only a real allocating class
  works. Verdict: not adopted. The ambient style IS the direct
  style — `summon[Env].user` needs no `.map`; a box would reintroduce
  the monadic ceremony the feature exists to delete. When reader-y
  data flow should be a first-class effect, the row already has one:
  `Reader % R` with `Ask`.
- **The Applicative too, and it is the S combinator** (E15,
  2026-09-01): `ap[B, C](ff: A ?=> (B => C))(fb: A ?=> B): A ?=> C
  = ff(fb)` compiles and runs — BOTH sides auto-apply to the same
  ambient `A`, which is exactly `S f g x = f x (g x)`, the Reader
  ap. `map2`/`product` fall out and work (two independent
  "computations" read one environment). Same verdict as E13: the
  instance is real, the ceremony is redundant — and the mapN idiom
  the Applicative would buy already SHIPPED as `provide` at 22
  arities (E11: currying is the n-ary Reader idiom).
- **provide composes applicatively WITHOUT nesting — currying as
  value composition, and the 22 cap falls** (E16, 2026-09-01,
  SHIPPED as ctx-provide-and): an installer carries the type
  constructor `F[X] = A ?=> X`; `and` composes CONSTRUCTORS,
  `F[G[X]] = A ?=> G[X]` — the curried chain assembled by values:
  `(providing[Db](db) and providing[Log](log)) { app }`. Type
  lambdas reduce where the match-type route (E11/E12) stalled, so
  the using-method eta-expands into the chain in parameter
  position — the single-definition unbounded form EXISTS after
  all, just with `and` written by the caller instead of a tuple.
  Nesting order: the RIGHT operand is the inner layer, so it wins
  under nearest-wins — `base and providing[Log](testLog)` is the
  override story as data. Installers and their compositions are
  VALUES (build once, reuse across tests); composition is
  heterogeneous (the type grows with each `and`, no homogeneous
  fold) but uncapped — 25 layers tested past ContextFunction22.
  Core: Providing.scala (`providing[A](a)`, `and`, `apply`),
  suite TestProviding (5 tests, incl. the DI compile-error claim).
- **The consumer is one line too** (E17, 2026-09-01, SHIPPED as
  ctx-wire): `inline def wire[A]: A ?=> A = summon[A]` — Reader's
  `ask` on context functions. The naive `def wire[T] = summon[T]`
  does not compile (no given at the definition site); the `A ?=> A`
  result type fixes it, and E10's eager auto-application works FOR
  us for once: `wire[Db].q` applies to the nearest given in
  receiver position, `val d = wire[Db]` lands as a plain `Db`, and
  doors write point-free — `val getQ: Db ?=> String = wire[Db].q`.
  A missing given stays a compile error. The vocabulary closes:
  `providing`/`provide` install, `wire` consumes, the type is the
  contract.
- **`f.curried <*> wire[A] <*> wire[B] <*> wire[C] : A ?=> B ?=>
  C ?=> D` — verified, and ONE overload suffices** (E18,
  2026-09-01): the chain is the applicative of the COMPOSITION of
  distinct Readers (environments accumulate — graded, not one
  fixed `A`), and weakening is free (`wire[A]` ascribes into any
  wider chain: `val w: A ?=> B ?=> A = wire[A]`). The literal
  operator needs only `extension [B, C](fn: B => C) infix def
  <*>[E](fb: E ?=> B): E ?=> C = fn(fb)` — a PLAIN-function left
  side — because E10 eagerness collapses each intermediate ctx
  layer back to a plain function against the ambient given, so the
  same overload fires at every link. Not shipped, same verdict as
  E13/E15: juxtaposition already IS the idiom bracket — `val prog:
  A ?=> B ?=> C ?=> D = f(wire[A], wire[B], wire[C])` — the
  compiler inserts the closures and performs every `<*>` itself;
  the operator would reintroduce ceremony the elaborator performs
  for free. (The symbolic `<*>` later landed in
  `Applicative` itself — applicative-op — as `app`'s alias for ANY
  carrier; the graded ctx-specific operator stays unshipped.)
- **The E13/E15 verdict, revised where it was incomplete** (E19,
  2026-09-01, SHIPPED as ctx-monad-instance): direct style still
  needs no instance — but the GENERIC combinators written once over
  any F (traverse, sequence, replicateA) DO, and juxtaposition
  cannot replace them. Core now carries `given ctxMonad[E]:
  Monad[[X] =>> E ?=> X]` (Providing.scala): pure is the value,
  flatMap is literally `f(fa)`. `sequence(Seq[Env ?=> Int])` works
  with F INFERRED (higher-kinded unification finds the type
  lambda); traverse and replicateA take it explicitly. Method
  syntax on a bare ctx function stays rejected — the receiver
  eagerly applies before extension lookup (E10), `(x: Env ?=>
  Int).flatMap` dispatches against Int. The full matrix stayed
  green: the global given collides with nothing.
- **The ctx layer composes with `!` cleanly, and the shipped doors
  are the proof** (E14, compiled against core): `Env ?=> (A ! F)`
  gives the ambient environment INSIDE `for`-comprehensions over
  `!` with no threading; `provide(env) { !.run(...) }` peels the
  ctx layer at compile time while handlers peel the effect row at
  run time — the layers never touch. Both readers coexist without
  conflict: ctx-fn reader outside, `Reader % R`'s `Ask` inside
  (`Env ?=> String ! Reader % Int` runs). This is the same shape
  ctx-everywhere shipped as `Principal ?=> PartialFunction[Request,
  Response ! Async]` — capability outside, effects inside.

## ctx-prompts — implicit prompts for Scope and Cut (SHIPPED)

Scope and Cut thread their `Prompt` by hand. The context form makes
the prompt a capability: nesting gives "exit to the NEAREST scope"
for free (E8), and naming still crosses boundaries — Delim's
multi-prompt power is kept, it just stops being mandatory ceremony.
Deliberately named after the language's own direction
(`boundary`/`break`): these APIs are what Scala's capability style
will look like, on the machinery we already have.

```scala
object Scope:   // ADDITIVE — push/cancel/scoped stay
  /** install a scope whose prompt is ambient in the body */
  def mark[A](body: Prompt[A] ?=> A ! Row): A ! Row
  /** exit the NEAREST enclosing scope (or a named one, by binding) */
  def exit[A, R](value: R)(using p: Prompt[R]): A ! Row
  /** the one-scope form: mark + run */
  def bounded[A](body: Prompt[A] ?=> A ! Row): A ! Dialog

object Cut:     // ADDITIVE — guarded/cut/checked stay
  def guard[A](gen: Prompt[Either[Violation, A]] ?=> A ! Guarded[A])
  : Either[Violation, A] ! (Writer % String + Async)
  def violation[A, X](v: Violation)(using p: Prompt[Either[Violation, A]])
  : X ! Guarded[A]
  def watched[A](tokens: Unit ! (Writer % String + Async))
                (check: (Int, String) => Option[Violation])
                (using p: Prompt[Either[Violation, A]]): Unit ! Guarded[A]
```

- [x] `bounded { ... exit(v) ... }` exits without naming a prompt;
      two nested `mark`s: `exit` reaches the INNER (E8 semantics at
      the API)
- [x] a bound outer prompt still crosses the inner scope — the
      multi-prompt capability is kept, now opt-in
- [x] `Cut.guard { watched(tokens)(check) }` — the validator holds
      no prompt; behavior identical to the explicit `guarded` form
- [x] every existing explicit-form test passes unchanged (additive)

## obs-traced-routes — `Tracer ?=> Route` (SHIPPED)

specs/obs.md decided "the current span is handler state, not an
effect". Between an effect row and hand-threading there is a third
point — the capability: a route written against `using Tracer`
serves under `Traced.route`, which installs a PER-REQUEST tracer
rooted from the inbound traceparent. And the stored form
(`val q: Tracer ?=> Route`) is a library of already-traced route
VALUES that self-wire at the installation site (E8).

```scala
object Traced:   // okay-obs, jvm (it sees Request/Response)
  type Route = PartialFunction[Request, Response ! Async]
  def route(tracer: () => Tracer)(r: Tracer ?=> Route): Route
```

The span covers the route's ANSWER (the Response is ready), not the
body's streaming — body-level spans are the child spans the route
opens itself; stated, not hidden.

- [x] a request with a traceparent through a Traced.route: the root
      span carries the inbound trace id and the route's name; child
      spans opened via the ambient tracer parent correctly
- [x] a STORED `Tracer ?=> Route` value installs at two different
      tracers and parents to each — deferred requirement as a value
- [x] an untraced route is untouched (additive)

## ctx-principal — `Principal ?=> route` (SHIPPED)

The third capability route, and the symmetry that proves the
pattern: Secure.bearer's protected route is a
`Principal => PartialFunction[...]` — the type system already held
the door; the context form `Secure.granted(verify, policy)(route:
Principal ?=> PartialFunction[...])` makes the principal AMBIENT in
the handler, and the door gets stronger: the capability IS the
door. The crown is composition: a stored
`(Principal, Tracer) ?=> Route` is one VALUE that is protected AND
traced, self-wiring wherever both capabilities are installed —
deferred requirements composing as arrows.

- [x] Secure.granted: the handler reads the ambient principal; the
      401/403 ladder is byte-identical to bearer's (delegation, not
      reimplementation)
- [x] the composition crown: one stored (Principal, Tracer) ?=>
      Route serves under Traced.route(Secure.granted(...)(...)) —
      the response names the principal, the topic holds the root
      and the handler's child span
- [x] the explicit bearer form stays untouched (additive)

## ctx-blocking — `Blocking[A]` (SHIPPED)

`type Blocking[A] = CanBlock ?=> A`, in core beside CanBlock: the
first-class "this parks a thread" — a returned Blocking[A] is
storable, composable, and only an edge HOLDING the capability can
force it. Zero runtime; the platform gets a NAME for what its seams
already do, and new APIs can return the requirement instead of
demanding it.

- [x] a Blocking[A] value stored and passed forces only where a
      CanBlock is given; on the JVM the ambient given forces it in
      place (the alias is the existing practice, named)

## ctx-edge-docs — the patterns where developers look (SHIPPED)

The two linear-context patterns move from this spec's experimental
base into docs/typepedia (with a pointer from specs/conf.md's edge
section): the type-changing given-chain for load -> resolve ->
connect -> migrate, and the import-thread for same-typed evolution
— WITH the forgotten-import footgun stated in the same breath.

- [x] typepedia carries both patterns with their E-numbers and the
      footgun; conf.md points at it from the edge example

## ctx-everywhere — doors wherever the environment is a type (operator directive)

The operator's call, 2026-09-01: add the capability forms EVERYWHERE
— optionally. And the operator's framing, adopted as this section's
thesis: **this is the dependency-injection story** — doors (APIs
accepting capabilities) plus `provide` (the generic installer) give
DI with COMPILE-TIME resolution: a missing dependency is a type
error, not a container exception at startup; the "object graph" is
given-scopes; "modules" are ordinary values; zero reflection, zero
framework, zero dependencies.

**`provide` — the installer half of the pair:**

```scala
// core: expression-scoped installation, no given-line, no nesting
inline def provide[A, B](a: A)(inline body: A ?=> B): B = body(using a)
inline def provide[A, B, C](a: A, b: B)(inline body: (A, B) ?=> C): C
inline def provide[A, B, C, D](a: A, b: B, c: C)(inline body: (A, B, C) ?=> D): D
```

`provide(testHttp, testSecrets) { app }` swaps a whole environment
for a block — the test-override story containers sell, as one inline.

**The line that keeps "everywhere" honest — environment vs
resource:** a door is added where the parameter is an ENVIRONMENT
type (`Http`, `Secrets`, `Crypto`, `ChatModel`, `Store`, `Tracer`,
`Principal`, `Prompt`) — process-wide, swap-per-context, the thing
DI containers call a scope. A per-instance RESOURCE (`Connection`,
a `Resp`, a socket) stays an argument: ambient resources are how
leaks happen. And no newtypes are invented just to make string
params (apiKey, model) door-able — the door waits for typed config.

Doors added by this sweep (all additive, explicit forms stay):

- `McpAuth.granted` — closes the route-wrapper family (Traced.route,
  Secure.granted, and now the protected MCP route)
- `OAuth2.exchange/refresh/clientCredentials` and `Jwks.fetch`,
  `McpAuth.discover/connect` — `using Http` overloads: the one
  recurring environment of the security flows
- `Tls.serverSocket` — ambient `Secrets` (client already defaults)
- `Langchain4j.wired` — `ChatModel ?=> Handler[Model]`: the
  handler-awaiting-environment form, first of the wiring family
- `S3.wired` — `Http ?=> S3`
- `Configs.ambient` — `Store ?=> Configs`

- [x] provide: installs for a block, nests to the NEAREST (inner
      provide shadows outer), works in expression position
- [x] each door delegates byte-for-byte to its explicit form (one
      assertion per door, against stubs where a wire would be)
- [x] the DI claim demonstrated: one program wired twice —
      provide(prodEnv){...} and provide(testEnv){...} — with a
      missing capability a COMPILE error, quoted
- [x] the recipe lives in typepedia: two lines to add a door to any
      future API, the auto-application eagerness (E10) warned

## Filed (BACKLOG slugs, each with its gate)

- **ctx-wiring** — handlers-awaiting-environment: module factories
  returning `Http ?=> Secrets ?=> Handler[Model]`-shaped values, the
  conf doctrine ("the edge builds handlers") expressed in types;
  okay-demo adopts first. Gate: a demo consumer wanting rewiring —
  possibly OPEN since demo-chat (offered to that lane, room n244).
- **ctx-reader-bridge** — `(A ?=> B) <-> B ! Reader % A`: a context
  function IS a pure Reader program and the tower has Reader.scala.
  GATED: no consumer named. The sketch "one Conversion each way" was
  REFUTED by experiment (E10, 2026-09-01): the ctx->Reader direction
  must be a FUNCTION (`lift(cf: A ?=> B)`) — a Conversion never
  fires because the context function EAGERLY auto-applies at the
  ascription site first; and Reader->ctx as a Conversion SAM lambda
  hits an implementation restriction (an explicit `with apply` form
  compiles but inherits the same eagerness). The honest bridge is
  two small named functions, which is also why it can wait: they
  are one line each at any call site that wants them.

## Rejected, with the reason

- **A ui builder DSL over context collectors** (`Column: row(...)`)
  — rejected: the tree-as-value decision (specs/ui.md, "no functions
  in the tree") exists precisely to keep construction dumb;
  a context-collector builder reintroduces ambient mutation into
  the one place the design evicted it.
- **Direct-style rewriting via macros** — rejected at FULL
  generality, not as impossible: dotty-cps-async proves the general
  transform exists for Scala 3 typed trees, at the cost of years of
  re-typing machinery (the "typed trees arrive after implicit
  resolution" problem is real — it is what makes it expensive, not
  impossible). A SCOPED macro (linear `val`/`if`/`match`/`try`, no
  reflect under lambdas, clear error) is feasible and stays open as
  a future road. Two corrections to the earlier wording: (1) Loom is
  the road that forfeits multi-shot (one-shot runtime continuations,
  JVM-only) — macro-CPS and shift/reset both PRESERVE it, since the
  continuation is a pure closure; (2) the no-macro floor already
  exists: Filinski's monadic reflection over Cont
  (specs/monadic-reflection.md) gives direct style relative to Cont
  for any Monad[F] today, in for-comprehensions.

## Decisions

- **Nearest-by-nesting over named-by-default** — E8 makes the
  common case implicit and keeps naming for the crossing case;
  precedent: every language's `break`.
- **Capabilities stay additive** — the adoption doctrine applied to
  its third facility: context functions join PState and Delim as
  extra doors, and the explicit forms remain the floor.
- **Patterns are documented, not wrapped** — the given-chain and
  import-thread are STYLES, not APIs; wrapping them in library
  types would add names without adding safety (the footgun is in
  the language, not in our wrapper).
