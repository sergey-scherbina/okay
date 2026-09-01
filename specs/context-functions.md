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

## The experimental base (2026-09-01, scratch experiments E1-E8)

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
- **Macros cannot rewrite a block into nested implicit scopes** —
  Scala 3 macros run after typing; implicit resolution has already
  happened. A pre-typer compiler plugin could; that road is noted,
  not taken.

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

## Filed (BACKLOG slugs, each with its gate)

- **ctx-blocking** — `type Blocking[A] = CanBlock ?=> A`: the
  first-class "this parks a thread" marker; a returned Blocking[A]
  is storable and composable, and only an edge holding the
  capability can force it. Zero runtime cost; adoption is per-seam
  and additive. Gate: none — small, waits its turn.
- **ctx-edge-docs** — the application-edge patterns documented where
  developers look (docs/typepedia + a conf.md pointer): the
  type-changing given-chain (E3) for load -> resolve -> connect ->
  migrate, and the import-thread (E6) for same-typed evolution, WITH
  its forgotten-import footgun stated. Gate: none — docs only.
- **ctx-wiring** — handlers-awaiting-environment: module factories
  returning `Http ?=> Secrets ?=> Handler[Model]`-shaped values, the
  conf doctrine ("the edge builds handlers") expressed in types;
  okay-demo adopts first. Gate: a demo consumer wanting rewiring.
- **ctx-reader-bridge** — `(A ?=> B) <-> B ! Reader % A`: a context
  function IS a pure Reader program and the tower has Reader.scala;
  the bridge is one Conversion each way. GATED: no consumer named —
  machinery for nobody until one appears.

## Rejected, with the reason

- **A ui builder DSL over context collectors** (`Column: row(...)`)
  — rejected: the tree-as-value decision (specs/ui.md, "no functions
  in the tree") exists precisely to keep construction dumb;
  a context-collector builder reintroduces ambient mutation into
  the one place the design evicted it.
- **Direct-style rewriting via macros** — rejected as impossible in
  the general case (typed trees arrive after implicit resolution);
  the honest roads are the ones the stack already walks: Loom for
  one-shot (comonadic handlers ARE direct style), for-comprehension
  or CPS-plugins for multi-shot, where multi-shot is the FEATURE
  (Logic, sim, Stepper) and direct style would forfeit it.

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
